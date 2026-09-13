#' @title updateSignature
#' @description Update a signature in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param signature_id Database ID of signature to be updated (required)
#' @param omic_signature An R6 class object from the OmicSignature package (required)
#' @param visibility A logical value indicates whether or not to allow others  
#' to view and access one's uploaded signature. Defaults to 'FALSE'.
#' @param metabolomics_nomenclature Optional metabolite dictionary for
#' metabolomics signatures. One of refmet_id, refmet, hmdb, smiles, or inchikey.
#' @param verbose Logical;  whether or not to print the diagnostic messages. 
#' Defaults to 'TRUE'.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Create a connection handler
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", 
#'   host = "sigrepo.org", 
#'   port = 3306, 
#'   user = "your_username", 
#'   password = "your_password"
#' )
#' 
#' # Update a signature in the database
#' SigRepo::updateSignature(
#'   conn_handler = conn_handler, 
#'   signature_id = 20, 
#'   omic_signature = test_omic_signature
#' )
#' 
#'}
#' 
#' @export
updateSignature <- function(
    conn_handler = NULL,
    signature_id,
    omic_signature,
    visibility = NULL,
    metabolomics_nomenclature = NULL,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]    
  
  # Get unique signature id
  signature_id <- base::unique(signature_id) 
  
  # Check signature_id
  if(!base::length(signature_id) == 1 || base::all(signature_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'signature_id' must have a length of 1 and cannot be empty.\n")
  }
  
  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = "signature_id",
    filter_coln_val = base::list("signature_id" = signature_id),
    check_db_table = TRUE
  )
  
  # If signature exists, return the signature table else throw an error message
  if(base::nrow(signature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("There is no signature_id = '%s' in the 'signatures' table of the SigRepo database.\n", signature_id))
    
  }else{
    
    # If user is not admin, check if it has access to signature
    if(user_role != "admin"){
      
      # Check if user is the one who uploaded the signature
      signature_user_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signatures",
        return_var = "*",
        filter_coln_var = c("signature_id", "user_name"), 
        filter_coln_val = base::list("signature_id" = signature_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(base::nrow(signature_user_tbl) == 0){
        
        # Get access signature table
        signature_access_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "signature_access",
          return_var = "*",
          filter_coln_var = c("signature_id", "user_name", "access_type"),
          filter_coln_val = base::list("signature_id" = signature_id, "user_name" = user_name, "access_type" = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user does not have permission, throw an error message
        if(base::nrow(signature_access_tbl) == 0){
          
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn)) 
          
          # Show message
          base::stop(base::sprintf("User = '%s' does not have the permission to update signature_id = '%s' in the SigRepo database.\n", user_name, signature_id))
          
        }
      }
      
    }
    
    # Create an original omic_signature object in case updating failed ####
    orig_omic_signature <- SigRepo::getSignature(conn_handler = conn_handler, signature_id = signature_id, verbose = FALSE)[[1]]
    
    # Reset the options message
    SigRepo::print_messages(verbose = verbose)
    
    # 1. Create metadata with new omic_signature object ####
    if (omic_signature$metadata$assay_type[1] == "metabolomics") {
      if (base::length(metabolomics_nomenclature) == 0 || base::all(metabolomics_nomenclature %in% c("", NA))) {
        metabolomics_nomenclature <- resolveMetabolomicsFeatureConfig(
          metadata = orig_omic_signature$metadata
        )$feature_database
      }

      metadata <- addMetabolomicsNomenclature(
        metadata = omic_signature$metadata,
        metabolomics_nomenclature = metabolomics_nomenclature
      )
      omic_signature <- OmicSignature::OmicSignature$new(
        metadata = metadata,
        signature = omic_signature$signature,
        difexp = omic_signature$difexp
      )
    }
    
    # Check and create signature metadata table ####
    metadata_tbl <- SigRepo::createSignatureMetadata(
      conn_handler = conn_handler,
      omic_signature = omic_signature,
      verbose = FALSE
    )
    
    # Reset the options message
    SigRepo::print_messages(verbose = verbose)
    
    # If visibility is given, update to new value ####
    if(base::length(visibility) > 0 && base::all(!visibility %in% c("", NA))){
      visibility <- base::ifelse(visibility[1] == TRUE, 1, 0)
    }else{
      visibility <- signature_tbl$visibility[1]
    }
    
    # Add additional variables in signature metadata table ####
    # Keep its original id and name of the user who owned the signature
    metadata_tbl <- metadata_tbl |> 
      dplyr::mutate(
        signature_id = signature_tbl$signature_id[1],
        user_name = signature_tbl$user_name[1],
        visibility = visibility
      )
    
    # Create a new hash key for the signature ####
    metadata_tbl <- SigRepo::createHashKey(
      table = metadata_tbl,
      hash_var = "signature_hashkey",
      hash_columns = c("signature_name", "user_name"),
      hash_method = "md5"
    )
    
    # Check table against database table ####
    metadata_tbl <- SigRepo::checkTableInput(
      conn = conn,
      db_table_name = "signatures",
      table = metadata_tbl, 
      exclude_coln_names = "date_created",
      check_db_table = FALSE
    )    
    
    # Check if the new signature hashkey exists in the database ####
    check_signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      filter_coln_var = "signature_hashkey",
      filter_coln_val = base::list("signature_hashkey" = metadata_tbl$signature_hashkey[1]),
      check_db_table = FALSE
    ) 
    
    # If the signature exists, throw an error message ####
    if(base::nrow(check_signature_tbl) > 0 && check_signature_tbl$signature_hashkey[1] != signature_tbl$signature_hashkey[1]){
      
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      
      # Show message
      base::stop(
        base::sprintf("\tCannot update signature. There is already a signature with the name = '%s' owned by '%s' in the database.\n", check_signature_tbl$signature_name[1], check_signature_tbl$user_name[1]),
        base::sprintf("\tID of the uploaded signature: %s\n", check_signature_tbl$signature_id[1])
      )
      
    }else{
      
      # 1. Delete signature from signatures table of the database ####
      SigRepo::delete_table_sql(
        conn = conn,
        db_table_name = "signatures",
        delete_coln_var = "signature_id",
        delete_coln_val = signature_tbl$signature_id[1],
        check_db_table = FALSE
      )
      
      # 2. Delete signature feature set from signature_feature_set table of the database ####
      SigRepo::delete_table_sql(
        conn = conn,
        db_table_name = "signature_feature_set",
        delete_coln_var = "signature_id",
        delete_coln_val = signature_tbl$signature_id[1],
        check_db_table = TRUE
      )
      
      # 3. If signature has difexp, remove it ####
      if(signature_tbl$has_difexp[1] == 1){
        # Get API URL
        api_url <- SigRepo::build_api_url(
          conn_handler = conn_handler,
          endpoint = "delete_difexp",
          query = base::list(
            api_key = conn_info$api_key[1],
            signature_hashkey = signature_tbl$signature_hashkey[1]
          )
        )
        # Delete difexp from database
        res <- httr::DELETE(url = api_url)
        # Check status code
        if(res$status_code != 200){
          # Put signature back to its original form
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler, 
            omic_signature = orig_omic_signature, 
            assign_signature_id = signature_tbl$signature_id[1], 
            assign_user_name = signature_tbl$user_name, 
            visibility = signature_tbl$visibility[1], 
            check_difexp = FALSE
          )
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn))
          # Show message
          SigRepo::stop_for_api_error(
            res = res,
            api_url = api_url,
            action = "delete the difexp table from the SigRepo API"
          )
        }
      }
      
      # Insert metadata into the database ####
      SigRepo::insert_table_sql(
        conn = conn,
        db_table_name = "signatures", 
        table = metadata_tbl,
        check_db_table = FALSE
      ) 
      
      # Get the signature assay type
      assay_type <- metadata_tbl$assay_type[1]
      
      # Add signature set to database based on assay types
      if(assay_type == "transcriptomics"){
        
        # If there is a error during the process, restore the signature to its origin structure and output the messages
        warn_tbl <- base::tryCatch({
          SigRepo::addTranscriptomicsSignatureSet(
            conn_handler = conn_handler,
            signature_id = metadata_tbl$signature_id[1],
            organism_id = metadata_tbl$organism_id[1],
            signature_set = omic_signature$signature,
            verbose = FALSE
          )
        }, error = function(e){
          # Delete signature
          SigRepo::deleteSignature(
            conn_handler = conn_handler, 
            signature_id = signature_tbl$signature_id[1], 
            verbose = FALSE
          )
          # Put signature back to its original form
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler, 
            omic_signature = orig_omic_signature, 
            assign_signature_id = signature_tbl$signature_id[1], 
            assign_user_name = signature_tbl$user_name, 
            visibility = signature_tbl$visibility[1]
          )
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn))  
          # Return error message
          base::stop(base::paste0(e, "\n"))
        }) 
        
        # Check if warning table is returned
        if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
          # Delete signature
          SigRepo::deleteSignature(
            conn_handler = conn_handler, 
            signature_id = signature_tbl$signature_id[1], 
            verbose = FALSE
          )
          # Put signature back to its original form
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler, 
            omic_signature = orig_omic_signature, 
            assign_signature_id = signature_tbl$signature_id[1], 
            assign_user_name = signature_tbl$user_name, 
            visibility = signature_tbl$visibility[1]
          )
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn))  
          # Return warning table
          return(warn_tbl)
        }
        
      }else if(assay_type == "proteomics"){
        
        # If there is a error during the process, restore the signature to its origin structure and output the messages
        warn_tbl <- base::tryCatch({
          SigRepo::addProteomicsSignatureSet(
            conn_handler = conn_handler,
            signature_id = metadata_tbl$signature_id[1],
            organism_id = metadata_tbl$organism_id[1],
            signature_set = omic_signature$signature,
            verbose = FALSE
          )
        }, error = function(e){
          # Delete signature
          SigRepo::deleteSignature(
            conn_handler = conn_handler, 
            signature_id = signature_tbl$signature_id[1], 
            verbose = FALSE
          )
          # Put signature back to its original form
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler, 
            omic_signature = orig_omic_signature, 
            assign_signature_id = signature_tbl$signature_id[1], 
            assign_user_name = signature_tbl$user_name, 
            visibility = signature_tbl$visibility[1]
          )
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn))  
          # Return error message
          base::stop(base::paste0(e, "\n"))
        }) 
        
        # Check if warning table is returned
        if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
          # Delete signature
          SigRepo::deleteSignature(
            conn_handler = conn_handler, 
            signature_id = signature_tbl$signature_id[1], 
            verbose = FALSE
          )
          # Put signature back to its original form
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler, 
            omic_signature = orig_omic_signature, 
            assign_signature_id = signature_tbl$signature_id[1], 
            assign_user_name = signature_tbl$user_name, 
            visibility = signature_tbl$visibility[1]
          )
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn))  
          # Return warning table
          return(warn_tbl)
        }
        
      }else if(assay_type == "metabolomics"){
        warn_tbl <- base::tryCatch({
          SigRepo::addMetabolomicsSignatureSet(
            conn_handler = conn_handler,
            signature_id = metadata_tbl$signature_id[1],
            signature_set = omic_signature$signature,
            feature_database = metabolomics_nomenclature,
            verbose = FALSE
          )
        }, error = function(e){
          SigRepo::deleteSignature(
            conn_handler = conn_handler,
            signature_id = signature_tbl$signature_id[1],
            verbose = FALSE
          )
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler,
            omic_signature = orig_omic_signature,
            assign_signature_id = signature_tbl$signature_id[1],
            assign_user_name = signature_tbl$user_name,
            visibility = signature_tbl$visibility[1]
          )
          base::suppressWarnings(DBI::dbDisconnect(conn))
          base::stop(base::paste0(e, "\n"))
        })

        if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
          SigRepo::deleteSignature(
            conn_handler = conn_handler,
            signature_id = signature_tbl$signature_id[1],
            verbose = FALSE
          )
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler,
            omic_signature = orig_omic_signature,
            assign_signature_id = signature_tbl$signature_id[1],
            assign_user_name = signature_tbl$user_name,
            visibility = signature_tbl$visibility[1]
          )
          base::suppressWarnings(DBI::dbDisconnect(conn))
          return(warn_tbl)
        }
        
      }else if(assay_type == "methylomics"){
        
        SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
        
      }else if(assay_type == "genetic_variants"){
        
        warn_tbl <- base::tryCatch({
          SigRepo::addGeneticVariantsSignatureSet(
            conn_handler = conn_handler,
            signature_id = metadata_tbl$signature_id[1],
            organism_id = metadata_tbl$organism_id[1],
            signature_set = omic_signature$signature,
            verbose = FALSE
          )
        }, error = function(e){
          SigRepo::deleteSignature(
            conn_handler = conn_handler,
            signature_id = signature_tbl$signature_id[1],
            verbose = FALSE
          )
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler,
            omic_signature = orig_omic_signature,
            assign_signature_id = signature_tbl$signature_id[1],
            assign_user_name = signature_tbl$user_name,
            visibility = signature_tbl$visibility[1]
          )
          base::suppressWarnings(DBI::dbDisconnect(conn))
          base::stop(base::paste0(e, "\n"))
        })

        if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
          SigRepo::deleteSignature(
            conn_handler = conn_handler,
            signature_id = signature_tbl$signature_id[1],
            verbose = FALSE
          )
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler,
            omic_signature = orig_omic_signature,
            assign_signature_id = signature_tbl$signature_id[1],
            assign_user_name = signature_tbl$user_name,
            visibility = signature_tbl$visibility[1]
          )
          base::suppressWarnings(DBI::dbDisconnect(conn))
          return(warn_tbl)
        }
        
      }
      
      # Reset the options message
      SigRepo::print_messages(verbose = verbose)
      
      # If signature has difexp, save a copy with its signature hash key ####
      # This action must be performed before a signature is imported into the database.
      # This helps to make sure data is properly stored to prevent any interruptions in-between.
      if(base::as.numeric(metadata_tbl$has_difexp[1]) == 1){
        # Extract difexp from omic_signature ####
        difexp <- omic_signature$difexp
        # Save difexp to local storage ####
        data_path <- base::tempdir()
        base::saveRDS(difexp, file = base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
        # Get API URL
        api_url <- SigRepo::build_api_url(
          conn_handler = conn_handler,
          endpoint = "store_difexp",
          query = base::list(
            api_key = conn_info$api_key[1],
            signature_hashkey = metadata_tbl$signature_hashkey[1]
          )
        )
        # Store difexp in database
        res <- 
          httr::POST(
            url = api_url,
            body = list(
              difexp = httr::upload_file(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")), "application/rds")
            )
          )
        # Check status code
        if(res$status_code != 200){
          # Delete signature
          SigRepo::deleteSignature(
            conn_handler = conn_handler, 
            signature_id = signature_tbl$signature_id[1], 
            verbose = FALSE
          )
          # Put signature back to its original form
          SigRepo::addSignatureWithID(
            conn_handler = conn_handler, 
            omic_signature = orig_omic_signature, 
            assign_signature_id = signature_tbl$signature_id[1], 
            assign_user_name = signature_tbl$user_name, 
            visibility = signature_tbl$visibility[1]
          )
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn))        
          # Show message
          SigRepo::stop_for_api_error(
            res = res,
            api_url = api_url,
            action = "upload the difexp table to the SigRepo API"
          )
        }else{
          # Remove files from file system 
          base::unlink(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
        }
      }
      
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))    
      
      # Return message
      SigRepo::verbose(base::sprintf("signature_id = '%s' has been updated.\n", metadata_tbl$signature_id[1]))
      
    }
  }
} 
