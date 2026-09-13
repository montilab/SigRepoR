#' @title addSignature
#' @description Add signature to database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param omic_signature An OmicSignature R6 object from the
#' OmicSignature package (required).
#' @param visibility Logical; whether the uploaded collection should be visible 
#' and accessible to others, Defaults to 'FALSE'
#' @param add_users A Data Frame; must contain the following column names:
#' 'user_name', 'access'. Access types are owner, viewer, or editor.This argument is only relevant when 
#' visibility is set to 'FALSE'. 
#' @param metabolomics_nomenclature Optional metabolite dictionary for
#' metabolomics signatures. One of refmet_id, refmet, hmdb, smiles, or inchikey.
#' @param return_signature_id Logical; if 'TRUE', the function will 
#' return the ID generated for the newly uploaded signature. Defaults to 'FALSE'.
#' @param return_missing_features Logical; if set to 
#' 'TRUE' the function will return a list of missing features present in the OmicSignature. Defaults to 'FALSE'
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
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
#' # Add signatures to database
#' SigRepo::addSignature(
#'   conn_handler = conn_handler,
#'   omic_signature = omc_signature_1,
#'   visibility = FALSE, # default is false
#'   add_users = c("John", "Jane"),
#'   return_signature_id = TRUE,
#'   verbose = TRUE
#' )
#' 
#' }
#' 
#' 
#' @export
addSignature <- function(
    conn_handler = NULL,
    omic_signature,
    visibility = TRUE,
    add_users = NULL,
    metabolomics_nomenclature = NULL,
    return_signature_id = FALSE,
    return_missing_features = FALSE,
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

  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Get visibility ####
  visibility <- base::ifelse(visibility == TRUE, 1, 0)
    
  # If visibility is public (==1), add_users will be ignored
  if(visibility == 1){ 
    
    if(!base::is.null(add_users))
      base::warning("By setting visibility = 1, the signature will be set as public and visible to all users, thus 'add_users' will be ignored.\n")
    
    add_users <- base::data.frame(NULL)
    
  }else if (visibility == 0){
    
    if(base::is.null(add_users)){
      add_users <- base::data.frame(NULL)
    }else if(!base::is.data.frame(add_users) || !base::all(c("user_name", "access") %in% base::colnames(add_users))){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Show error message
      base::stop("<add_users> must be a data frame with the required column names: 'user_name' and 'access'.\n")
    }else{
      # Validate user_name exists in the database
      valid_users <- SigRepo::searchUser(
        conn_handler = conn_handler,
        user_name = add_users$user_name
      )
      # Get list of user names not found
      missing_users <- base::setdiff(add_users$user_name, valid_users$user_name)
      # Check for any missing users
      if(base::length(missing_users) > 0){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))
        # Show error message
        base::stop(base::sprintf("The following users do not exist in the database: %s.\n", base::paste0(missing_users, collapse = ", ")))
      }
    }
    
  }
  
  # Check if omic_signature is a valid R6 object ####
  omic_signature <- SigRepo::checkOmicSignature(
    omic_signature = omic_signature
  )

  if (omic_signature$metadata$assay_type[1] == "metabolomics") {
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

  # Create signature metadata table ####
  metadata_tbl <- SigRepo::createSignatureMetadata(
    conn_handler = conn_handler, 
    omic_signature = omic_signature,
    verbose = FALSE
  )
  
  # Reset diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Add additional variables in signature metadata table ####
  metadata_tbl <- metadata_tbl |> 
    dplyr::mutate(
      user_name = user_name,
      visibility = visibility
    )
  
  # Create a hash key to look up whether signature is already existed in the database ####
  metadata_tbl <- SigRepo::createHashKey(
    table = metadata_tbl,
    hash_var = "signature_hashkey",
    hash_columns = c("signature_name", "user_name"),
    hash_method = "md5"
  )

  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    return_var = "*", 
    filter_coln_var = "signature_hashkey",
    filter_coln_val = list("signature_hashkey" = metadata_tbl$signature_hashkey[1]),
    check_db_table = TRUE
  ) 
  
  # If the signature exists, throw an error message ####
  if(base::nrow(signature_tbl) > 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))
    
    # Show message
    SigRepo::verbose(
      base::sprintf("\tYou already uploaded a signature with the name = '%s' to the database.\n", signature_tbl$signature_name[1]),
      base::sprintf("\tID of the uploaded signature: %s\n", signature_tbl$signature_id[1])
    )
    
    # Return signature id
    if(return_signature_id == TRUE) return(signature_tbl$signature_id[1])
    
  }else{
    
    # 1. Uploading signature metadata into database
    SigRepo::verbose("Uploading signature metadata to the database...\n")
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn, 
      db_table_name = db_table_name,
      table = metadata_tbl, 
      exclude_coln_names = c("signature_id", "date_created"),
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      table = metadata_tbl,
      check_db_table = FALSE
    ) 
    
    # Look up signature id for the next step ####
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      return_var = "*", 
      filter_coln_var = "signature_hashkey",
      filter_coln_val = base::list("signature_hashkey" = metadata_tbl$signature_hashkey[1]),
      check_db_table = FALSE
    ) 
    
    # If signature has difexp, save a copy with its signature hash key ####
    # This action must be performed before a signature is imported into the database.
    # This helps to make sure data is stored properly if there are interruptions in-between.
    if(metadata_tbl$has_difexp == TRUE){
      # 1.1 Saving signature difexp into database
      SigRepo::verbose("Saving difexp to the database...\n")
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
          body = base::list(
            difexp = httr::upload_file(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")), "application/rds")
          )
        )
      # Check status code
      if(res$status_code != 200){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
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
    
    # 2. Adding user to signature access table after signature
    # was imported successfully in step (1)
    SigRepo::verbose("Adding signature owner to the signature access table of the database...\n")
    
    # If there is a error during the process, remove the signature and output the message
    base::tryCatch({
      
      SigRepo::addUserToSignature(
        conn_handler = conn_handler,
        signature_id = signature_tbl$signature_id[1],
        user_name = user_name,
        access_type = "owner",
        verbose = FALSE
      )
      
      # Add additional users to the access table
      if(base::nrow(add_users) > 0){
        SigRepo::verbose(base::sprintf("Adding additional users: %s to the signature access table of the database.\n", base::paste(add_users$user_name, collapse = ", ")))
        SigRepo::addUserToSignature(
          conn_handler = conn_handler,
          signature_id = signature_tbl$signature_id[1],
          user_name = add_users$user_name,
          access_type = add_users$access,
          verbose = FALSE
        )
      }        
      
    }, error = function(e){
      # Delete signature
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(base::as.character(e), "\n")
    }) 
    
    # Reset options
    SigRepo::print_messages(verbose = verbose)
    
    # 3. Importing signature set into database after signature
    # was imported successfully in step (1)
    SigRepo::verbose("Adding signature feature set to the database...\n")
    
    # Get the signature assay type
    assay_type <- signature_tbl$assay_type[1]
    
    # Add signature set to database based on its assay type
    if(assay_type == "transcriptomics"){
      
      # If there is a error during the process, remove the signature and output the message
      warn_tbl <- base::tryCatch({
        SigRepo::addTranscriptomicsSignatureSet(
          conn_handler = conn_handler,
          signature_id = signature_tbl$signature_id[1],
          organism_id = signature_tbl$organism_id[1],
          signature_set = omic_signature$signature,
          verbose = FALSE
        )
      }, error = function(e){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(base::as.character(e), "\n")
      }) 
      
      # Check if warning table is returned
      if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return warning table
        if(return_missing_features == TRUE){
          return(warn_tbl)
        }else{
          return(base::invisible())
        }
      }
      
    }else if(assay_type == "proteomics"){
      
      # If there is a error during the process, remove the signature and output the message
      warn_tbl <- base::tryCatch({
        SigRepo::addProteomicsSignatureSet(
          conn_handler = conn_handler,
          signature_id = signature_tbl$signature_id[1],
          organism_id = signature_tbl$organism_id[1],
          signature_set = omic_signature$signature,
          verbose = verbose
        )
      }, error = function(e){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(base::as.character(e), "\n")
      }) 
      
      # Check if warning table is returned
      if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return warning table
        if(return_missing_features == TRUE){
          return(warn_tbl)
        }else{
          return(base::invisible())
        }
      }

    }else if(assay_type == "metabolomics"){
      warn_tbl <- base::tryCatch({
        SigRepo::addMetabolomicsSignatureSet(
          conn_handler = conn_handler,
          signature_id = signature_tbl$signature_id[1],
          signature_set = omic_signature$signature,
          feature_database = metabolomics_nomenclature,
          verbose = verbose
        )
      }, error = function(e){
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        base::suppressWarnings(DBI::dbDisconnect(conn))
        base::stop(base::as.character(e), "\n")
      })

      if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        base::suppressWarnings(DBI::dbDisconnect(conn))
        if(return_missing_features == TRUE){
          return(warn_tbl)
        }else{
          return(base::invisible())
        }
      }
      
    

    }else if(assay_type == "methylomics"){
      
      SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)

    }else if(assay_type == "genetic_variants"){
      
      # If there is a error during the process, remove the signature and output the message
      warn_tbl <- base::tryCatch({
        SigRepo::addGeneticVariantsSignatureSet(
          conn_handler = conn_handler,
          signature_id = signature_tbl$signature_id[1],
          organism_id = signature_tbl$organism_id[1],
          signature_set = omic_signature$signature,
          verbose = verbose
        )
      }, error = function(e){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(e, "\n")
      }) 
      
      # Check if warning table is returned
      if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return warning table
        if(return_missing_features == TRUE){
          return(warn_tbl)
        }else{
          return(base::invisible())
        }
      }
    
      
    }
 
    # Reset options
    SigRepo::print_messages(verbose = verbose)
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))    
    
    # Return message
    SigRepo::verbose("Finished uploading.\n")
    SigRepo::verbose(base::sprintf("ID of the uploaded signature: %s\n", signature_tbl$signature_id[1]))
    
    # Return signature id
    if(return_signature_id == TRUE) return(signature_tbl$signature_id[1])

  } 
}  
