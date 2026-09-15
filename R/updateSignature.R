#' @title updateSignature
#' @description Update a signature in the database. Only the signature's owner
#' (the user who uploaded it) or an admin can update a signature; users who were
#' given editor access with addUserToSignature() cannot. If the update fails
#' part-way, the original signature is restored, including the users it is
#' shared with and the collections it belongs to.
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

    # Only the owner (the user who uploaded the signature) or an admin can update it.
    # Editor access from signature_access is not enough: add*SignatureSet() only
    # accepts the owner, so an editor's update would fail after the signature had
    # already been deleted (#214).
    if(user_role != "admin" && !base::identical(signature_tbl$user_name[1], user_name)){

      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))

      # Show message
      base::stop(base::sprintf("User = '%s' does not have the permission to update signature_id = '%s' in the SigRepo database. Only its owner or an admin can update a signature.\n", user_name, signature_id))

    }

    # Create an original omic_signature object in case updating failed ####
    orig_omic_signature <- SigRepo::getSignature(conn_handler = conn_handler, signature_id = signature_id, verbose = FALSE)[[1]]

    # Keep who the signature is shared with and which collections it belongs to.
    # deleteSignature() removes both and addSignatureWithID() only re-adds the
    # owner, so a failed update puts them back from here (#215) ####
    orig_access_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signature_access",
      return_var = "*",
      filter_coln_var = "signature_id",
      filter_coln_val = base::list("signature_id" = signature_id),
      check_db_table = TRUE
    )

    orig_collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signature_collection_access",
      return_var = "*",
      filter_coln_var = "signature_id",
      filter_coln_val = base::list("signature_id" = signature_id),
      check_db_table = TRUE
    )

    # Put the signature back to its original form ####
    # delete_first = FALSE when the signature rows have already been removed
    restore_signature <- function(delete_first = TRUE, check_difexp = TRUE){

      if(delete_first){
        SigRepo::deleteSignature(
          conn_handler = conn_handler,
          signature_id = signature_tbl$signature_id[1],
          verbose = FALSE
        )
      }

      SigRepo::addSignatureWithID(
        conn_handler = conn_handler,
        omic_signature = orig_omic_signature,
        assign_signature_id = signature_tbl$signature_id[1],
        assign_user_name = signature_tbl$user_name[1],
        visibility = signature_tbl$visibility[1],
        check_difexp = check_difexp,
        verbose = FALSE
      )

      # Re-add the access rows that are missing
      access_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_access",
        return_var = "*",
        filter_coln_var = "signature_id",
        filter_coln_val = base::list("signature_id" = signature_tbl$signature_id[1]),
        check_db_table = FALSE
      )

      missing_access_tbl <- orig_access_tbl[!orig_access_tbl$user_name %in% access_tbl$user_name, , drop = FALSE]
      
      if(base::nrow(missing_access_tbl) > 0){
        SigRepo::insert_table_sql(
          conn = conn,
          db_table_name = "signature_access",
          table = missing_access_tbl,
          check_db_table = FALSE
        )
      }

      # Re-add the collection memberships that are missing
      collection_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_collection_access",
        return_var = "*",
        filter_coln_var = "signature_id",
        filter_coln_val = base::list("signature_id" = signature_tbl$signature_id[1]),
        check_db_table = FALSE
      )

      missing_collection_tbl <- orig_collection_tbl[!orig_collection_tbl$collection_id %in% collection_tbl$collection_id, , drop = FALSE]
      
      if(base::nrow(missing_collection_tbl) > 0){
        SigRepo::insert_table_sql(
          conn = conn,
          db_table_name = "signature_collection_access",
          table = missing_collection_tbl,
          check_db_table = FALSE
        )
      }

    }

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
          restore_signature(delete_first = FALSE, check_difexp = FALSE)
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

      if(assay_type == "methylomics"){
        SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
      }

      # Add signature set to database based on assay types
      add_signature_set <- base::switch(
        assay_type,
        "transcriptomics" = function(){
          SigRepo::addTranscriptomicsSignatureSet(
            conn_handler = conn_handler,
            signature_id = metadata_tbl$signature_id[1],
            organism_id = metadata_tbl$organism_id[1],
            signature_set = omic_signature$signature,
            verbose = FALSE
          )
        },
        "proteomics" = function(){
          SigRepo::addProteomicsSignatureSet(
            conn_handler = conn_handler,
            signature_id = metadata_tbl$signature_id[1],
            organism_id = metadata_tbl$organism_id[1],
            signature_set = omic_signature$signature,
            verbose = FALSE
          )
        },
        "metabolomics" = function(){
          SigRepo::addMetabolomicsSignatureSet(
            conn_handler = conn_handler,
            signature_id = metadata_tbl$signature_id[1],
            signature_set = omic_signature$signature,
            feature_database = metabolomics_nomenclature,
            verbose = FALSE
          )
        },
        "genetic_variants" = function(){
          SigRepo::addGeneticVariantsSignatureSet(
            conn_handler = conn_handler,
            signature_id = metadata_tbl$signature_id[1],
            organism_id = metadata_tbl$organism_id[1],
            signature_set = omic_signature$signature,
            verbose = FALSE
          )
        }
      )

      if(!base::is.null(add_signature_set)){

        # If there is a error during the process, restore the signature to its origin structure and output the messages
        warn_tbl <- base::tryCatch({
          add_signature_set()
        }, error = function(e){
          # Put signature back to its original form
          restore_signature()
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn))
          # Return error message
          base::stop(base::paste0(e, "\n"))
        })

        # Check if warning table is returned
        if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
          # Put signature back to its original form
          restore_signature()
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn))
          # Return warning table
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
          # Put signature back to its original form
          restore_signature()
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
