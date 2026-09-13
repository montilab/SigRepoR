#' @title addSignatureWithID
#' @description Add signature to database with an assigned ID
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param omic_signature An OmicSignature R6 object from the
#' OmicSignature package (required).
#' @param assign_signature_id Assign an unique ID to the uploaded signature (required)
#' @param assign_user_name Assign an unique user name to the uploaded signature (required) 
#' @param visibility Logical; whether the uploaded collection should be visible 
#' and accessible to others, Defaults to 'FALSE'
#' @param metabolomics_nomenclature Optional metabolite dictionary for
#' metabolomics signatures. One of refmet_id, refmet, hmdb, smiles, or inchikey.
#' @param check_difexp Logical; whether or not to check difexp
#' table in the database. Defaults to 'TRUE'
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#'
#' @keywords internal
#' 
#' @export
addSignatureWithID <- function(
    conn_handler = NULL,
    omic_signature,
    assign_signature_id,
    assign_user_name,
    visibility = FALSE,
    metabolomics_nomenclature = NULL,
    check_difexp = TRUE,
    verbose = FALSE
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

  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Get visibility ####
  visibility <- base::ifelse(visibility == TRUE, 1, 0)
  
  # Check assign_signature_id
  if(!base::length(assign_signature_id) == 1 || base::all(assign_signature_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'assign_signature_id' must have a length of 1 and cannot be empty.\n")
  }else{
    signature_id <- assign_signature_id
  }
  
  # Check assign_user_name
  if(!base::length(assign_user_name) == 1 || base::all(assign_user_name %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'assign_user_name' must have a length of 1 and cannot be empty.\n")
  }else{
    user_name <- assign_user_name
  }

  if (omic_signature$metadata$assay_type[1] == "metabolomics") {
    if (base::length(metabolomics_nomenclature) == 0 || base::all(metabolomics_nomenclature %in% c("", NA))) {
      metabolomics_nomenclature <- resolveMetabolomicsFeatureConfig(
        metadata = omic_signature$metadata
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
    omic_signature = omic_signature
  )
  
  # Add additional variables in signature metadata table ####
  # Keep its original id and name of the user who owned the signature
  metadata_tbl <- metadata_tbl |> 
    dplyr::mutate(
      signature_id = signature_id[1],
      user_name = user_name[1],
      visibility = visibility[1]
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
    db_table_name = db_table_name,
    table = metadata_tbl, 
    exclude_coln_names = "date_created",
    check_db_table = FALSE
  )
  
  # Put signature back to its original form
  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = db_table_name, 
    table = metadata_tbl,
    check_db_table = FALSE
  ) 
  
  # Put difexp back to its original form
  if(base::as.numeric(metadata_tbl$has_difexp[1]) == 1 && check_difexp == TRUE){
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
      # Delete signature -- without this, a failed difexp store leaves an
      # orphaned 'signatures' row behind with has_difexp/num_of_difexp
      # already set from the in-memory object, but no difexp file and no
      # signature_feature_set rows to back it up.
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Show message
      SigRepo::stop_for_api_error(
        res = res,
        api_url = api_url,
        action = "re-upload the difexp table to the SigRepo API"
      )
    }else{
      # Remove files from file system
      base::unlink(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
    }
  }

  # Put signature set back to its original form
  #
  # Every branch below is wrapped in tryCatch (and, for assay types that can
  # fail silently by returning a non-empty warning table instead of raising
  # an error -- metabolomics chief among them, when none of the incoming
  # feature identifiers resolve against the reference tables) an explicit
  # warn_tbl check too, mirroring addSignature.R's rollback pattern. Without
  # this, a failure here leaves the 'signatures' row from earlier in this
  # function permanently orphaned: has_difexp/num_of_difexp already claim
  # data that was never actually written to signature_feature_set.
  if (metadata_tbl$assay_type[1] == "transcriptomics") {
    warn_tbl <- base::tryCatch({
      SigRepo::addTranscriptomicsSignatureSet(
        conn_handler = conn_handler,
        signature_id = metadata_tbl$signature_id[1],
        organism_id = metadata_tbl$organism_id[1],
        signature_set = omic_signature$signature,
        verbose = verbose
      )
    }, error = function(e){
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      base::suppressWarnings(DBI::dbDisconnect(conn))
      base::stop(base::as.character(e), "\n")
    })
    if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      base::suppressWarnings(DBI::dbDisconnect(conn))
      return(warn_tbl)
    }
  } else if (metadata_tbl$assay_type[1] == "proteomics") {
    warn_tbl <- base::tryCatch({
      SigRepo::addProteomicsSignatureSet(
        conn_handler = conn_handler,
        signature_id = metadata_tbl$signature_id[1],
        organism_id = metadata_tbl$organism_id[1],
        signature_set = omic_signature$signature,
        verbose = verbose
      )
    }, error = function(e){
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      base::suppressWarnings(DBI::dbDisconnect(conn))
      base::stop(base::as.character(e), "\n")
    })
    if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      base::suppressWarnings(DBI::dbDisconnect(conn))
      return(warn_tbl)
    }
  } else if (metadata_tbl$assay_type[1] == "metabolomics") {
    warn_tbl <- base::tryCatch({
      SigRepo::addMetabolomicsSignatureSet(
        conn_handler = conn_handler,
        signature_id = metadata_tbl$signature_id[1],
        signature_set = omic_signature$signature,
        feature_database = metabolomics_nomenclature,
        verbose = verbose
      )
    }, error = function(e){
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      base::suppressWarnings(DBI::dbDisconnect(conn))
      base::stop(base::as.character(e), "\n")
    })
    if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      base::suppressWarnings(DBI::dbDisconnect(conn))
      return(warn_tbl)
    }
  } else if (metadata_tbl$assay_type[1] == "genetic_variants") {
    warn_tbl <- base::tryCatch({
      SigRepo::addGeneticVariantsSignatureSet(
        conn_handler = conn_handler,
        signature_id = metadata_tbl$signature_id[1],
        organism_id = metadata_tbl$organism_id[1],
        signature_set = omic_signature$signature,
        verbose = verbose
      )
    }, error = function(e){
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      base::suppressWarnings(DBI::dbDisconnect(conn))
      base::stop(base::as.character(e), "\n")
    })
    if(methods::is(warn_tbl, "data.frame") && base::nrow(warn_tbl) > 0){
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
      base::suppressWarnings(DBI::dbDisconnect(conn))
      return(warn_tbl)
    }
  } else {
    SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
    base::suppressWarnings(DBI::dbDisconnect(conn))
    SigRepo::showAssayTypeErrorMessage(unknown_values = metadata_tbl$assay_type[1])
  }

  # Add user to signature access table after signature
  base::tryCatch({
    SigRepo::addUserToSignature(
      conn_handler = conn_handler,
      signature_id = metadata_tbl$signature_id[1],
      user_name = user_name,
      access_type = "owner",
      verbose = verbose
    )
  }, error = function(e){
    SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = metadata_tbl$signature_id[1], verbose = FALSE)
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::as.character(e), "\n")
  })

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))

  # Return
  return(base::invisible())

}
