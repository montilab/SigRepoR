#' @title updateTranscriptomicsFeatureSet
#' @description Extract data from biomaRt package and update transcriptomics 
#' feature set in the database.
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param organism An organism to extract and update its features (required)
#' @param verbose A logical value of whether to print diagnostic messages. 
#' Defaults to 'TRUE'.
#' 
#' @import biomaRt
#'
#' @export
updateTranscriptomicsFeatureSet <- function(
    conn_handler = NULL,
    organism, 
    verbose = TRUE
) {
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "UPDATE",
    required_role = "admin"
  )  
  
  # Check organism (required) #####
  if(base::length(organism) != 1 || base::all(organism %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Return error message
    base::stop("'organism' must have a length of 1 and cannot be empty.\n")
  }
  
  # Look up organism in the database
  organism_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "organisms", 
    return_var = "*", 
    filter_coln_var = "organism", 
    filter_coln_val = base::list("organism" = base::unique(organism)),
    check_db_table = TRUE
  ) 
  
  # Check if organisms exists
  if(base::nrow(organism_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    
    # Show message
    base::stop("There are no organisms returned from the search parameters.\n")
    
  }else{
    
    # Show message
    SigRepo::verbose("Getting the latest version available in the biomaRt...\n")
    
    ensembl_genes <- biomaRt::listEnsembl() |> dplyr::filter(.data$biomart %in% "genes")
    current_release <- ensemblReleaseNumber(ensembl_genes$version)[1]
    stored_release <- ensemblReleaseNumber(organism_tbl$biomart_version[1])

    if(base::is.na(current_release)){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      base::stop("Could not determine the current Ensembl release from biomaRt::listEnsembl(). Updates aborted.\n")
    }

    # Show message
    SigRepo::verbose(base::sprintf("Checking biomaRt version against the database version...\n"))

    # Compared as integers: the old string comparison ordered "116" before "99".
    if(!base::is.na(stored_release) && current_release < stored_release){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      base::stop(base::sprintf("Current biomaRt version = '%s' is older than the database version = '%s'. Updates canceled.\n", current_release, stored_release))
    }else if(!base::is.na(stored_release) && current_release == stored_release){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      base::stop(base::sprintf("Current biomaRt version = '%s' is the same as the database version = '%s'. No updates needed.\n", current_release, stored_release))
    }
    
    # Get biomaRt dataset. Deliberately no `version =`: we only get here when
    # the current release is newer than the stored one, so the current release
    # is what we want, and asking for it by number routes every query to
    # www.ensembl.org, which answers the full-table getBM() with HTTP 405.
    # Without `version` biomaRt picks a working mirror.
    ensembl <- base::tryCatch({
      biomaRt::useEnsembl(biomart = organism_tbl$biomart_db[1], dataset = organism_tbl$biomart_dataset[1])
    }, error = function(e){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      base::stop("Error in BiomaRt package:\n", base::as.character(e), "\n")
    })
    
    # Get transcriptomics features in the database
    transcriptomics_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "transcriptomics_features", 
      return_var = "*", 
      filter_coln_var = "organism_id", 
      filter_coln_val = base::list("organism_id" = organism_tbl$organism_id[1]),
      check_db_table = TRUE
    ) |> 
      dplyr::transmute(
        feature_name = base::trimws(base::tolower(.data$feature_name)),
        gene_symbol = base::trimws(base::tolower(.data$gene_symbol)),
        orig_feature_name = .data$feature_name,
        orig_gene_symbol = .data$gene_symbol,
        organism_id = .data$organism_id,
        is_current = .data$is_current,
        version = .data$version
      ) |>
      dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })
    
    # Grab ensembl ids and gene symbols. The attribute that carries symbols
    # depends on the dataset: hgnc_symbol is only populated for human, mouse
    # needs mgi_symbol, and every other organism gets external_gene_name.
    # Asking every organism for hgnc_symbol came back blank for mouse and
    # erased the symbols /init_db had loaded.
    symbol_attribute <- symbolAttributeForOrganism(organism_tbl$organism[1])
    feature_tbl <- base::tryCatch({
      biomaRt::getBM(
        attributes = c("ensembl_gene_id", symbol_attribute),
        mart = ensembl
      ) |>
        dplyr::transmute(
          feature_name = base::trimws(.data$ensembl_gene_id),
          gene_symbol = base::trimws(.data[[symbol_attribute]])
        ) |>
        dplyr::distinct(.data$feature_name, .keep_all = TRUE) |>
        dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })
    }, error = function(e){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Return error message
      base::stop(e, "\n")
    })

    # Check if biomaRt features are empty #####
    if(base::nrow(feature_tbl) == 0){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Return error message
      base::stop("There are no features returned from biomaRt. Updates aborted.\n")
    }

    # Decide what to do with each stored feature. A blank fetched symbol never
    # replaces a stored one, so a source without symbols cannot erase them.
    plan <- partitionFeatureUpdates(
      db_tbl = base::data.frame(
        feature_name = transcriptomics_tbl$orig_feature_name,
        gene_symbol = transcriptomics_tbl$orig_gene_symbol,
        stringsAsFactors = FALSE
      ),
      fetched_tbl = feature_tbl
    )

    # Check if there is a need to update
    if(base::nrow(plan$resymbol) == 0 && base::nrow(plan$add) == 0 && base::nrow(plan$archive) == 0){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Return error message
      base::stop("All features are up to date. No further updates needed.\n")
    }

    new_version <- current_release
    organism_id <- organism_tbl$organism_id[1]
    sql_quote <- function(x) base::gsub("'", "''", x, fixed = TRUE)
    sql_list <- function(x) base::paste0("'", sql_quote(x), "'", collapse = ", ")
    in_chunks <- function(x, size = 1000){
      if(base::length(x) == 0) return(base::list())
      base::split(x, base::ceiling(base::seq_along(x) / size))
    }
    run_statement <- function(statement){
      base::tryCatch({
        base::suppressWarnings(DBI::dbExecute(conn = conn, statement = statement))
      }, error = function(e){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))
        # Return error message
        base::stop(e, "\n")
      })
    }

    # Show message
    SigRepo::verbose("Updating features to latest version...\n")

    # Unchanged (or unresolvable) symbols: stamp the new version only
    for(names in in_chunks(plan$refresh$feature_name)){
      run_statement(base::sprintf(
        "UPDATE transcriptomics_features SET version = %s, is_current = 1 WHERE feature_name IN (%s) AND organism_id = %s;",
        new_version, sql_list(names), organism_id
      ))
    }

    # Symbols that changed, or were blank and are now known: rewrite them
    for(s in base::seq_len(base::nrow(plan$resymbol))){
      run_statement(base::sprintf(
        "UPDATE transcriptomics_features SET gene_symbol = '%s', is_current = 1, version = %s WHERE feature_name = '%s' AND organism_id = %s;",
        sql_quote(plan$resymbol$gene_symbol[s]), new_version, sql_quote(plan$resymbol$feature_name[s]), organism_id
      ))
    }

    # Features not stored yet: add them
    if(base::nrow(plan$add) > 0){
      SigRepo::addTranscriptomicsFeatureSet(
        conn_handler = conn_handler,
        feature_set = base::data.frame(
          feature_name = plan$add$feature_name,
          gene_symbol = plan$add$gene_symbol,
          organism = organism,
          is_current = 1,
          version = new_version,
          stringsAsFactors = FALSE
        )
      )
    }

    # Features that left this Ensembl release: archive them. (This used to
    # archive the overlapping list by mistake, retiring still-current genes.)
    for(names in in_chunks(plan$archive$feature_name)){
      run_statement(base::sprintf(
        "UPDATE transcriptomics_features SET is_current = 0 WHERE feature_name IN (%s) AND organism_id = %s;",
        sql_list(names), organism_id
      ))
    }

    # Show message
    SigRepo::verbose("Updating organism table to latest version...\n")
    
    # Create SQL statement to update version in organisms table
    statement <- base::sprintf(
      "
        UPDATE organisms
        SET biomart_version = '%s', biomart_updated_date = '%s'
        WHERE organism_id = %s;
        ", current_release, base::as.Date(base::Sys.Date(), format = "%Y-%m-%d"), organism_tbl$organism_id[1]
    )
    
    # RUN SQL
    base::tryCatch({
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
    }, error = function(e){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    })  
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    
    # Return message
    SigRepo::verbose("Finished updating.\n")
    
  }
}  
