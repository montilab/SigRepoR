#' @title updateProteomicsFeatureSet
#' @description Refresh an organism's proteomics reference features from
#' UniProt: every accession for the organism with its primary gene name.
#' Existing rows are re-symbolled where UniProt now has a name, features that
#' left UniProt are archived, and new accessions are added. A stored symbol is
#' never replaced by a blank; stored placeholders (UniProt entry names such as
#' `1433B_HUMAN`, or the accession copied into the symbol column) are treated
#' as blank so a real name can take their place, and are cleared if none does.
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param organism An organism to extract and update its features (required)
#' @param verbose A logical value of whether to print diagnostic messages.
#' Defaults to 'TRUE'.
#'
#' @export
updateProteomicsFeatureSet <- function(
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

  bail <- function(...) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(..., call. = FALSE)
  }

  # Check organism (required) #####
  if(base::length(organism) != 1 || base::all(organism %in% c(NA, ""))){
    bail("'organism' must have a length of 1 and cannot be empty.\n")
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

  if(base::nrow(organism_tbl) == 0){
    bail("There are no organisms returned from the search parameters.\n")
  }

  organism_id <- organism_tbl$organism_id[1]
  taxid <- base::trimws(base::as.character(organism_tbl$prot_organism_taxid[1]))
  organism_code <- base::trimws(base::as.character(organism_tbl$prot_organism_code[1]))
  if(base::is.na(taxid) || !base::nzchar(taxid)){
    bail(base::sprintf("Organism = '%s' has no UniProt taxonomy id (organisms.prot_organism_taxid). Updates aborted.\n", organism))
  }

  # Download the organism's accessions and primary gene names from UniProt ####
  url <- uniprotGeneNameUrl(taxid)
  tmp_file <- base::tempfile(fileext = ".tsv.gz")
  on.exit(base::unlink(tmp_file), add = TRUE)
  SigRepo::verbose(base::sprintf("Downloading UniProt gene names for %s:\n %s\n", organism, url))
  base::tryCatch({
    utils::download.file(url = url, destfile = tmp_file, method = "curl", quiet = TRUE)
  }, error = function(e){
    bail("Download from UniProt failed:\n", base::conditionMessage(e), "\n")
  })

  fetched <- base::tryCatch(parseUniprotGeneNames(tmp_file), error = function(e){
    bail("Could not read the UniProt download:\n", base::conditionMessage(e), "\n")
  })
  if(base::nrow(fetched) == 0){
    bail("There are no features returned from UniProt. Updates aborted.\n")
  }
  SigRepo::verbose(base::sprintf("UniProt lists %d accessions, %d with a primary gene name.\n", base::nrow(fetched), base::sum(base::nzchar(fetched$gene_symbol))))

  # Stored features for this organism ####
  stored_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "proteomics_features",
    return_var = "*",
    filter_coln_var = "organism_id",
    filter_coln_val = base::list("organism_id" = organism_id),
    check_db_table = TRUE
  )
  stored <- base::data.frame(
    feature_name = base::as.character(stored_tbl$feature_name),
    gene_symbol = base::as.character(stored_tbl$gene_symbol),
    stringsAsFactors = FALSE
  )
  stored$gene_symbol[base::is.na(stored$gene_symbol)] <- ""

  # Placeholders are not symbols: let a real name replace them, and clear
  # whatever placeholder no real name replaces.
  placeholder <- proteomicsSymbolIsPlaceholder(stored$gene_symbol, stored$feature_name, organism_code)
  for_plan <- stored
  for_plan$gene_symbol[placeholder] <- ""
  plan <- partitionFeatureUpdates(for_plan, fetched)
  to_clear <- base::setdiff(stored$feature_name[placeholder], plan$resymbol$feature_name)

  if(base::nrow(plan$resymbol) == 0 && base::nrow(plan$add) == 0 && base::nrow(plan$archive) == 0 && base::length(to_clear) == 0){
    bail("All features are up to date. No further updates needed.\n")
  }

  new_version <- base::format(base::Sys.Date(), "%Y-%m-%d")
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
      bail(base::conditionMessage(e), "\n")
    })
  }

  SigRepo::verbose(base::sprintf(
    "Refreshing %d, re-symbolling %d, clearing %d placeholder(s), adding %d, archiving %d feature(s)...\n",
    base::nrow(plan$refresh), base::nrow(plan$resymbol), base::length(to_clear), base::nrow(plan$add), base::nrow(plan$archive)
  ))

  # Unchanged symbols: stamp the new version only
  for(names in in_chunks(plan$refresh$feature_name)){
    run_statement(base::sprintf(
      "UPDATE proteomics_features SET version = '%s', is_current = 1 WHERE feature_name IN (%s) AND organism_id = %s;",
      new_version, sql_list(names), organism_id
    ))
  }

  # Changed, blank, or placeholder symbols that UniProt now names: rewrite them
  for(idx in in_chunks(base::seq_len(base::nrow(plan$resymbol)))){
    chunk <- plan$resymbol[idx, , drop = FALSE]
    cases <- base::paste(base::sprintf("WHEN '%s' THEN '%s'", sql_quote(chunk$feature_name), sql_quote(chunk$gene_symbol)), collapse = " ")
    run_statement(base::sprintf(
      "UPDATE proteomics_features SET gene_symbol = CASE feature_name %s END, is_current = 1, version = '%s' WHERE feature_name IN (%s) AND organism_id = %s;",
      cases, new_version, sql_list(chunk$feature_name), organism_id
    ))
  }

  # Placeholders UniProt has no name for: clear them rather than keep a fake symbol
  for(names in in_chunks(to_clear)){
    run_statement(base::sprintf(
      "UPDATE proteomics_features SET gene_symbol = NULL, is_current = 1, version = '%s' WHERE feature_name IN (%s) AND organism_id = %s;",
      new_version, sql_list(names), organism_id
    ))
  }

  # Accessions not stored yet: add them
  if(base::nrow(plan$add) > 0){
    SigRepo::addProteomicsFeatureSet(
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

  # Accessions UniProt no longer lists: archive them
  for(names in in_chunks(plan$archive$feature_name)){
    run_statement(base::sprintf(
      "UPDATE proteomics_features SET is_current = 0 WHERE feature_name IN (%s) AND organism_id = %s;",
      sql_list(names), organism_id
    ))
  }

  # Record the refresh on the organism ####
  run_statement(base::sprintf(
    "UPDATE organisms SET prot_updated_date = '%s' WHERE organism_id = %s;",
    new_version, organism_id
  ))

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))

  # Return message
  SigRepo::verbose("Finished updating.\n")
}
