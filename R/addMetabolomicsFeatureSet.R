#' @title addMetabolomicsFeatureSet
#' @description Add metabolomics features to the canonical metabolite reference
#' and mapping tables. Each input row is treated as a metabolite record that may
#' contain multiple identifiers at once, and all non-empty identifier columns are
#' written to the mapping table.
#' @param conn_handler Optional R object obtained from SigRepo::newConnhandler().
#' If NULL, the stored internal handle is used.
#' @param feature_set A data frame containing metabolite reference rows. Typical
#' columns include \code{refmet_id}, \code{refmet_name}, \code{hmdb_id},
#' \code{smiles}, \code{inchikey} (or \code{inchi_key}), \code{is_current},
#' and \code{version}.
#' @param feature_database Optional metabolomics identifier type used to interpret
#' \code{feature_name}, when \code{feature_name} is supplied instead of a
#' dedicated identifier column. One of refmet_id, refmet, hmdb, smiles, or
#' inchikey. This argument does \emph{not} limit which
#' mappings are inserted; all non-empty identifier columns in \code{feature_set}
#' are uploaded to \code{metabolite_xref}. If omitted, SigRepo infers a primary
#' identifier type from the non-empty identifier columns in \code{feature_set}.
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#'
#' @details
#' Use this function to load the metabolite reference and mapping tables
#' themselves. In the current API, \code{feature_database} acts only as an input
#' hint so the function knows how to interpret \code{feature_name} if that
#' generic column is used. It is most relevant when the uploaded table uses
#' \code{feature_name} rather than explicit columns such as \code{hmdb_id} or
#' \code{smiles}. When omitted, SigRepo infers the primary identifier type
#' from the uploaded columns, preferring \code{refmet_id}, then
#' \code{refmet_name}, followed by HMDB, SMILES, and InChIKey.
#'
#' By contrast, \code{feature_database} is semantically important in
#' \code{searchMetabolomicsFeatureSet()} and \code{addMetabolomicsSignatureSet()},
#' where the caller is explicitly choosing which identifier namespace to search
#' or map against.
#'
#' Large uploads are inserted in batches to avoid oversized SQL statements.
#'
#' @export
addMetabolomicsFeatureSet <- function(
    conn_handler = NULL,
    feature_set,
    feature_database = NULL,
    verbose = TRUE
){

  SigRepo::print_messages(verbose = verbose)

  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)

  SigRepo::checkPermissions(
    conn = conn,
    action_type = "INSERT",
    required_role = "admin"
  )

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    feature_database <- inferMetabolomicsFeatureDatabase(feature_set = feature_set)
  }

  config <- resolveMetabolomicsFeatureConfig(feature_database = feature_database)
  normalized_tbl <- normalizeMetabolomicsFeatureSet(
    feature_set = feature_set,
    feature_database = config$feature_database
  )

  required_column_fields <- c("refmet_id", "refmet_name", "is_current", "version")

  if (base::any(base::is.na(normalized_tbl[, required_column_fields]) == TRUE)) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\nAll required column names in 'metabolomics features' table: %s cannot contain any empty values.\n",
        base::paste0(required_column_fields, collapse = ", ")
      )
    )
  }

  reference_tbl <- buildMetabolomicsReferenceRows(
    feature_set = normalized_tbl,
    feature_database = config$feature_database
  )

  reference_tbl <- addMetaboliteHashKey(reference_tbl)

  reference_tbl <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = config$reference_table,
    table = reference_tbl,
    exclude_coln_names = "metabolite_id",
    check_db_table = FALSE
  )

  reference_tbl <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = config$reference_table,
    table = reference_tbl,
    coln_var = "metabolite_hashkey",
    check_db_table = FALSE
  )

  if (base::nrow(reference_tbl) > 0) {
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = config$reference_table,
      table = reference_tbl,
      batch_size = 500,
      check_db_table = FALSE
    )
  }

  lookup_reference_tbl <- buildMetabolomicsReferenceRows(
    feature_set = normalized_tbl,
    feature_database = config$feature_database
  )

  lookup_reference_tbl <- addMetaboliteHashKey(lookup_reference_tbl)

  metabolite_identity_columns <- c("refmet_id", "refmet_name", "hmdb_id", "smiles", "inchikey")

  metabolite_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = config$reference_table,
    return_var = c("metabolite_id", "metabolite_hashkey"),
    filter_coln_var = "metabolite_hashkey",
    filter_coln_val = base::list("metabolite_hashkey" = base::unique(lookup_reference_tbl$metabolite_hashkey)),
    check_db_table = FALSE
  )

  xref_hash_lookup_tbl <- buildMetabolomicsXrefRows(
    feature_set = normalized_tbl,
    feature_database = config$feature_database
  ) |>
    dplyr::select(dplyr::all_of(metabolite_identity_columns)) |>
    dplyr::distinct() |>
    addMetaboliteHashKey() |>
    dplyr::select(dplyr::all_of(c(metabolite_identity_columns, "metabolite_hashkey")))

  xref_tbl <- buildMetabolomicsXrefRows(
    feature_set = normalized_tbl,
    feature_database = config$feature_database
  ) |>
    dplyr::left_join(
      xref_hash_lookup_tbl,
      by = metabolite_identity_columns
    ) |>
    dplyr::left_join(
      metabolite_tbl,
      by = "metabolite_hashkey"
    ) |>
    dplyr::transmute(
      metabolite_id = .data$metabolite_id,
      source_db = .data$source_db,
      source_value = .data$source_value,
      is_primary = ifelse(.data$source_db %in% config$xref_source_db, 1, 0)
    ) |>
    dplyr::distinct()

  if (base::any(base::is.na(xref_tbl$metabolite_id))) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop("Unable to resolve metabolite IDs for all metabolomics xref rows.\n")
  }

  xref_tbl <- SigRepo::createHashKey(
    table = xref_tbl,
    hash_var = "xref_hashkey",
    hash_columns = c("metabolite_id", "source_db", "source_value"),
    hash_method = "md5"
  )

  xref_tbl <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = config$xref_table,
    table = xref_tbl,
    exclude_coln_names = "xref_id",
    check_db_table = FALSE
  )

  xref_tbl <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = config$xref_table,
    table = xref_tbl,
    coln_var = "xref_hashkey",
    check_db_table = FALSE
  )

  if (base::nrow(xref_tbl) > 0) {
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = config$xref_table,
      table = xref_tbl,
      batch_size = 1000,
      check_db_table = FALSE
    )
  }

  base::suppressWarnings(DBI::dbDisconnect(conn))

  SigRepo::verbose("Finished uploading.\n")
}
