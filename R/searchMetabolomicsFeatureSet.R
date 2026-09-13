#' @title searchMetabolomicsFeatureSet
#' @description Search metabolomics features through a selected identifier
#' namespace.
#' @param conn_handler Optional R object obtained from SigRepo::newConnhandler().
#' If NULL, the stored internal handle is used.
#' @param feature_database Metabolomics dictionary to search against. One of
#' refmet_id, refmet, hmdb, smiles, or inchikey. This
#' chooses which identifier namespace should be matched in the mapping table.
#' @param feature_name A feature, or list of features, to look up.
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#'
#' @export
searchMetabolomicsFeatureSet <- function(
    conn_handler = NULL,
    feature_database,
    feature_name = NULL,
    verbose = TRUE
){

  SigRepo::print_messages(verbose = verbose)

  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)

  SigRepo::checkPermissions(
    conn = conn,
    action_type = "SELECT",
    required_role = "viewer"
  )

  config <- resolveMetabolomicsFeatureConfig(feature_database = feature_database)

  reference_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = config$reference_table,
    return_var = "*",
    check_db_table = TRUE
  )

  if (config$feature_database == "refmet") {
    tbl <- reference_tbl |>
      dplyr::transmute(
        metabolite_id = .data$metabolite_id,
        feature_name = .data$refmet_name,
        refmet_id = .data$refmet_id,
        refmet_name = .data$refmet_name,
        hmdb_id = .data$hmdb_id,
        smiles = .data$smiles,
        inchikey = .data$inchikey,
        is_current = .data$is_current,
        version = .data$version
      )
  } else {
    xref_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = config$xref_table,
      return_var = c("metabolite_id", "source_db", "source_value", "is_primary"),
      filter_coln_var = "source_db",
      filter_coln_val = list("source_db" = config$xref_source_db),
      check_db_table = TRUE
    )

    tbl <- xref_tbl |>
      dplyr::left_join(
        reference_tbl |>
          dplyr::select(c(
            "metabolite_id", "refmet_id", "refmet_name", "hmdb_id",
            "smiles", "inchikey", "is_current", "version"
          )),
        by = "metabolite_id"
      ) |>
      dplyr::transmute(
        metabolite_id = .data$metabolite_id,
        feature_name = .data$source_value,
        refmet_id = .data$refmet_id,
        refmet_name = .data$refmet_name,
        hmdb_id = .data$hmdb_id,
        smiles = .data$smiles,
        inchikey = .data$inchikey,
        is_primary = .data$is_primary,
        is_current = .data$is_current,
        version = .data$version
      )
  }

  filter_var_list <- base::list(
    "feature_name" = base::unique(feature_name)
  )

  for (r in base::seq_along(filter_var_list)) {
    filter_status <- base::ifelse(base::length(filter_var_list[[r]]) == 0 || base::all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
    if (filter_status == TRUE) {
      filter_var <- base::names(filter_var_list)[r]
      filter_val <- filter_var_list[[r]][base::which(!filter_var_list[[r]] %in% c(NA, ""))]
      tbl <- tbl |> dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }

  base::suppressWarnings(DBI::dbDisconnect(conn))

  tbl
}
