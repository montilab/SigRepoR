#' @title addMetabolomicsSignatureSet
#' @description Add metabolomics signature feature set to database.
#' @param conn_handler Optional R object obtained from SigRepo::newConnhandler().
#' If NULL, the stored internal handle is used.
#' @param signature_id Database ID of the signature (required)
#' @param signature_set A Data Frame; must contain the following column names:
#' feature_name, probe_id, score, group_label (required)
#' @param feature_database Metabolomics dictionary used by the incoming
#' signature's \code{feature_name} column. One of refmet_id, refmet, hmdb,
#' smiles, or inchikey. If \code{feature_database = "refmet"} and
#' \code{signature_set} also contains \code{refmet_id}, those IDs are preferred
#' for lookup over \code{feature_name}.
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#'
#' @export
addMetabolomicsSignatureSet <- function(
    conn_handler = NULL,
    signature_id,
    signature_set,
    feature_database,
    verbose = TRUE
){

  SigRepo::print_messages(verbose = verbose)

  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)

  conn_info <- SigRepo::checkPermissions(
    conn = conn,
    action_type = "INSERT",
    required_role = "editor"
  )

  user_role <- conn_info$user_role[1]
  user_name <- conn_info$user[1]

  if (!base::length(signature_id) == 1 || signature_id %in% c(NA, "")) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop("\n'signature_id' must have a length of 1 and cannot be empty.\n")
  }

  required_fields <- c("feature_name", "probe_id", "score", "group_label")
  if (!methods::is(signature_set, "data.frame") || base::nrow(signature_set) == 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop("\n'signature_set' must be a data frame and cannot be empty.\n")
  }

  if (base::any(!required_fields %in% base::colnames(signature_set))) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::sprintf("\n'signature_set' must have the following column names: %s.\n", base::paste0(required_fields, collapse = ", ")))
  }

  if (base::any(base::is.na(signature_set[, required_fields]) == TRUE)) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::sprintf("\nAll required column names in 'signature_set': %s cannot contain any empty values.\n", base::paste0(required_fields, collapse = ", ")))
  }

  config <- resolveMetabolomicsFeatureConfig(feature_database = feature_database)
  db_table_name <- "signature_feature_set"
  attempted_growth <- FALSE

  if (user_role != "admin") {
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = c("signature_id", "user_name"),
      filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name),
      filter_var_by = "AND",
      check_db_table = TRUE
    )
  } else {
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = "signature_id",
      filter_coln_val = list("signature_id" = signature_id),
      check_db_table = TRUE
    )
  }

  if (base::nrow(signature_tbl) == 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::sprintf("\nThere is no signature_id = '%s' belonged to user = '%s' in the 'signatures' table of the SigRepo database.\n", signature_id, user_name))
  }

  table <- signature_set |>
    dplyr::mutate(
      signature_id = signature_id,
      assay_type = "metabolomics",
      nomenclature_type = config$feature_database,
      match_status = "resolved"
    )

  lookup_spec <- resolveMetabolomicsSignatureLookupSpec(
    signature_set = table,
    feature_database = config$feature_database
  )

  table <- table |>
    dplyr::mutate(lookup_feature_value = lookup_spec$feature_values)

  lookup_tbl <- resolveMetabolomicsSignatureMatches(
    conn = conn,
    feature_database = lookup_spec$feature_database,
    feature_values = table$lookup_feature_value
  )

  if (config$maintenance_model == "growing") {
    missing_values <- base::setdiff(base::unique(table$lookup_feature_value), base::unique(lookup_tbl$input_feature_name))

    if (base::length(missing_values) > 0) {
      attempted_growth <- TRUE
      missing_feature_tbl <- table |>
        dplyr::filter(.data$lookup_feature_value %in% missing_values) |>
        dplyr::distinct(.data$lookup_feature_value, .keep_all = TRUE) |>
        dplyr::mutate(
          feature_name = .data$lookup_feature_value,
          is_current = 1,
          version = as.integer(base::format(base::Sys.Date(), "%m%d%Y"))
        )

      SigRepo::addMetabolomicsFeatureSet(
        conn_handler = conn_handler,
        feature_set = missing_feature_tbl,
        feature_database = config$feature_database,
        verbose = FALSE
      )

      lookup_tbl <- resolveMetabolomicsSignatureMatches(
        conn = conn,
        feature_database = lookup_spec$feature_database,
        feature_values = table$lookup_feature_value
      )
    }
  }

  if (base::nrow(lookup_tbl) == 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn))

    unknown_values <- base::setdiff(base::unique(table$lookup_feature_value), base::unique(lookup_tbl$input_feature_name))

    SigRepo::showMetabolomicsErrorMessage(
      db_table_name = config$lookup_table,
      unknown_values = unknown_values,
      feature_database = config$feature_database,
      attempted_growth = attempted_growth
    )

    return(base::data.frame(table = config$lookup_table, unknown_values = unknown_values))
  }

  match_summary_tbl <- lookup_tbl |>
    dplyr::distinct(.data$input_feature_name, .data$metabolite_id) |>
    dplyr::count(.data$input_feature_name, name = "n_matches")

  unresolved_values <- base::setdiff(base::unique(table$lookup_feature_value), base::unique(lookup_tbl$input_feature_name))
  if (base::length(unresolved_values) > 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn))

    SigRepo::showMetabolomicsErrorMessage(
      db_table_name = config$lookup_table,
      unknown_values = unresolved_values,
      feature_database = config$feature_database,
      attempted_growth = attempted_growth
    )

    return(base::data.frame(table = config$lookup_table, unknown_values = unresolved_values))
  }

  resolved_lookup_tbl <- lookup_tbl |>
    dplyr::distinct(.data$input_feature_name, .data$metabolite_id) |>
    dplyr::inner_join(
      match_summary_tbl |>
        dplyr::filter(.data$n_matches == 1) |>
        dplyr::select(.data$input_feature_name),
      by = "input_feature_name"
    )

  ambiguity_report <- buildMetabolomicsAmbiguityReport(
    lookup_tbl = lookup_tbl |>
      dplyr::filter(.data$input_feature_name %in% (match_summary_tbl |>
        dplyr::filter(.data$n_matches > 1) |>
        dplyr::pull(.data$input_feature_name)))
  )

  table <- table |>
    dplyr::left_join(
      resolved_lookup_tbl,
      by = c("lookup_feature_value" = "input_feature_name")
    ) |>
    dplyr::mutate(
      feature_id = .data$metabolite_id,
      match_status = dplyr::if_else(base::is.na(.data$feature_id), "ambiguous", "resolved")
    ) |>
    dplyr::select(-.data$metabolite_id)

  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "sig_feature_hashkey",
    hash_columns = c("signature_id", "assay_type", "nomenclature_type", "probe_id", "feature_name"),
    hash_method = "md5"
  )

  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table |> dplyr::select(-.data$lookup_feature_value),
    check_db_table = TRUE
  )

  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "sig_feature_hashkey",
    check_db_table = FALSE
  )

  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    check_db_table = FALSE
  )

  if (base::nrow(ambiguity_report) > 0) {
    ambiguity_rows <- table |>
      dplyr::filter(.data$match_status == "ambiguous") |>
      dplyr::select(.data$feature_name, .data$sig_feature_hashkey) |>
      dplyr::distinct() |>
      dplyr::left_join(
        lookup_tbl |>
          dplyr::distinct(.data$input_feature_name, .data$metabolite_id),
        by = c("feature_name" = "input_feature_name")
      ) |>
      dplyr::transmute(
        sig_feature_hashkey = .data$sig_feature_hashkey,
        candidate_metabolite_id = .data$metabolite_id
      )

    insertMetabolomicsAmbiguityRows(
      conn = conn,
      ambiguity_tbl = ambiguity_rows
    )

    SigRepo::showMetabolomicsAmbiguityMessage(ambiguity_tbl = ambiguity_report)
  }

  base::suppressWarnings(DBI::dbDisconnect(conn))

  return(base::invisible())
}
