#' @title addGenesetResource
#' @description Add geneset resource metadata to the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param geneset_resource A data frame containing geneset resource metadata.
#' Required columns are \code{source}, \code{species}, \code{collection},
#' \code{version}, \code{format}, \code{storage_path}, and \code{is_current}.
#' @param verbose Logical; whether to print diagnostic messages.
#' Defaults to \code{TRUE}.
#'
#' @export
addGenesetResource <- function(
    conn_handler = NULL,
    geneset_resource,
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

  db_table_name <- "geneset_resources"
  table <- geneset_resource
  required_column_fields <- c(
    "source",
    "species",
    "collection",
    "version",
    "format",
    "storage_path",
    "is_current"
  )

  if (base::any(!required_column_fields %in% base::colnames(table))) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\n'geneset resource' table is missing the following required column names: %s.\n",
        base::paste0(required_column_fields[base::which(!required_column_fields %in% base::colnames(table))], collapse = ", ")
      )
    )
  }

  if (base::any(base::is.na(table[, required_column_fields]) == TRUE)) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\nAll required column names in 'geneset resource' table: %s cannot contain any empty values.\n",
        base::paste0(required_column_fields, collapse = ", ")
      )
    )
  }

  optional_columns <- c(
    "subcollection",
    "source_version",
    "checksum",
    "n_genesets",
    "n_features",
    "notes"
  )

  for (coln in optional_columns) {
    if (!coln %in% base::colnames(table)) {
      table[[coln]] <- NA
    }
  }

  table <- table |>
    dplyr::mutate(
      subcollection_hash = dplyr::if_else(
        base::is.na(.data$subcollection) | .data$subcollection %in% "",
        "__NULL__",
        base::as.character(.data$subcollection)
      )
    )

  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "geneset_resource_hashkey",
    hash_columns = c("source", "species", "collection", "subcollection_hash", "version"),
    hash_method = "md5"
  ) |>
    dplyr::select(-"subcollection_hash")

  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    exclude_coln_names = c("geneset_resource_id", "created_at", "updated_at"),
    check_db_table = FALSE
  )

  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "geneset_resource_hashkey",
    check_db_table = FALSE
  )

  if (base::nrow(table) > 0) {
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      table = table,
      check_db_table = FALSE
    )
  }

  base::suppressWarnings(DBI::dbDisconnect(conn))

  SigRepo::verbose("Finished uploading.\n")
}
