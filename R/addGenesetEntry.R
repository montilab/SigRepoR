#' @title addGenesetEntry
#' @description Add geneset entry metadata to the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param geneset_entry A data frame containing geneset entry metadata.
#' Required columns are \code{geneset_resource_id} and \code{geneset_name}.
#' Optional columns include \code{description} and \code{n_features}.
#' @param verbose Logical; whether to print diagnostic messages.
#' Defaults to \code{TRUE}.
#'
#' @export
addGenesetEntry <- function(
    conn_handler = NULL,
    geneset_entry,
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

  db_table_name <- "geneset_entries"
  table <- geneset_entry
  required_column_fields <- c("geneset_resource_id", "geneset_name")

  if (base::any(!required_column_fields %in% base::colnames(table))) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\n'geneset entry' table is missing the following required column names: %s.\n",
        base::paste0(required_column_fields[base::which(!required_column_fields %in% base::colnames(table))], collapse = ", ")
      )
    )
  }

  if (base::any(base::is.na(table[, required_column_fields]) == TRUE)) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\nAll required column names in 'geneset entry' table: %s cannot contain any empty values.\n",
        base::paste0(required_column_fields, collapse = ", ")
      )
    )
  }

  if (!"description" %in% base::colnames(table)) {
    table$description <- NA
  }

  if (!"n_features" %in% base::colnames(table)) {
    table$n_features <- NA
  }

  resource_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "geneset_resources",
    return_var = "geneset_resource_id",
    filter_coln_var = "geneset_resource_id",
    filter_coln_val = base::list("geneset_resource_id" = base::unique(table$geneset_resource_id)),
    check_db_table = TRUE
  )

  if (base::any(!base::unique(table$geneset_resource_id) %in% resource_tbl$geneset_resource_id)) {
    missing_ids <- base::unique(table$geneset_resource_id[!table$geneset_resource_id %in% resource_tbl$geneset_resource_id])
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\nThe following geneset_resource_id values do not exist in 'geneset_resources': %s.\n",
        base::paste0(missing_ids, collapse = ", ")
      )
    )
  }

  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "geneset_entry_hashkey",
    hash_columns = c("geneset_resource_id", "geneset_name"),
    hash_method = "md5"
  )

  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    exclude_coln_names = "geneset_entry_id",
    check_db_table = FALSE
  )

  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "geneset_entry_hashkey",
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
