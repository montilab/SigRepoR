#' @title searchGenesetEntry
#' @description Search geneset entry metadata in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param geneset_resource_id A geneset resource ID to look up. Defaults to \code{NULL}.
#' @param source A geneset source to look up. Defaults to \code{NULL}.
#' @param species A species to look up. Defaults to \code{NULL}.
#' @param collection A geneset collection to look up. Defaults to \code{NULL}.
#' @param subcollection A geneset subcollection to look up. Defaults to \code{NULL}.
#' @param version A geneset resource version to look up. Defaults to \code{NULL}.
#' @param geneset_name A geneset name to look up. Defaults to \code{NULL}.
#' @param verbose Logical; whether to print diagnostic messages.
#' Defaults to \code{TRUE}.
#'
#' @export
searchGenesetEntry <- function(
    conn_handler = NULL,
    geneset_resource_id = NULL,
    source = NULL,
    species = NULL,
    collection = NULL,
    subcollection = NULL,
    version = NULL,
    geneset_name = NULL,
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

  entry_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "geneset_entries",
    return_var = "*",
    check_db_table = TRUE
  )

  resource_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "geneset_resources",
    return_var = c(
      "geneset_resource_id",
      "source",
      "species",
      "collection",
      "subcollection",
      "version",
      "source_version",
      "format",
      "storage_path",
      "checksum",
      "n_genesets",
      "n_features",
      "is_current"
    ),
    check_db_table = TRUE
  )

  tbl <- entry_tbl |>
    dplyr::left_join(resource_tbl, by = "geneset_resource_id")

  filter_var_list <- base::list(
    geneset_resource_id = base::unique(geneset_resource_id),
    source = base::unique(source),
    species = base::unique(species),
    collection = base::unique(collection),
    subcollection = base::unique(subcollection),
    version = base::unique(version),
    geneset_name = base::unique(geneset_name)
  )

  for (r in base::seq_along(filter_var_list)) {
    filter_status <- base::ifelse(
      base::length(filter_var_list[[r]]) == 0 || base::all(filter_var_list[[r]] %in% c("", NA)),
      FALSE,
      TRUE
    )

    if (filter_status == TRUE) {
      filter_var <- base::names(filter_var_list)[r]
      filter_val <- filter_var_list[[r]][base::which(!filter_var_list[[r]] %in% c(NA, ""))]

      if (filter_var %in% c("geneset_resource_id")) {
        tbl <- tbl |>
          dplyr::filter(!!rlang::sym(filter_var) %in% filter_val)
      } else {
        tbl[[filter_var]] <- base::ifelse(base::is.na(tbl[[filter_var]]), "", tbl[[filter_var]])
        tbl <- tbl |>
          dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
      }
    }
  }

  base::suppressWarnings(DBI::dbDisconnect(conn))

  tbl
}
