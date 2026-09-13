#' @title searchGenesetResource
#' @description Search geneset resource metadata in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param source A geneset source to look up. Defaults to \code{NULL}.
#' @param species A species to look up. Defaults to \code{NULL}.
#' @param collection A geneset collection to look up. Defaults to \code{NULL}.
#' @param subcollection A geneset subcollection to look up. Defaults to \code{NULL}.
#' @param version A geneset resource version to look up. Defaults to \code{NULL}.
#' @param is_current Logical or integer filter for current resources. Defaults to \code{NULL}.
#' @param verbose Logical; whether to print diagnostic messages.
#' Defaults to \code{TRUE}.
#'
#' @export
searchGenesetResource <- function(
    conn_handler = NULL,
    source = NULL,
    species = NULL,
    collection = NULL,
    subcollection = NULL,
    version = NULL,
    is_current = NULL,
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

  tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "geneset_resources",
    return_var = "*",
    check_db_table = TRUE
  )

  filter_var_list <- base::list(
    source = base::unique(source),
    species = base::unique(species),
    collection = base::unique(collection),
    subcollection = base::unique(subcollection),
    version = base::unique(version)
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

      tbl[[filter_var]] <- base::ifelse(base::is.na(tbl[[filter_var]]), "", tbl[[filter_var]])
      tbl <- tbl |>
        dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }

  if (base::length(is_current) > 0 && !base::all(is_current %in% c("", NA))) {
    tbl <- tbl |>
      dplyr::filter(.data$is_current %in% base::as.integer(is_current))
  }

  base::suppressWarnings(DBI::dbDisconnect(conn))

  tbl
}
