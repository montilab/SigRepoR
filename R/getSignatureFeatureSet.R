#' @title getSignatureFeatureSet
#' @description Get raw rows from the \code{signature_feature_set} table for one
#' or more signatures in the database.
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param signature_name Name of signature to be returned.
#' @param signature_id Database ID of signature to be returned.
#' @param verbose Logical; whether or not to print the diagnostic messages.
#' Default is \code{TRUE}.
#'
#' @export
#' @examples
#' \dontrun{
#' SigRepo::getSignatureFeatureSet(
#'   conn_handler = conn_handler,
#'   signature_id = 56,
#'   verbose = TRUE
#' )
#' }
getSignatureFeatureSet <- function(
    conn_handler = NULL,
    signature_name = NULL,
    signature_id = NULL,
    verbose = TRUE
){

  SigRepo::print_messages(verbose = verbose)

  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)

  conn_info <- SigRepo::checkPermissions(
    conn = conn,
    action_type = "SELECT",
    required_role = "viewer"
  )

  user_role <- conn_info$user_role[1]
  user_name <- conn_info$user[1]

  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = c("signature_id", "signature_name", "visibility"),
    check_db_table = TRUE
  )

  filter_var_list <- base::list(
    "signature_id" = base::unique(signature_id),
    "signature_name" = base::unique(signature_name)
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
      signature_tbl <- signature_tbl |>
        dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }

  if (user_role != "admin" && base::nrow(signature_tbl) > 0) {
    private_signatures <- signature_tbl |>
      dplyr::filter(.data$visibility == FALSE) |>
      dplyr::distinct(.data$signature_id)

    if (base::nrow(private_signatures) > 0) {
      accessible_private_ids <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_access",
        return_var = c("signature_id", "user_name", "access_type"),
        filter_coln_var = c("signature_id", "user_name", "access_type"),
        filter_coln_val = list(
          "signature_id" = private_signatures$signature_id,
          "user_name" = user_name,
          "access_type" = c("owner", "editor", "viewer")
        ),
        filter_var_by = c("AND", "AND"),
        check_db_table = TRUE
      ) |>
        dplyr::distinct(.data$signature_id)

      signature_tbl <- signature_tbl |>
        dplyr::filter(
          .data$visibility == TRUE |
            .data$signature_id %in% accessible_private_ids$signature_id
        )
    }
  }

  if (base::nrow(signature_tbl) == 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    SigRepo::verbose("There are no signature feature set rows returned from the search parameters.\n")
    return(NULL)
  }

  signature_feature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signature_feature_set",
    return_var = "*",
    filter_coln_var = "signature_id",
    filter_coln_val = base::list("signature_id" = base::unique(signature_tbl$signature_id)),
    check_db_table = TRUE
  )

  base::suppressWarnings(DBI::dbDisconnect(conn))

  signature_feature_tbl

}
