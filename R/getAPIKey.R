#' @title getAPIKey
#' @description Get API Key of a specific user in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param verbose Logical; whether or not to print the diagnostic messages. 
#' Default is \code{TRUE}.
#' 
#' @export
getAPIKey <- function(
    conn_handler = NULL,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)

  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "editor"
  )
  
  # Look up api key
  api_key_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "users", 
    return_var = c("user_name", "api_key"), 
    filter_coln_var = "user_name", 
    filter_coln_val = base::list("user_name" = conn_info$user), 
    check_db_table = TRUE
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return table
  return(api_key_tbl)
  
}



