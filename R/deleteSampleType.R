#' @title deleteSampleType
#' @description Remove sample types from database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param sample_type A list of sample types to be removed (required)
#' @param verbose Logical; whether to print diagnostic messages. 
#' Defaults to 'TRUE'.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Create a connection handler
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", 
#'   host = "sigrepo.org", 
#'   port = 3306, 
#'   user = "your_username", 
#'   password = "your_password"
#' )
#' 
#' # Delete a list of sample types from database
#' SigRepo::deleteSampleType(
#'   conn_handler = conn_handler,
#'   sample_type = "3T3-L1 cell",
#'   verbose = TRUE
#' )
#' 
#' }
#' 
#' @export
deleteSampleType <- function(
    conn_handler = NULL,
    sample_type,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "DELETE",
    required_role = "admin"
  )
  
  # Delete specific sample types from the database ####
  SigRepo::delete_table_sql(
    conn = conn,
    db_table_name = "sample_types",
    delete_coln_var = "sample_type",
    delete_coln_val = base::unique(sample_type),
    check_db_table = TRUE
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))  
  
  # Return message
  SigRepo::verbose("Finished deleting.\n")
  
}



