#' @title deleteOrganism
#' @description Remove organisms from database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param organism A list of organisms to be removed (required) 
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
#' # Delete a list of organisms from database
#' SigRepo::deleteOrganism(
#'   conn_handler = conn_handler,
#'   organism = "homo sapiens",
#'   verbose = TRUE
#' )
#' 
#' }
#' 
#' @export
deleteOrganism <- function(
    conn_handler = NULL,
    organism,
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
  
  # Delete specific organisms from the database ####
  SigRepo::delete_table_sql(
    conn = conn,
    db_table_name = "organisms",
    delete_coln_var = "organism",
    delete_coln_val = base::unique(organism),
    check_db_table = TRUE
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return message
  SigRepo::verbose("Finished deleting.\n")
  
}



