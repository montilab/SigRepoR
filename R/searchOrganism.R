#' @title searchOrganism
#' @description Search for an organism in the database, can handle a singular organism or list 
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param organism an organism, or  list of organisms to search by. 
#' Default is NULL which will return all of the organisms in the database.
#' @param verbose Logical; whether or not to print the diagnostic messages. 
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
#' # Search for a list of organims in the database 
#' SigRepo::searchOrganism(
#'   conn_handler = conn_handler,
#'   organism = "Mus musculus",
#'   verbose = TRUE
#' )
#' 
#' }
#'     
#'
#' @export
searchOrganism <- function(
    conn_handler = NULL,
    organism = NULL,
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
    required_role = "viewer"
  )
  
  # Look up signatures
  if(base::length(organism) == 0 || base::all(organism %in% c("", NA))){
    
    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = "*", 
      exclude_return_var = "organism_id",
      check_db_table = TRUE
    )  
    
  }else{
    
    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = "*", 
      exclude_return_var = "organism_id",
      filter_coln_var = "organism", 
      filter_coln_val = base::list("organism" = base::unique(organism)),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(organism_tbl)

}







