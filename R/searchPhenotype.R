#' @title searchPhenotype
#' @description Search for a phenotype in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param phenotype A phenotype, or a  list of phenotypes to search by. 
#' Default is NULL which will return all of the phenotypes in the database
#' @param verbose Logical; whether or not to print the
#' diagnostic messages. Defaults to 'TRUE'.
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
#' # Search for a list of phenotypes in the database
#' SigRepo::searchPhenotype(
#'   conn_handler = conn_handler,
#'   phenotype = "test_phenotype",
#'   verbose = TRUE
#' )
#' 
#' }
#' 
#' @export
searchPhenotype <- function(
    conn_handler = NULL,
    phenotype = NULL,
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
  if(base::length(phenotype) == 0 || base::all(phenotype %in% c("", NA))){
    
    phenotype_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "phenotypes", 
      return_var = "phenotype", 
      check_db_table = TRUE
    )  
    
  }else{
    
    phenotype_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "phenotypes", 
      return_var = "phenotype", 
      filter_coln_var = "phenotype", 
      filter_coln_val = base::list("phenotype" = base::unique(phenotype)),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return tabl
  return(phenotype_tbl)

}







