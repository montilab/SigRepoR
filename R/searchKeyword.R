#' @title searchKeyword
#' @description Search for platform in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param keyword A list of keywords to be looked up. 
#' Default is NULL which will return all of the keywords in the database.
#' @param verbose Logical; whether or not to print the diagnostic messages. 
#' Default to 'TRUE'.
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
#' # Search for a list of keywords in the database
#' SigRepo::searchKeyword(
#'   conn_handler = conn_handler, 
#'   keyword = "test_keyword",
#'   verbose = TRUE
#' )
#' 
#' }
#' 
#' @export
searchKeyword <- function(
    conn_handler = NULL,
    keyword = NULL,
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
  
  # Look up keyword
  if(base::length(keyword) == 0 || base::all(keyword %in% c("", NA))){
    
    keyword_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "keywords", 
      return_var = "keyword", 
      check_db_table = TRUE
    )  
    
  }else{
    
    keyword_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "keywords", 
      return_var = "keyword", 
      filter_coln_var = "keyword", 
      filter_coln_val = base::list("keyword" = base::unique(keyword)),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(keyword_tbl)

}







