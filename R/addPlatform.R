#' @title addPlatform
#' @description Add platforms to database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param platform_tbl A Data Frame; Must contain the following column names:
#' platform_name (required)
#' @param verbose Logical; whether to print diagnostic messages. 
#' Defaults to 'TRUE'.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Create platforms table
#' platform_tbl <- base::data.frame(
#'   platform_name = c("platform_1", "platform_2")
#' )
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
#' # Add platforms to database
#' SigRepo::addPlatform(
#'   conn_handler = conn_handler,
#'   platform_tbl = platform_tbl,
#'   verbose = TRUE
#' )
#' 
#' }
#' 
#' @export
addPlatform <- function(
    conn_handler = NULL,
    platform_tbl,
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
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Create a list of variables to check database ####
  required_column_fields <- "platform_name"
  db_table_name <- "platforms"
  table <- platform_tbl
  
  # Check required column fields
  if(base::any(!required_column_fields %in% base::colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop(base::sprintf("\n'Platforms' table is missing the following required column names: %s.\n", base::paste0(required_column_fields[base::which(!required_column_fields %in% base::colnames(table))], collapse = ", ")))
  }
  
  # Make sure required column fields do not have any empty values ####
  if(base::any(base::is.na(table[,required_column_fields]) == TRUE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop(base::sprintf("All required column names in 'platforms' table: %s cannot contain any empty values.\n", base::paste0(required_column_fields, collapse = ", ")))
  }
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    check_db_table = TRUE,
    exclude_coln_names = 'platform_id'
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn, 
    db_table_name = db_table_name,
    table = table,
    coln_var = "platform_name",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  ) 

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return message
  SigRepo::verbose("Finished uploading.\n")
  
}



