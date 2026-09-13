#' @title addSampleType
#' @description Add sample types to database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param sample_type_tbl A Data Frame; must contain the following column names:
#' sample_type, brenda_accession (required)
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Create sample types table
#' sample_type_tbl <- base::data.frame(
#'   sample_type = c("sample_1","sample_2"),
#'   brenda_accession = c("accession_1", "accession_2")
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
#' # Add sample types to database
#' SigRepo::addSampleType(
#'   conn_handler = conn_handler,
#'   sample_type_tbl = sample_type_tbl,
#'   verbose = FALSE
#' )
#' 
#' }
#' 
#' @export
addSampleType <- function(
    conn_handler = NULL,
    sample_type_tbl,
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
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  required_column_fields <- "sample_type"
  db_table_name <- "sample_types"
  table <- sample_type_tbl
  
  # Check required column fields
  if(base::any(!required_column_fields %in% base::colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("'Sample types' table is missing the following required column names: %s.\n", base::paste0(required_column_fields[base::which(!required_column_fields %in% base::colnames(table))], collapse = ", ")))
  }
  
  # Make sure required column fields do not have any empty values ####
  if(base::any(base::is.na(table[,required_column_fields]) == TRUE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("All required column names in 'sample types' table: %s cannot contain any empty values.\n", base::paste0(required_column_fields, collapse = ", ")))
  }
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "sample_type_id",
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn, 
    db_table_name = db_table_name,
    table = table,
    coln_var = "sample_type",
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



