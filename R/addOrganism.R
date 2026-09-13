#' @title addOrganism
#' @description Add organisms to database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param organism_tbl A Data Frame; Must contain the following column names: 
#' organism, biomart_db, biomart_dataset, biomart_description, biomart_version, 
#' biomart_updated_date, prot_organism_code, prot_organism_taxid, prot_updated_date (required) 
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Create organisms table
#' organism_tbl <- base::data.frame(
#'   organism = "Homo sapiens",
#'   biomart_db = "genes", 
#'   biomart_dataset = "hsapiens_gene_ensembl", 
#'   biomart_description = "Human genes (GRCh38.p14)", 
#'   biomart_version = 114, 
#'   biomart_updated_date = base::as.Date(base::Sys.Date(), format = "%Y-%m-%d"),
#'   prot_organism_code = "Human", 
#'   prot_organism_taxid = 9606,
#'   prot_updated_date = base::as.Date(base::Sys.Date(), format = "%Y-%m-%d")
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
#' # Add organisms to database
#' SigRepo::addOrganism(
#'   conn_handler = conn_handler,
#'   organism_tbl = organism_tbl,
#'   verbose = TRUE
#' )
#' 
#'}
#'    
#' @export
addOrganism <- function(
    conn_handler = NULL,
    organism_tbl,
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
  required_column_fields <- "organism"
  db_table_name <- "organisms"
  table <- organism_tbl
  
  # Check required column fields
  if(base::any(!required_column_fields %in% base::colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop(base::sprintf("\n'Organisms' table is missing the following required column names: %s.\n", base::paste0(required_column_fields[base::which(!required_column_fields %in% base::colnames(table))], collapse = ", ")))
  }
  
  # Make sure required column fields do not have any empty values ####
  if(base::any(base::is.na(table[,required_column_fields]) == TRUE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop(base::sprintf("\nAll required column names in 'organisms' table: %s cannot contain any empty values.\n", base::paste0(required_column_fields, collapse = ", ")))
  }
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = c("organism_id", "biomart_db", "biomart_dataset", "biomart_description", "biomart_version", "biomart_updated_date", "prot_organism_code", "prot_organism_taxid", "prot_updated_date"),
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn, 
    db_table_name = db_table_name,
    table = table,
    coln_var = "organism",
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



