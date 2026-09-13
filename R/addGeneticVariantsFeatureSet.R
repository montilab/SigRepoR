#' @title addGeneticVariantsFeatureSet
#' @description Add GeneticVariants feature set to database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param feature_set A Data Frame; must contain the following column names: 
#' feature_name, organism, gene_symbol, is_current, version (required)
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#' 
#' 
#' @keywords internal
#' 
#' @export
addGeneticVariantsFeatureSet <- function(
    conn_handler = NULL,
    feature_set,
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
  required_column_fields <- c("feature_name", "organism", "is_current")
  db_table_name <- "genetic_variants_features"
  table <- feature_set
  
  # Check required column fields
  if(base::any(!required_column_fields %in% base::colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop(base::sprintf("\n'GeneticVariants features' table is missing the following required column names: %s.\n", base::paste0(required_column_fields[which(!required_column_fields %in% base::colnames(table))], collapse = ", ")))
  }
  
  # Make sure required column fields do not have any empty values ####
  if(base::any(base::is.na(table[,required_column_fields]) == TRUE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop(base::sprintf("\nAll required column names in 'GeneticVariants features' table: %s cannot contain any empty values.\n", base::paste0(required_column_fields, collapse = ", ")))
  }
  
  # Get organism id ####
  coln_var <- "organism"
  coln_var_id <- "organism_id"
  
  # Look up table
  lookup_id_tbl <- SigRepo::getVariableID(
    conn = conn,
    db_table_name = "organisms",
    table = table,
    coln_var = coln_var, 
    coln_var_id = coln_var_id,
    check_db_table = TRUE
  )
  
  ## Add ID to table
  table <- table |> dplyr::mutate(id = base::trimws(base::tolower(!!!rlang::syms(coln_var)))) |> 
    dplyr::left_join(
      lookup_id_tbl |> dplyr::mutate(id = base::trimws(base::tolower(!!!rlang::syms(coln_var)))) |> dplyr::select(c("id", "organism_id")), 
      by = "id"
    )
  
  # If any ID is missing, produce an error message
  if(base::any(table$organism_id %in% c("", NA))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    
    # Get the unknown values
    unknown_values <- table$organism[base::which(table$organism_id %in% c("", NA))]
    
    # Return error message
    SigRepo::showOrganismErrorMessage(
      db_table_name = 'organisms',
      unknown_values = unknown_values
    )
    # Return a list of unknown values
    return(base::data.frame(table = "organisms", "unknown_values" = unknown_values))
  }
  
  # Create a hash key to look up values in database ####
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "feature_hashkey",
    hash_columns = c("feature_name", "organism_id"),
    hash_method = "md5"
  )
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = c("feature_id", "version","chromosome", "position", "annotation"),
    check_db_table = FALSE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "feature_hashkey",
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


