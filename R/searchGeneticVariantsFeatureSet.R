#' @title searchGeneticVariantsFeatureSet
#' @description Search for a list of GeneticVariants features in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param feature_name A list of feature names to look up. 
#' Defaults to 'NULL',  which will return all of the feature names in the database.
#' @param organism A list of organism to look up.
#' Default to 'NULL' which will return all of the organisms in the database.
#' @param verbose Logical; whether or not to print the
#' diagnostic messages. Defaults to 'TRUE'.
#' @examples
#' \dontrun{
#' SigRepo::searchGeneticVariantsFeatureSet(conn_handler = conn_handler,
#'                                     feature_name = "test_feature",
#'                                     organism = "Homo sapiens"
#'                                     )
#' }
#' 
#' 
#' @export
searchGeneticVariantsFeatureSet <- function(
    conn_handler = NULL,
    feature_name = NULL,
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
  
  # Get reference table
  ref_table <- "genetic_variants_features"
  
  # Look up features by organism
  if(base::length(feature_name) == 0 || base::all(feature_name %in% c("", NA))){
    
    feature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      exclude_return_var = "feature_id",
      check_db_table = TRUE
    )  
    
  }else{
    
    feature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      exclude_return_var = "feature_id",
      filter_coln_var = "feature_name",
      filter_coln_val = base::list("feature_name" = base::unique(feature_name)),
      check_db_table = TRUE
    ) 
    
  }
  
  # Check if feature exists
  if(base::nrow(feature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop("\nThere are no features returned from the search parameters.\n")
    
  }else{
    
    # look up organism
    lookup_organism_id <- base::unique(feature_tbl$organism_id)
    
    # Look up organism id
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = c("organism_id", "organism"), 
      filter_coln_var = "organism_id", 
      filter_coln_val = base::list("organism_id" = lookup_organism_id),
      check_db_table = TRUE
    )  
    
    # Add variables to table
    feature_tbl <- feature_tbl |> 
      dplyr::left_join(organism_id_tbl, by = "organism_id") 
    
    # Rename table with appropriate column names
    coln_names <- base::colnames(feature_tbl) |> 
      base::replace(base::match("organism_id", base::colnames(feature_tbl)), "organism")
    
    # Extract the table with appropriate column names ####
    feature_tbl <- feature_tbl |> dplyr::select(dplyr::all_of(coln_names))
    
    # Get a list of filtered variables
    filter_var_list <- base::list(
      "organism" = base::unique(organism)
    )
    
    # Filter table with given search variables
    for(r in base::seq_along(filter_var_list)){
      #r=1;
      filter_status <- base::ifelse(base::length(filter_var_list[[r]]) == 0 || base::all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
      if(filter_status == TRUE){
        filter_var <- base::names(filter_var_list)[r]
        filter_val <- filter_var_list[[r]][base::which(!filter_var_list[[r]] %in% c(NA, ""))]
        feature_tbl <- feature_tbl |> dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
      }
    }
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return table
    return(feature_tbl)
    
  }
}







