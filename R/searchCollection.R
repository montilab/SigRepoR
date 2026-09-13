#' @title searchCollection
#' @description Search for a list of collection in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param collection_name Name of collection to be looked up by.
#' @param collection_id Database ID of collection to be looked up by.
#' @param signature_name Signature name of the signatures to be looked up by.
#' @param signature_id Database ID of the signatures to be looked up by.
#' @param user_name Name of users that the collection belongs to.
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
#' # Delete a list of organisms from database
#' SigRepo::searchCollection(
#'   conn_handler = conn_handler, 
#'   collection_name = "test_collection", 
#'   collection_id = "67"
#' )
#' 
#' }
#'  
#' 
#' @export
searchCollection <- function(
    conn_handler = NULL,
    collection_id = NULL,
    collection_name = NULL,
    signature_name = NULL,
    signature_id = NULL,
    user_name = NULL,
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
  
  # Look up collection
  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "collection", 
    return_var = "*", 
    check_db_table = TRUE
  ) 
  
  # Get a list of filtered variables
  filter_var_list <- base::list(
    "collection_id" = base::unique(collection_id),
    "collection_name" = base::unique(collection_name),
    "user_name" = base::unique(user_name)
  )
  
  # Filter table with given search variables
  for(r in base::seq_along(filter_var_list)){
    #r=1;
    filter_status <- base::ifelse(base::length(filter_var_list[[r]]) == 0 || base::all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
    if(filter_status == TRUE){
      filter_var <- base::names(filter_var_list)[r]
      filter_val <- filter_var_list[[r]][base::which(!filter_var_list[[r]] %in% c(NA, ""))]
      collection_tbl <- collection_tbl |> dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }
  
  # Check if signature exists
  if(base::nrow(collection_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    
    # Show message
    SigRepo::verbose("There is no collection returned from the search parameters.\n")
    
    # Return NULL
    return(base::data.frame(NULL))
    
  }else{
    
    # Look up signatures in the signature collection table
    signature_collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signature_collection_access", 
      return_var = "*", 
      filter_coln_var = "collection_id", 
      filter_coln_val = base::list("collection_id" = base::unique(collection_tbl$collection_id)),
      check_db_table = TRUE
    ) 
    
    # Add signatures to collection table
    collection_tbl <- collection_tbl |> dplyr::left_join(signature_collection_tbl, by = "collection_id")  
    
    # Look up name of the signatures in the signature table
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = c("signature_id", "signature_name"),
      filter_coln_var = "signature_id", 
      filter_coln_val = base::list("signature_id" = base::unique(collection_tbl$signature_id)),
      check_db_table = TRUE
    ) 
    
    # Add name of the signatures to collection table
    collection_tbl <- collection_tbl |> dplyr::left_join(signature_tbl, by = "signature_id")
    
    # Get a list of filtered variables
    filter_var_list <- base::list(
      "signature_id" = base::unique(signature_id),
      "signature_name" = base::unique(signature_name)
    )
    
    # Filter table with given search variables
    for(r in base::seq_along(filter_var_list)){
      #r=1;
      filter_status <- base::ifelse(length(filter_var_list[[r]]) == 0 || base::all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
      if(filter_status == TRUE){
        filter_var <- base::names(filter_var_list)[r]
        filter_val <- filter_var_list[[r]][base::which(!filter_var_list[[r]] %in% c(NA, ""))]
        collection_tbl <- collection_tbl |> dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
      }
    }
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))
    
    # Return table
    return(collection_tbl)
    
  }
}







