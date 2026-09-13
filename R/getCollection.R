#' @title getCollection
#' @description Get a list of collection uploaded by a specified user in the database.
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param collection_name Name of collection to be returned (required)
#' @param collection_id Database ID of collection to be returned (required)
#' @param verbose Logical; whether or not to print the diagnostic messages. 
#' Default is \code{TRUE}.
#'  
#' @export
getCollection <- function(
    conn_handler = NULL,
    collection_name = NULL,
    collection_id = NULL,
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
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
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
    "collection_name" = base::unique(collection_name)
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
  
  # If user_role is not admin, check if user has the permission to access the collection ####
  if(user_role != "admin"){
    
    # Get a list of collection with visibility = FALSE
    collection_visibility <- collection_tbl |> dplyr::filter(.data$visibility == FALSE) |> dplyr::distinct(.data$collection_id, .data$visibility) 
    
    # Check if user has the permission to view the signatures ####
    for(w in 1:base::nrow(collection_visibility)){
      #w=1;
      # Check user access ####
      collection_access_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "collection_access", 
        return_var = "*", 
        filter_coln_var = c("collection_id", "user_name", "access_type"),
        filter_coln_val = base::list("collection_id" = collection_visibility$collection_id[w], "user_name" = user_name, "access_type" = c("owner", "editor", "viewer")),
        filter_var_by = c("AND", "AND"),
        check_db_table = TRUE
      ) 
      # If user does not have owner or editor permission, remove from the returned list
      if(base::nrow(collection_access_tbl) == 0){
        collection_tbl <- collection_tbl |> dplyr::filter(!.data$collection_id %in% collection_visibility$collection_id[w])
      }
    }
    
  }
  
  # Check if signature exists
  if(base::nrow(collection_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop("\nThere are no collection returned from the search parameters.\n")
    
  }else{
    
    # Create a place holder to store collection
    omic_collection_list <- base::list()
    
    # Create an omic signature object for each signature id ####
    for(r in 1:nrow(collection_tbl)){
      #r=1;
      db_collection_tbl <- collection_tbl |> dplyr::slice(r)
      
      # Create an OmicSignature object
      omic_signature_collection <- SigRepo::createOmicCollection(
        conn_handler = conn_handler,
        db_collection_tbl = db_collection_tbl
      )
      
      # Append OmicSignature object to overall list
      omic_collection_list <- c(
        omic_collection_list, 
        omic_signature_collection
      )
    }
    
    # Add names to signatures
    base::names(omic_collection_list) <- collection_tbl$collection_name
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))
    
    # Return table
    return(omic_collection_list)
    
  }  
}


