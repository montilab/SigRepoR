#' @title addSignatureToCollection
#' @description Add a list of signatures to a collection in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param collection_id Database ID of the OmicSignature Collection (required)
#' @param signature_id A single signature database ID, or a list, to be added to a collection 
#' in the database (required)
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#' 
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
#' # Add a list of signatures to a collection
#' SigRepo::addSignatureToCollection(
#'   conn_handler = conn_handler,
#'   collection_id = 1,
#'   signature_id = c(1, 67, 80, 5),
#'   verbose = FALSE
#' )
#' 
#' }
#' 
#' 
#' @export
addSignatureToCollection <- function(
    conn_handler = NULL,
    collection_id,
    signature_id,
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
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
  # Get unique collection id
  collection_id <- base::unique(collection_id) 

  # Get unique signature id
  signature_id <- base::unique(signature_id) 
  
  # Check collection_id
  if(!base::length(collection_id) == 1 || base::all(collection_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("\n'collection_id' must have a length of 1 and cannot be empty.\n")
  }
  
  # Check signature_id
  if(base::length(signature_id) == 0 || base::all(signature_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("\n'signature_id' cannot be empty.\n")
  }
  
  # Check if signature exists in the signatures table of the database
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = c("signature_id"),
    filter_coln_val = base::list("signature_id" = signature_id),
    filter_var_by = "AND",
    check_db_table = FALSE
  )
  
  # If any signatures does not exit in the database, throw an error message
  if(base::nrow(signature_tbl) != base::length(signature_id)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("\nsignature_id = %s do(es) not exist in the SigRepo database.\n", base::paste0("'", signature_id[base::which(!signature_id %in% signature_tbl$signature_id)], "'", collapse = ", ")))
  }
  
  # If user is not admin, check if user has access to the signature as owner or editor
  if(user_role != "admin"){
    
    # Check if user is the one who uploaded the signature
    signature_user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = c("signature_id", "user_name"), 
      filter_coln_val = base::list("signature_id" = signature_id, "user_name" = user_name),
      filter_var_by = "AND",
      check_db_table = FALSE
    )
    
    # If not, check if user was added as an owner or editor
    if(base::nrow(signature_user_tbl) == 0 || (base::nrow(signature_user_tbl) != base::length(unique(signature_id)))){
      
      signature_access_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_access",
        return_var = "*",
        filter_coln_var = c("signature_id", "user_name", "access_type"),
        filter_coln_val = base::list("signature_id" = signature_id, "user_name" = user_name, "access_type" = c("owner", "editor")),
        filter_var_by = c("AND", "AND"),
        check_db_table = TRUE
      )
      
      # If user does not have permission, throw an error message
      if(base::nrow(signature_access_tbl) == 0 || (base::nrow(signature_access_tbl) != base::length(unique(signature_id)))){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn)) 
        # Show message
        base::stop(base::sprintf("\nUser = '%s' does not have the permission to add signature_id = %s to collection_id = '%s' in the SigRepo database.\n", user_name, base::paste0("'", signature_id[base::which(!signature_id %in% signature_access_tbl$signature_id)], "'", collapse = ", "), collection_id))
      }
    }
  }
  
  # Check if collection exists ####
  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "collection",
    return_var = "*",
    filter_coln_var = "collection_id",
    filter_coln_val = list("collection_id" = collection_id),
    check_db_table = TRUE
  )
  
  # If collection not exists, throw an error message
  if(base::nrow(collection_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("\nThere is no collection_id = '%s' in the 'collection' table of the SigRepo database.\n", collection_id))
    
  }else{
    
    # If user is not admin, check if user has access to the collection as owner or editor
    if(user_role != "admin"){
      
      # Check if user is the one who uploaded the collection
      collection_user_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "collection",
        return_var = "*",
        filter_coln_var = c("collection_id", "user_name"), 
        filter_coln_val = base::list("collection_id" = collection_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(base::nrow(collection_user_tbl) == 0){
        
        signature_access_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "collection_access",
          return_var = "*",
          filter_coln_var = c("collection_id", "user_name", "access_type"),
          filter_coln_val = base::list("collection_id" = collection_id, "user_name" = user_name, "access_type" = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user does not have permission, throw an error message
        if(base::nrow(signature_access_tbl) == 0){
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn)) 
          # Show message
          base::stop(base::sprintf("\nUser = '%s' does not have permission to add signature_id = %s to collection_id = '%s' in the SigRepo database.\n", user_name, base::paste0("'", signature_id, "'", collapse = ", "), collection_id))
        }
      }
    }
    
    # Create user collection access table
    table <- base::data.frame(
      collection_id = collection_id,
      signature_id = signature_id,
      stringsAsFactors = FALSE
    )
    
    # Create a hash key to look up values in database ####
    table <- SigRepo::createHashKey(
      table = table,
      hash_var = "signature_collection_hashkey",
      hash_columns = c("collection_id", "signature_id"),
      hash_method = "md5"
    )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn,
      db_table_name = 'signature_collection_access',
      table = table, 
      check_db_table = TRUE
    )
    
    # Remove duplicates from table before inserting into database ####
    table <- SigRepo::removeDuplicates(
      conn = conn,
      db_table_name = "signature_collection_access",
      table = table,
      coln_var = "signature_collection_hashkey",
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = "signature_collection_access", 
      table = table,
      check_db_table = FALSE
    )

    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return 
    return(base::invisible())

  }  
}  


