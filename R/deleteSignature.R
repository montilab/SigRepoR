#' @title deleteSignature
#' @description Delete a signature from the signatures table of the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param signature_id Database ID of signature to be removed from the database (required)
#' @param verbose Logical; whether or not to print the diagnostic messages. 
#' Default is \code{TRUE}.
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
#' # Delete signature from database
#' SigRepo::deleteSignature(
#'   conn_handler = conn_handler,
#'   signature_id = 56,
#'   verbose = TRUE
#' )
#' 
#' }
#' 
#' @export
deleteSignature <- function(
    conn_handler = NULL, 
    signature_id = NULL,
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
    action_type = "DELETE",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
  # Get unique signature id
  signature_id <- base::unique(signature_id) 
  
  # Check signature_id
  if(!base::length(signature_id) == 1 || base::all(signature_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'signature_id' must have a length of 1 and cannot be empty.\n")
  }
  
  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = "signature_id",
    filter_coln_val = base::list("signature_id" = signature_id),
    check_db_table = TRUE
  )
  
  # If signature exists, return the signature table else throw an error message
  if(base::nrow(signature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("There is no signature_id = '%s' existed in the 'signatures' table of the SigRepo database.\n", signature_id))
    
  }else{
    
    # If user is not admin, check if user has access to signature
    if(user_role != "admin"){
      
      # Check if user is the one who uploaded the signature
      signature_user_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name  = "signatures",
        return_var = "*",
        filter_coln_var = c("signature_id", "user_name"), 
        filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(base::nrow(signature_user_tbl) == 0){
        
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
        if(base::nrow(signature_access_tbl) == 0){
          
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn)) 
          
          # Show message
          base::stop(base::sprintf("User = '%s' does not have permission to delete signature_id = '%s' from the SigRepo database.\n", user_name, signature_id))
          
        }
      }
    }
    
    # If signature has difexp, remove it
    if(signature_tbl$has_difexp[1] == 1){
      # Return message
      SigRepo::verbose(base::sprintf("Remove difexp belongs to signature_id = '%s' from the database.\n", signature_id))
      # Get API URL
      api_url <- SigRepo::build_api_url(
        conn_handler = conn_handler,
        endpoint = "delete_difexp",
        query = base::list(
          api_key = conn_info$api_key[1],
          signature_hashkey = signature_tbl$signature_hashkey[1]
        )
      )
      # Delete difexp from database
      res <- httr::DELETE(url = api_url)
      # Check status code
      if(res$status_code != 200){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))
        # Show message
        SigRepo::stop_for_api_error(
          res = res,
          api_url = api_url,
          action = "delete the difexp table from the SigRepo API"
        )
      }
    }
    
    # Return message
    SigRepo::verbose(base::sprintf("Remove signature_id = '%s' from 'signatures' table of the database.\n", signature_id))
    
    # Delete signature from signatures table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "signatures",
      delete_coln_var = "signature_id",
      delete_coln_val = signature_id,
      check_db_table = FALSE
    )
    
    # Return message
    SigRepo::verbose(base::sprintf("Remove features belongs to signature_id = '%s' from 'signature_feature_set' table of the database.\n", signature_id))
    
    # Delete signature from signature_feature_set table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "signature_feature_set",
      delete_coln_var = "signature_id",
      delete_coln_val = signature_id,
      check_db_table = TRUE
    )
    
    # Return message
    SigRepo::verbose(base::sprintf("Remove user access to signature_id = '%s' from 'signature_access' table of the database.\n", signature_id))
    
    # Delete user from signature_access table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "signature_access",
      delete_coln_var = "signature_id",
      delete_coln_val = signature_id,
      check_db_table = TRUE
    )
    
    # Return message
    SigRepo::verbose(base::sprintf("Remove signature_id = '%s' from 'signature_collection_access' table of the database.\n", signature_id))
    
    # Delete user from signature_access table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "signature_collection_access",
      delete_coln_var = "signature_id",
      delete_coln_val = signature_id,
      check_db_table = TRUE
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return message
    SigRepo::verbose(base::sprintf("signature_id = '%s' has been removed.\n", signature_id))
    
  } 
}
