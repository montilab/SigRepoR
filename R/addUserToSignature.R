#' @title addUserToSignature
#' @description Add a list of users with specific access to a signature 
#' in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param signature_id Database ID of a signature (required)
#' @param user_name A list of users to be added to a signature (required)
#' @param access_type A list of permissions to be given to users in order for 
#' them to view or manage the signature in the database (required). 
#' 
#' There are three types of permissions:
#' 
#' \code{owner} has Read and Write access to their own uploaded signatures.
#' \code{editor} has Read and Write access to signatures that other users were 
#' given them access to.
#' \code{viewer} has ONLY Read access to signatures that other users were 
#' given them access to.
#' @param verbose Logical; whether to print diagnostic messages. 
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
#' # Add users to signature
#' SigRepo::addUserToSignature(
#'   conn_handler = conn_handler,
#'   collection_id = 67,
#'   user_name = "john smith",
#'   access_type = "editor"
#' )
#' 
#' }
#' 
#' @export
addUserToSignature <- function(
    conn_handler = NULL,
    signature_id,
    user_name,
    access_type = c("owner", "editor", "viewer"),
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
  orig_user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  orig_user_name <- conn_info$user[1]
  
  # Check access_type for each user
  access_type <- base::match.arg(access_type, several.ok = FALSE)  
  
  # Check signature_id
  if(!base::length(signature_id) == 1 || base::all(signature_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("\n'signature_id' must have a length of 1 and cannot be empty.\n")
  }
  
  # Check user_name
  if(base::length(user_name) == 0 || base::all(user_name %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("\n'user_name' cannot be empty.\n")
  }
  
  # Make sure length of user_name equal to length of its access_type
  if(base::length(user_name) != base::length(access_type)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("\nLength of 'user_name' must equal the length of its 'access_type'.\n")
  }
  
  # Check if user exist in the users table of the database
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "users",
    return_var = "*",
    filter_coln_var = "user_name", 
    filter_coln_val = base::list("user_name" = base::unique(user_name)),
    check_db_table = FALSE
  )
  
  # If any user does not exit in the database, throw an error message
  if(base::nrow(user_tbl) != base::length(base::unique(user_name))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("\nUser = %s do(es) not exist in the 'users' table of the SigRepo database.\n", base::paste0("'", user_name[base::which(!user_name %in% user_tbl$user_name)], "'", collapse = ", ")))
  }
  
  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = "signature_id",
    filter_coln_val = base::list("signature_id" = base::unique(signature_id)),
    check_db_table = TRUE
  )
  
  # If signature exists, return the signature table else throw an error message
  if(base::nrow(signature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("\nThere is no signature_id = '%s' in the 'signatures' table of the SigRepo database.\n", signature_id))
    
  }else{
    
    # If user is not admin, check if it has access to the signature
    if(orig_user_role != "admin"){
      
      # Check if user is the one who uploaded the signature
      signature_user_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signatures",
        return_var = "*",
        filter_coln_var = c("signature_id", "user_name"), 
        filter_coln_val = base::list("signature_id" = signature_id, "user_name" = orig_user_name),
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
          filter_coln_val = base::list("signature_id" = signature_id, "user_name" = orig_user_name, "access_type" = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user does not have permission, throw an error message
        if(base::nrow(signature_access_tbl) == 0){
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn)) 
          # Show message
          base::stop(base::sprintf("\nUser = '%s' does not have the permission to add User = %s to signature_id = '%s' in the SigRepo database.\n", orig_user_name, base::paste0("'", user_name, "'", collapse = ", "), signature_id))
        }
      }
      
    }
    
    # Create user signature access table
    table <- base::data.frame(
      signature_id = signature_id,
      user_name = user_name,
      access_type = access_type,
      stringsAsFactors = FALSE
    )
    
    # Create a hash key to look up values in database ####
    table <- SigRepo::createHashKey(
      table = table,
      hash_var = "access_sig_hashkey",
      hash_columns = c("signature_id", "user_name"),
      hash_method = "md5"
    )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn,
      db_table_name = "signature_access",
      table = table, 
      check_db_table = TRUE
    )
    
    # Remove duplicates from table before inserting into database ####
    table <- SigRepo::removeDuplicates(
      conn = conn,
      db_table_name = "signature_access",
      table = table,
      coln_var = "access_sig_hashkey",
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = "signature_access", 
      table = table,
      check_db_table = FALSE
    )

    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return 
    return(base::invisible())
    
  }  
}  


