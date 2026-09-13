#' @title inActivateUser
#' @description Inactivate a user in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param user_name Name of the user to be inactivated (required).
#' @param verbose Logical; whether or not to print the diagnostic messages. 
#' Default is \code{TRUE}.
#' 
#' @export
inActivateUser <- function(
    conn_handler = NULL,
    user_name,
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
  
  # Check user_name ####
  if(!base::length(user_name) == 1 || base::all(user_name %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop("\n'user_name' must have a length of 1 and cannot be empty.\n")
  }
  
  # Create a list of variables to check database ####
  db_table_name <- "users"
  
  # Get user table
  table <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    return_var = "*",
    filter_coln_var = "user_name", 
    filter_coln_val = base::list("user_name" = base::unique(user_name)),
    check_db_table = FALSE
  )

  # Check if user exists in the database
  if(base::nrow(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("\nThere is no user = '%s' existed in the 'users' table of the SigRepo database.\n", user_name))
  }
  
  # Update user to be inactive in the users table ####
  SigRepo::updateUser(
    conn_handler = conn_handler,
    user_name = user_name,
    active = FALSE,
    verbose = FALSE
  )
  
  # Reset message options
  SigRepo::print_messages(verbose = verbose)
  
  # CHECK IF USER EXIST IN DATABASE
  check_user_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", user_name)))
  
  # IF USER EXISTS, DROP USER FROM DATABASE
  if(base::nrow(check_user_tbl) > 0)
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("DROP USER '%s'@'%%';", user_name)))

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return message
  SigRepo::verbose(base::sprintf("user_name = '%s' has been inactivated.", user_name))
  
}



