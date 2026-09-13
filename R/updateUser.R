#' @title updateUser
#' @description Update a user in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param user_name Name of a user to be updated (required).
#' @param password Password of a user to be updated. Default is NULL.
#' @param email Email of a user to be updated. Default is NULL.
#' @param first_name First name of a user to be updated. Default is NULL
#' @param last_name Last name of a user to be updated. Default is NULL.
#' @param affiliation Affiliation of the user. Default is NULL.
#' @param role Role of a user to be updated. Choices are admin/editor/viewer.
#' @param active Whether to make a user TRUE (active) or FALSE (inactive). 
#' Default is NULL.
#' @param verbose Logical; whether or not to print the diagnostic messages. 
#' Defaults to 'TRUE'
#' 
#' @export
updateUser <- function(
    conn_handler = NULL,
    user_name,
    password = NULL,
    email = NULL,
    first_name = NULL,
    last_name = NULL,
    affiliation = NULL,
    role = NULL,
    active = NULL,
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
  
  # Get unique signature id
  user_name <- base::unique(user_name) 
  
  # Check user_name ####
  if(!base::length(user_name) == 1 || base::all(user_name %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop("\n'user_name' must have a length of 1 and cannot be empty.\n")
  }
  
  # Get table name ####
  db_table_name <- "users"
  
  # Get user table
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    return_var = "*",
    filter_coln_var = "user_name", 
    filter_coln_val = base::list("user_name" = user_name),
    check_db_table = FALSE
  )
  
  # If user does not have permission, throw an error message
  if(base::nrow(user_tbl) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("\nCannot update user = '%s' as user does not exist in the 'users' table of the database.\n", user_name))
  }
  
  # Check role ####
  if(base::length(role[1]) > 0 && base::all(!role[1] %in% c("admin", "editor", "viewer"))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop("\n'role' must have a length of 1 and be one of the three roles: admin/editor/viewer.\n")
  }
  
  # Check active status ####
  if(base::length(active[1]) > 0 && base::all(!active[1] %in% c(0,1))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop("\n'active' must have a length of 1 and should be set to TRUE (active) or FALSE (inactive).\n")
  }
  
  # Check email ####
  if(base::length(email[1]) > 0){
    # Check email format ####
    check_email <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", base::as.character(email[1]), ignore.case = TRUE)
    
    # If any emails do not have a correct format, throw an error message
    if(base::any(check_email == FALSE)){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop("\nInvalid email format.\n")
    }
    
    # Get email table
    email_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      return_var = "*",
      filter_coln_var = "user_email", 
      filter_coln_val = base::list("user_email" = email[1]),
      check_db_table = FALSE
    ) 
    
    # Check if email not belongs to other users
    if(base::nrow(email_tbl) > 0 && !base::trimws(base::tolower(user_name[1])) %in% base::trimws(base::tolower(email_tbl$user_name))){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      # Show message
      base::stop("\nSomeone with the same email = '%s' already existed in the SigRepo database. Please try another email.\n")
    }
  }

  # Remove user from users table of the database
  SigRepo::delete_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    delete_coln_var = "user_name", 
    delete_coln_val = user_name,
    check_db_table = FALSE
  )
  
  # Create updated variables
  table <- user_tbl |> 
    dplyr::mutate(
      user_password_hashkey = base::ifelse(base::length(password[1]) == 0 || base::all(password[1] %in% c("", NA)), .data$user_password_hashkey, digest::digest(password[1], algo = "md5", serialize = FALSE)),
      user_email = base::ifelse(base::length(email[1]) == 0 || base::all(email[1] %in% c("", NA)), .data$user_email, email[1]),
      user_first = base::ifelse(base::length(first_name[1]) == 0 || base::all(first_name[1] %in% c("", NA)), .data$user_first, first_name[1]),
      user_last = base::ifelse(length(last_name[1]) == 0 || base::all(last_name[1] %in% c("", NA)), .data$user_last, last_name[1]),
      user_affiliation = base::ifelse(base::length(affiliation[1]) == 0 || base::all(affiliation[1] %in% c("", NA)), .data$user_affiliation, affiliation[1]),
      user_role = base::ifelse(base::length(role[1]) == 0 || base::all(role[1] %in% c("", NA)), .data$user_role, role[1]),
      active = base::ifelse(base::length(!!active[1]) == 0 || base::all(!!active[1] %in% c("", NA)), .data$active, base::as.numeric(!!active[1]))
    )
  
  # Create an api key 
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "api_key",
    hash_columns = c("user_name", "user_email", "user_role"),
    hash_method = "md5"
  )
  
  # Remove duplicated hash keys ####
  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "user_name",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )  
  
  # CHECK IF USER EXISTS IN DATABASE
  check_user_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", user_name[1])))
  
  # MAKE SURE USER EXISTS
  if(base::nrow(check_user_tbl) > 0){
    # CHANGE PASSWORD IF A NEW PASSWORD IS GIVEN
    if(base::length(password[1]) == 1 && base::all(!password[1] %in% c("", NA))){
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("ALTER USER '%s'@'%%' IDENTIFIED BY '%s';", user_name[1], password[1])))
    }
    # GRANT USER PERMISSIONS TO DATABASE BASED ON THEIR ROLES
    if(base::length(role[1]) == 1 && role[1] %in% "admin"){
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT ALL PRIVILEGES ON `sigrepo`.* TO '%s'@'%%' WITH GRANT OPTION;", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))
    }else if(base::length(role[1]) == 1 && role[1] %in% "editor"){
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT ON `sigrepo`.* TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT ON `sigrepo`.`users` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT ON `sigrepo`.`keywords` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT ON `sigrepo`.`phenotypes` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT ON `sigrepo`.`platforms` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`signatures` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`signature_access` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`signature_feature_set` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`signature_collection_access` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`collection` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`collection_access` TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
    }else if(base::length(role[1]) == 1 && role[1] %in% "viewer"){
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT ON `sigrepo`.* TO '%s'@'%%';", user_name[1])))
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
    }
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return message
  SigRepo::verbose(base::sprintf("user_name = '%s' has been updated.", user_name[1]))
  
}



