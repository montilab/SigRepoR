#' @title addUser
#' @description Add a list of users to the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param user_tbl A Data Frame; must contain the following column names: 
#' user_name, user_password, user_email, user_first, user_last, user_affiliation, 
#' user_role, active (required)
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Create users table
#' user_tbl <- base::data.frame(
#'   user_name = "joe12343",
#'   user_password = "ffgfd",
#'   user_email = "joejoe@gmail.com",
#'   user_first = "joe",
#'   user_last = "joe",
#'   user_affiliation = "Imaginary Tester"
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
#' # Add users to database                      
#' SigRepo::addUser(
#'   conn_handler = conn_handler,
#'   user_tbl = user_tbl,
#'   verbose = FALSE
#' )
#'
#' }
#' 
#' @export
addUser <- function(
    conn_handler = NULL,
    user_tbl,
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
  required_column_fields <- c("user_name", "user_password", "user_email", "user_role")
  user_role_options <- c("admin", "editor", "viewer")
  db_table_name <- "users"
  table <- user_tbl
  
  # Check required column fields
  if(base::any(!required_column_fields %in% colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("\nThe table is missing the following required column names: %s.\n", base::paste0(required_column_fields[base::which(!required_column_fields %in% base::colnames(table))], collapse = ", ")))
  }
  
  # Make sure required column fields do not have any empty values ####
  if(base::any(base::is.na(table[,required_column_fields]) == TRUE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("\nAll required column names: %s cannot contain any empty values.\n", base::paste0(required_column_fields, collapse = ", ")))
  }
  
  # Check user roles ####
  if(base::any(!table$user_role %in% user_role_options)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("\nThe 'user_role' column must contain one of the following roles: %s.\n", base::paste0(user_role_options, collapse = "/")))
  }
  
  # Check user active status
  if(!"active" %in% base::colnames(table)){
    table <- table |> dplyr::mutate(active = 0)
  }else{
    table <- table |> dplyr::mutate(active = base::sapply(base::seq_along(.data$active), function(s){ base::ifelse(.data$active[s] == 1, 1, 0) }))
  }
  
  # Check user emails ####
  check_emails <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", base::as.character(table$user_email), ignore.case = TRUE)
 
  # If any emails do not have correct format, throw an error message
  if(base::any(check_emails == FALSE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\nInvalid email format.\n")
  }
  
  # Check for duplicated emails ####
  SigRepo::checkDuplicatedEmails(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "user_email",
    check_db_table = FALSE
  )
  
  # Create a hash key for user password
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "user_password_hashkey",
    hash_columns = "user_password",
    hash_method = "md5"
  )
  
  # Create an api key for each user
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "api_key",
    hash_columns = c("user_name", "user_email", "user_role"),
    hash_method = "md5"
  )
  
  # Create a hash key to check for duplicates
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "user_hashkey",
    hash_columns = "user_name",
    hash_method = "md5"
  )
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "date_created",
    check_db_table = FALSE
  )
  
  # Get db user table 
  # Note inactive users do not have an account in DB
  db_user_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SELECT host, user FROM mysql.user"))
  
  # Extract inactive users and re-activate them
  inactive_user_tbl <- table |> dplyr::filter(!.data$user_name %in% db_user_tbl$user)
  
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
  
  # Add inactive users to user table to be recreated again
  table <- table |> base::rbind(inactive_user_tbl) |> dplyr::distinct_all()
  
  # IF USER IS NOT ROOT AND NOT EXIST IN DATABASE, CREATE USER AND GRANT USER PERMISSIONS TO DATABASE
  purrr::walk(
    base::seq_len(base::nrow(table)),
    function(u){
      #u=1;
      # CHECK IF USER EXIST IN DATABASE
      check_user_tbl <- base::suppressWarnings(
        DBI::dbGetQuery(conn = conn, statement = base::sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", table$user_name[u]))
      )
      
      # CREATE USER IF NOT EXIST
      if(base::nrow(check_user_tbl) == 0){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s';", table$user_name[u], table$user_password[u])))
      }
      
      # GRANT USER PERMISSIONS TO DATABASE BASED ON THEIR ROLES
      if(table$user_role[u] == "admin"){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT ALL PRIVILEGES ON `sigrepo`.* TO '%s'@'%%' WITH GRANT OPTION;", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))
      }else if(table$user_role[u] == "editor"){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT ON `sigrepo`.* TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT ON `sigrepo`.`users` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT ON `sigrepo`.`keywords` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT ON `sigrepo`.`phenotypes` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT ON `sigrepo`.`platforms` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`signatures` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`signature_access` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`signature_feature_set` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`signature_collection_access` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`collection` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON `sigrepo`.`collection_access` TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))       
      }else if(table$user_role[u] == "viewer"){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT ON `sigrepo`.* TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
      }
    }
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return message
  SigRepo::verbose("Finished uploading.\n")
  
}



