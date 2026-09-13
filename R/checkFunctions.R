
#' @title print_messages
#' @description Function to whether print diagnostic messages or not
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' 
#' @export
#' @keywords internal
print_messages <- function(verbose){

  verbose <- base::isTRUE(verbose)

  base::options(warning.length = 2000L, show.error.messages = TRUE, verbose = verbose)
  
}

#' @title verbose
#' @description Function to whether print diagnostic messages or not
#' 
#' @keywords internal
#' 
#' @export
verbose <- function(...){
  
  # Fetch verbose option
  opt <- base::isTRUE(base::getOption("verbose"))
  
  # If opt is FALSE
  if(!opt) return(base::invisible(NULL))
  
  # Return messages
  msg_text <- paste(..., collapse = "")
  base::message(msg_text)
  
}

#' @title checkPermissions
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param action_type A list of actions that the connected user can perform. 
#' Options: SELECT, INSERT, UPDATE, DELETE, CREATE USER
#' @param required_role The required role of the user who can perform the action. 
#' 
#' @keywords internal 
#' 
#' @export
checkPermissions <- function(
    conn,
    action_type = c("SELECT", "INSERT", "UPDATE", "DELETE", "CREATE USER"),
    required_role = c("admin", "editor", "viewer")
){
  
  # Check action_type
  action_type <- base::match.arg(action_type, several.ok = TRUE)
  
  # Check required_role
  required_role <- base::match.arg(required_role)
  
  # Get user connection info
  conn_info <- DBI::dbGetInfo(conn)
  
  # Look up user in the database
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "users", 
    return_var = c("user_name", "user_role", "api_key", "active"), 
    filter_coln_var = "user_name", 
    filter_coln_val = base::list("user_name" = conn_info$user), 
    check_db_table = TRUE
  )
  
  # Check if user existed in the users table in the database
  if(base::nrow(user_tbl) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("User = '%s' does not exist in the database. Please contact admin to add user to the database.\n", conn_info$user)) 
  }
  
  # Get the grant tables
  grant_tbl <- base::tryCatch({
    base::suppressWarnings(
      DBI::dbGetQuery(conn = conn, statement = base::sprintf("SHOW GRANTS FOR '%s'@'%%';", conn_info$user))
    ) 
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  })    
  
  # Create a placeholder to store all privileges
  user_privileges <- NULL
  
  # Loop through each grant and extract list of actions that user can perform in the database
  for(s in 1:base::nrow(grant_tbl)){ 
    #s=1;
    privs <- grant_tbl |> 
      dplyr::slice(s) |> 
      purrr::flatten_chr() |> 
      base::sapply(FUN = function(x){ base::gsub("GRANT(.*)ON(.*)TO(.*)", "\\1", x, perl = TRUE) }) |> 
      base::strsplit(",") |> 
      purrr::flatten_chr() |> 
      base::trimws()
    user_privileges <- c(user_privileges, privs) |> base::unique()
  }
  
  # Check if user has the permission to perform the selected actions in the database
  if(!"ALL PRIVILEGES" %in% base::toupper(user_privileges) && base::any(!action_type %in% user_privileges)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("User = '%s' does not have permission to perform this action in the database.\n", conn_info$user)) 
  }
  
  # Get the user roles
  if(user_tbl$user_role == "admin"){
    all_roles <- c("viewer", "editor", "admin")
  }else if(user_tbl$user_role == "editor"){
    all_roles <- c("viewer", "editor")
  }else if(user_tbl$user_role == "viewer"){
    all_roles <- c("viewer")
  }
  
  # Check if user has the specific role to perform the selected action in the database
  if(!required_role %in% all_roles || user_tbl$active %in% 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("User = '%s' does not have permission to perform this action in the database.", conn_info$user)) 
  }
  
  # Return user connection and user role
  return(
    c(conn_info, user_role = user_tbl$user_role, api_key = user_tbl$api_key)
  )
  
}

#' @title checkDBTable
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param db_table_name Name of table in the database
#' @param check Check whether table exists in the database. Default = TRUE.
#' 
#' @keywords internal
#' 
#' @export
checkDBTable <- function(
    conn,
    db_table_name,
    check = TRUE
){
  
  # If check is false, exit function
  if(check == FALSE) return(NULL)
  
  # Check if table exists in database
  all_tables <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
  
  # Check if table exists in the database
  if(!db_table_name %in% all_tables$Tables_in_sigrepo){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("There is no '%s' table in SigRepo database.", db_table_name))
  }
  
}

#' @title checkTableInput
#' @description Check table against a particular table in the database.
#' @param conn An established connection to database using newConnhandler() 
#' @param db_table_name Name of a table in the database
#' @param table A data frame object
#' @param exclude_coln_names A list of column names to be excluded from the check.
#' @param check_db_table Check whether table exists in the database. Default = TRUE.
#' 
#' @keywords internal
#'
#' @export
checkTableInput <- function(
    conn, 
    db_table_name,
    table,
    exclude_coln_names = NULL,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table,
  )
  
  # Check if table is a data frame object and not empty
  if(!methods::is(table, "data.frame") || base::length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'table' must be a data frame object and cannot be empty.\n")
  }
  
  # Whether to exclude selected column names from the checklist
  if(base::length(exclude_coln_names) > 0){
    db_col_names <- db_col_names[base::which(!db_col_names %in% exclude_coln_names)]
  }
  
  # Check column fields
  if(base::any(!db_col_names %in% base::colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("the table is missing the following column names: %s.\n", base::paste0(db_col_names[base::which(!db_col_names %in% colnames(table))], collapse = ", ")))
  }
  
  # Clean up the table by converting all empty values as NULL 
  table <- base::data.frame(table, stringsAsFactors = FALSE) |> 
    dplyr::mutate_all(function(x){ base::as.character(x) }) |>
    dplyr::mutate_if(base::is.character, function(x){ base::trimws(base::gsub("'", "", x, perl = TRUE)) }) |> 
    dplyr::mutate_if(base::is.character, function(x){ base::replace(x, x %in% "NA", "'NULL'") }) |> 
    dplyr::mutate_if(base::is.character, function(x){ base::replace(x, x %in% "NULL", "'NULL'") }) |> 
    dplyr::mutate_if(base::is.character, function(x){ base::replace(x, x %in% "", "'NULL'") }) |> 
    dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "'NULL'") }) |> 
    dplyr::distinct_all()
  
  # Return a unique and cleanup table
  return(table)
  
}

#' @title checkDuplicatedEmails
#' @description Check if provided user table has duplicated emails.
#' @param conn An established connection to database using newConnhandler() 
#' @param db_table_name Name of a table in the database
#' @param table A data frame object
#' @param coln_var A list of column names to be excluded from the check.
#' @param check_db_table Check whether table exists in the database. Default = TRUE.
#' 
#' @keywords internal
#' 
#' @export
checkDuplicatedEmails <- function( 
    conn,
    db_table_name = "users",
    table,
    coln_var = "user_email",
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check coln_var
  if(!base::length(coln_var) == 1 || base::any(coln_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'coln_var' must have a length of 1 and cannot be empty.")
  }
  
  # Check if table is a data frame object and not empty
  if(!methods::is(table, "data.frame") || base::length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'table' must be a data frame object and cannot be empty.")
  }
  
  ## Check if column variable existed in the table
  if(!coln_var %in% base::colnames(table)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("Column '%s' does not existed in the table.", coln_var))
  }
  
  ## Check if column variable existed in the database
  if(!coln_var %in% db_col_names){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("Columns: %s does not exist in the '%s' table of the database.", base::paste0("'", coln_var, "'", collapse = ", "), db_table_name))
  }
  
  # Get number of observations 
  n_obs <- SigRepo::getNumOfObs(conn = conn, db_table_name = db_table_name)
  
  ## Check if table has values, only return non-overlapping samples
  if(n_obs$count > 0){
    
    return_var <- "*"
    filter_coln_var <- coln_var
    filter_coln_val <- table |> dplyr::distinct(!!!rlang::syms(coln_var)) |> base::as.list()
    
    existing_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name, 
      return_var = return_var, 
      filter_coln_var = filter_coln_var, 
      filter_coln_val = filter_coln_val,
      check_db_table = check_db_table
    )
    
    if(base::nrow(existing_tbl) > 0){
      purrr::walk(
        1:base::nrow(table),
        function(s){
          #s=1;
          check_email <- existing_tbl |> dplyr::filter(.data$user_email %in% table$user_email[s])
          if(nrow(check_email) > 0 && !base::trimws(base::tolower(table$user_name[s])) %in% base::trimws(base::tolower(check_email$user_name))){
            # Disconnect from database ####
            base::suppressWarnings(DBI::dbDisconnect(conn))  
            # Return error message
            base::stop(
              "\tThe following email address:\n",
              base::sprintf("\t%s\n", base::paste0(base::unique(existing_tbl[,return_var]), collapse = ",\n")),
              base::sprintf("\talready existed in the '%s' table of the database.\n", db_table_name),
              base::sprintf("\tEmail address must be unique for each user. Please provide a different email for user = '%s'.\n", table$user_name[s])
            )
          }
        }
      )
    }
    
  }
}

#' @title checkAssayType
#' @description Check if assay type is a valid type in the database
#' @param assay_type An assay type of the signature
#' 
#' @keywords internal 
#' 
#' @export
checkAssayType <- function(
    assay_type
){
  
  # Get assay tbl
  assay_tbl <- SigRepo::assay_tbl
  
  # Check if assay is available
  check_tbl <- assay_tbl |> 
    dplyr::filter(base::trimws(base::tolower(.data$assay_type)) %in% base::trimws(base::tolower(!!assay_type)))
  
  # Return message
  if(base::nrow(check_tbl) == 0){
    base::stop(base::sprintf("Invalid assay type. 'assay_type' must be one of the following options: %s", base::paste0(assay_tbl$assay_type, collapse = "/")))
  }else if(check_tbl$status[1] == "Unavailable"){
    base::stop(base::sprintf("\nAssay type = '%s' is not implemented in the database yet.", base::paste0(check_tbl$assay_type[1])))
  }
  
}

#' @title checkDirectionType
#' @description Check if direction type is a valid type in the database
#' @param direction_type A direction type of the signature
#' 
#' @keywords internal 
#' 
#' @export
checkDirectionType <- function(
    direction_type
){
  
  # Get direction type options
  direction_type_options <- SigRepo::direction_types |> base::tolower() |> base::trimws()
  
  # Return message
  if(!base::trimws(base::tolower(direction_type[1])) %in% direction_type_options)
    base::stop(base::sprintf("'direction_type' must be one of the following options: %s", base::paste0(direction_type_options, collapse = "/")))

}

#' @title checkOmicSignature
#' @description Check if omic_signature is a valid R6 object
#' @param omic_signature An OmicSignature object from OmicSignature package
#' 
#' @keywords internal 
#' 
#' @export
checkOmicSignature <- function(
    omic_signature
){
  
  # Check if omic_signature is an OmicSignature class object ####
  if(!methods::is(omic_signature, "OmicSignature"))
    base::stop("'omic_signature' must be an R6 class object from OmicSignature package.") 
  
  # Check metadata and signature
  if(!"metadata" %in% base::names(omic_signature))
    base::stop("'omic_signature' must contain a metadata object.\n")
  
  if(!"signature" %in% base::names(omic_signature))
    base::stop("'omic_signature' must contain a signature object.\n")
  
  # Extract metadata from omic_signature ####
  metadata <- omic_signature$metadata
  
  # Check if metadata is a list ####
  if(!methods::is(metadata, "list"))
    base::stop("'metadata' in OmicSignature object must be a list.")
  
  # Check required metadata fields ####
  metadata_fields <- c('signature_name', 'organism', 'direction_type', 'assay_type', 'phenotype')
  
  if(base::any(!metadata_fields %in% base::names(metadata)))
    base::stop(base::sprintf("'metadata' in OmicSignature object must have the following column names: %s", base::paste0(metadata_fields, collapse = ", ")))
  
  # Check signature name (required) ####
  if(base::length(metadata$signature_name[1]) == 0 || metadata$signature_name[1] %in% c(NA, ""))
    base::stop("'signature_name' in OmicSignature's metadata object is required and cannot be empty.")
  
  # Check organism (required) #####
  if(base::length(metadata$organism[1]) == 0 || metadata$organism[1] %in% c(NA, ""))
    base::stop("'organism' in OmicSignature's metadata object is required and cannot be empty.")
  
  # Check direction_type (required) ####
  SigRepo::checkDirectionType(direction_type = metadata$direction_type[1])
  
  # Check assay_type (required) ####
  SigRepo::checkAssayType(assay_type = metadata$assay_type[1])

  # Check phenotype (required) #####
  if(base::length(metadata$phenotype[1]) == 0 || metadata$phenotype[1] %in% c(NA, ""))
    base::stop("'phenotype' in OmicSignature's metadata object is required and cannot be empty.")
  
  # Extract signature table from omic_signature ####
  signature <- omic_signature$signature  
  
  # Check if signature is a data frame ####
  if(!methods::is(signature, "data.frame") || base::length(signature) == 0)
    base::stop("'signature' in OmicSignature object must be a data frame and cannot be empty.")
  
  # Check required signature fields ####
  signature_fields <- c('feature_name', 'probe_id')
  
  if(base::any(!signature_fields %in% base::colnames(signature)))
    base::stop(base::sprintf("'signature' in OmicSignature object must have the following column names: %s", base::paste0(signature_fields, collapse = ", ")))

  if(!"score" %in% base::colnames(signature))
    signature <- signature |> dplyr::mutate(score = "")
  
  if(metadata$direction_type[1] %in% c("bi-directional", "categorical") && !"group_label" %in% base::colnames(signature)){
    base::stop("'signature' in OmicSignature object requires a 'group_label' variable as the direction of the signature is 'bi-directional' or 'categorical'")
  }else if(metadata$direction_type[1] %in% c("uni-directional") && !"group_label" %in% base::colnames(signature)){
    signature <- signature |> dplyr::mutate(group_label = "All Features")
  }
  
  # Make sure required column fields do not have any empty values ####
  if(base::any(base::is.na(signature[,signature_fields]) == TRUE))
    base::stop(base::sprintf("All required column names in 'signature' of OmicSignature object: %s cannot contain any empty values.", base::paste0(signature_fields, collapse = ", ")))
  
  # Check difexp is provided ####
  if("difexp" %in% base::names(omic_signature)){
    difexp <- omic_signature$difexp
    if(base::is.null(difexp)){
      difexp <- NULL
    }else{
      # Check if difexp is a data frame 
      if(!methods::is(difexp, "data.frame") || base::nrow(difexp) == 0) 
        base::stop("'difexp' in OmicSignature object must be a data frame and cannot be empty.")
    }
  }else{
    difexp <- NULL
  }
  
  # If difexp is provided, check required difexp fields ####
  difexp_req_fields <- c('feature_name', 'probe_id', 'score'); difexp_opt_fields <- c('p_value', 'q_value','adj_p')
  
  if(!base::is.null(difexp) && base::any(!difexp_req_fields %in% base::colnames(difexp)) && base::all(!difexp_opt_fields %in% base::colnames(difexp)))
    base::stop(base::sprintf("'difexp' in OmicSignature object must have the following required column names: %s, and as least one of the following fields: %s", base::paste0("'", difexp_req_fields, "'", collapse = ", "), base::paste0("'", difexp_opt_fields, "'", collapse = "/")))
  
  if(!base::is.null(difexp) && metadata$direction_type[1] %in% c("bi-directional", "categorical") && !"group_label" %in% base::colnames(difexp)){
    base::stop("When the direction of the signature is bi-directional or categorical, 'difexp' in OmicSignature requires a 'group_label' variable.")
  }else if(!base::is.null(difexp) && metadata$direction_type[1] %in% c("uni-directional") && !"group_label" %in% base::colnames(difexp)){
    difexp <- difexp |> dplyr::mutate(group_label = "All Features")
  }
  
  # Make sure required column fields do not have any empty values ####
  if(!base::is.null(difexp) && base::any(base::is.na(difexp[,difexp_req_fields]) == TRUE))
    base::stop(base::sprintf("All required column names in 'difexp' of OmicSignature object: %s cannot contain any empty values.", base::paste0(difexp_req_fields, collapse = ", ")))
  
  # Create has_difexp variable to store whether omic_signature has difexp included ####
  has_difexp <- base::ifelse(!base::is.null(difexp), 1, 0) 
  
  # Check if probe_ids in signature are in the difexp table if difexp is provided
  if(has_difexp == 1 && base::any(!signature$probe_id %in% difexp$probe_id)){
    base::stop("Some probe_ids in the `signature` object are not included in the probe_ids of the `difexp` object.")
  }
  
  # Check whether covariates is given ####
  if(!"covariates" %in% base::names(metadata)){
    metadata$covariates <- NULL
  }else{
    metadata$covariates <- base::paste0(metadata$covariates, collapse = ",")
  }
  
  # Check whether description is given ####
  if(!"description" %in% base::names(metadata)){
    metadata$description <- NULL
  }
  
  # Check whether score_cutoff is given ####
  if(!"score_cutoff" %in% base::names(metadata)){
    metadata$score_cutoff <- NULL
  }
  
  # Check whether logfc_cutoff is given ####
  if(!"logfc_cutoff" %in% base::names(metadata)){
    metadata$logfc_cutoff <- NULL
  }
  
  # Check whether p_value_cutoff is given ####
  if(!"p_value_cutoff" %in% base::names(metadata)){
    metadata$p_value_cutoff <- NULL
  }
  
  # Check whether adj_p_cutoff is given ####
  if(!"adj_p_cutoff" %in% base::names(metadata)){
    metadata$adj_p_cutoff <- NULL
  }
  
  # Check cutoff_description is given ####
  if(!"cutoff_description" %in% base::names(metadata)){
    metadata$cutoff_description <- NULL
  }
  
  # Check whether keywords is given ####
  if(!"keywords" %in% base::names(metadata)){
    metadata$keywords <- NULL
  }else{
    metadata$keywords <- base::paste0(metadata$keywords, collapse = ",")
  }
  
  # Check whether PMID is given ####
  if(!"PMID" %in% base::names(metadata)){
    metadata$PMID <- NULL
  }else{
    metadata$PMID <- base::paste0(metadata$PMID, collapse = ",")
  }
  
  # Check if year is given ####
  if(!"year" %in% base::names(metadata)){
    metadata$year <- NULL
  }else{
    metadata$year <- base::paste0(metadata$year, collapse = ",")
  }
  
  # Check whether others is given ####
  if(!"others" %in% base::names(metadata)){
    metadata$others <- NULL
  } 
  
  # Update signature and difexp in signature objects
  omic_signature <- base::tryCatch({
    OmicSignature::OmicSignature$new(
      metadata = metadata,
      signature = signature,
      difexp = difexp
    )
  }, error = function(e){
    # Return error message
    base::stop("OmicSignature Error: ", e, "\n")
  })
  
  # Return difexp status and omic_signature
  return(omic_signature = omic_signature)
  
}


#' @title checkOmicCollection
#' @description Check if omic_collection is a valid R6 object
#' @param omic_collection An OmicSignatureCollection object from OmicSignature package
#' 
#' @keywords internal
#' 
#' @export
checkOmicCollection <- function(
    omic_collection
){
  
  # Check if metadata exists in the collection
  if(!"metadata" %in% base::names(omic_collection))
    base::stop("'OmicSignatureCollection' must contain a metadata object.\n")
  
  # Check if OmicSigList exists in the collection
  if(!"OmicSigList" %in% base::names(omic_collection))
    base::stop("'OmicSignatureCollection' must contain a OmicSigList object.\n")
  
  # Extract metadata from omic_collection ####
  metadata <- omic_collection$metadata
  
  # Check if metadata is a list ####
  if(!methods::is(metadata, "list"))
    base::stop("'metadata' in OmicSignatureCollection must be a list.")
  
  # Check required metadata fields ####
  metadata_fields <- c('collection_name', 'description')
  
  if(base::any(!metadata_fields %in% base::names(metadata)))
    base::stop("'metadata' in OmicSignatureCollection must have the following names:", base::paste0(metadata_fields, collapse = ", "))
  
  # Extract OmicSigList from omic_collection ####
  omic_sig_list <- omic_collection$OmicSigList  
  
  # Check if OmicSigList is a list ####
  if(!methods::is(omic_sig_list, "list"))
    base::stop("'OmicSigList' in OmicSignatureCollection must be a list containning a list of signature objects.")
  
  # Create a place holder to store signature collection
  omic_signature_collection <- base::list()
  
  # Check required signature fields ####
  for(c in base::seq_along(omic_sig_list)){
    #c=1;
    SigRepo::verbose("Checking Signature: ", base::names(omic_sig_list[c]))
    omic_signature <- SigRepo::checkOmicSignature(
      omic_signature = omic_sig_list[[c]]
    )
    # Append OmicSignature object to overall list
    omic_signature_collection <- c(
      omic_signature_collection, 
      omic_signature
    )
  }
  
  # Create an OmicSignatureCollection object
  omic_collection <- base::tryCatch({
    OmicSignature::OmicSignatureCollection$new(
      metadata = metadata,
      OmicSigList = omic_signature_collection
    )
  }, error = function(e){
    base::stop("OmicSignatureCollection Error: ", e, "\n")
  })
  
  # Return omic_collection
  return(omic_collection = omic_collection)
  
}

#' @title getNumOfObs
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param db_table_name A table in the database
#' 
#' @keywords internal
#' 
#' @export
getNumOfObs <- function(
    conn,
    db_table_name
){
  
  # Create sql statement
  statement <- base::sprintf("SELECT COUNT(*) AS count FROM %s", db_table_name)
  
  # Get number of observations 
  n_obs <- base::tryCatch({
    base::suppressWarnings(
      DBI::dbGetQuery(conn = conn, statement = statement)
    )
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  })
  
  # Return number of observations 
  return(n_obs)
  
}

#' @title getDBColNames
#' @description Get column names of a particular table in the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param db_table_name Name of a table in the database
#' @param check_db_table Check whether table exists in the database. Default = TRUE
#' @param exclude_coln_names optional flag to exclude column names from the Colnames list.
#' @keywords internal
#' 
#' @export
getDBColNames <- function(
    conn,
    db_table_name,
    check_db_table = TRUE,
    exclude_coln_names = NULL
){
  
  # Check whether table exists in the database
  SigRepo::checkDBTable(
    conn = conn,
    db_table_name = db_table_name,
    check = check_db_table
  )
  
  # Get sql statement
  statement <- base::sprintf("SELECT * FROM %s LIMIT 1", db_table_name)
  
  # Run sql statement
  db_table <- base::tryCatch({
    base::suppressWarnings(
      DBI::dbGetQuery(conn = conn, statement = statement)
    )
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))    
    # Return error message
    base::stop(e, "\n")
  })
  
  # Get table column names
  col_names <- base::colnames(db_table)
  
  # Exclude column names from table
  if(!base::is.null(exclude_coln_names)) {
    col_names <- base::setdiff(col_names, exclude_coln_names)
  }
  
  # Return column names ####
  return(col_names)
  
}

#' @title getVariableID
#' @description Get a variable ID by looking up its primary key or hash key
#' @param conn An established connection to database using newConnhandler() 
#' @param db_table_name An api key uses to access the database
#' @param table An api key uses to access the database
#' @param coln_var An api key uses to access the database
#' @param coln_var_id An api key uses to access the database
#' @param check_db_table An api key uses to access the database
#' 
#' @keywords internal
#' 
#' @export
getVariableID <- function(
    conn,
    db_table_name,
    table,
    coln_var,
    coln_var_id,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check coln_var
  if(!base::length(coln_var) == 1 || base::any(coln_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'coln_var' must have a length of 1 and cannot be empty.")
  }
  
  # Check coln_var_id
  if(!base::length(coln_var_id) == 1 || base::any(coln_var_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'coln_var_id' must have a length of 1 and cannot be empty.")
  }
  
  # Check if table is a data frame object and not empty
  if(!methods::is(table, "data.frame") || base::length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'table' must be a data frame object and cannot be empty.")
  }
  
  ## Check if column variable existed in the table
  if(!coln_var %in% base::colnames(table)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("Column '%s' does not existed in the table.", coln_var))
  }
  
  # Make sure column variable is not empty
  if(base::any(table[, coln_var] %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("'%s' is a required field and cannot contain any empty values.", coln_var))
  }
  
  # Check if columns existed in the database
  check_db_var <- c(coln_var, coln_var_id)
  
  ## Check if column variable existed in the database
  if(base::any(!check_db_var %in% db_col_names)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("Columns: %s does not exist in the '%s' table of the database.", base::paste0("'", check_db_var[base::which(!check_db_var %in% db_col_names)], "'", collapse = ", "), db_table_name))
  }
  
  # Look up ID in database
  return_var <- check_db_var
  filter_coln_var <- coln_var
  filter_coln_val <- table |> dplyr::distinct(!!!rlang::syms(coln_var)) |> base::as.list()
  
  tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name, 
    return_var = return_var, 
    filter_coln_var = filter_coln_var, 
    filter_coln_val = filter_coln_val,
    check_db_table = FALSE
  )
  
  # Return table
  return(tbl)
  
}



#' @title createHashKey
#' @description Check api key whether it is valid to access the database
#' @param table A data frame with specific columns to be used to create 
#' the hash key  
#' @param hash_var The hash variable to be created
#' @param hash_columns The specific columns to be used to create the hash key
#' @param hash_method The hashing method to create the key. Default \code{md5}.
#' 
#' @keywords internal
#' 
#' @export
#' @import digest
createHashKey <- function(
    table,
    hash_var,
    hash_columns,
    hash_method = "md5"
){
  
  # check hash_method
  hash_method <- base::match.arg(hash_method)
  
  # Check hash_var
  if(!base::length(hash_var) == 1 || base::any(hash_var %in% c(NA, ""))){
    base::stop("'hash_var' must have a length of 1 and cannot be empty.")
  }
  
  # Check hash_columns
  if(base::length(hash_columns) == 0 || base::any(hash_columns %in% c(NA, ""))){  
    base::stop("'hash_columns' cannot be empty.")
  }
  
  # Check if table is a data frame object and not empty
  if(!methods::is(table, "data.frame") || base::length(table) == 0){
    base::stop("'table' must be a data frame object and cannot be empty.")
  }
  
  # Make sure hash columns exist in table and are not empty
  purrr::walk(
    base::seq_along(hash_columns),
    function(r){
      #r=1;
      ## Check if the hash column exists in the table
      if(!hash_columns[r] %in% base::colnames(table)){
        base::stop(base::sprintf("'%s' is required and does not existed in the table.\n", hash_columns[r]))
      }      
      ## Make sure hash column is not empty
      if(base::any(table[, hash_columns[r]] %in% c(NA, ""))){
        base::stop(base::sprintf("'%s' is required and cannot contain any empty values.\n", hash_columns[r]))
      }      
    }
  )
  
  # Get column names without the creating harh_var
  tbl_colnames <- base::colnames(table)[base::which(!base::colnames(table) %in% hash_var)]
  
  # Rename hash key
  renamed_key_var <- c("hash_key")
  base::names(renamed_key_var) <- hash_var
  
  # Create the hash variable
  table <- table |> 
    dplyr::select(dplyr::all_of(tbl_colnames)) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      hash_key = base::paste0(!!!rlang::syms(hash_columns)) |> base::tolower() |> digest::digest(algo = "md5", serialize = FALSE)
    ) |> 
    dplyr::rename(dplyr::all_of(renamed_key_var)) |> 
    dplyr::ungroup()
  
  # Return table
  return(table)
  
}

#' @title removeDuplicates
#' @description Remove duplicates from table in the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param db_table_name Name of a table in the database
#' @param table A data frame object
#' @param coln_var A column variable in the data table
#' @param check_db_table whether to check database table. Default = TRUE
#' 
#' @keywords internal
#' 
#' @export
removeDuplicates <- function( 
    conn,
    db_table_name,
    table,
    coln_var,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check coln_var
  if(!base::length(coln_var) == 1 || base::any(coln_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'coln_var' must have a length of 1 and cannot be empty.")
  }
  
  # Check if table is a data frame object and not empty
  if(!methods::is(table, "data.frame") || base::length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'table' must be a data frame object and cannot be empty.")
  }
  
  ## Check if column variable existed in the table
  if(!coln_var %in% base::colnames(table)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("Column '%s' does not existed in the table.", coln_var))
  }
  
  ## Check if column variable existed in the database
  if(!coln_var %in% db_col_names){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("Columns: %s does not exist in the '%s' table of the database.", base::paste0("'", coln_var, "'", collapse = ", "), db_table_name))
  }
  
  # Get number of observations
  n_obs <- SigRepo::getNumOfObs(conn = conn, db_table_name = db_table_name)
  
  ## Check if table has values, only return non-overlapping samples
  if(n_obs$count > 0){
    
    return_var <- coln_var
    filter_coln_var <- coln_var
    filter_coln_val <- table |> dplyr::distinct(!!!rlang::syms(coln_var)) |> base::as.list()
    
    existing_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name, 
      return_var = return_var, 
      filter_coln_var = filter_coln_var, 
      filter_coln_val = filter_coln_val,
      check_db_table = check_db_table
    )
    
    if(base::nrow(existing_tbl) > 0){
      table <- table |> dplyr::mutate(id = base::trimws(base::tolower(!!!rlang::syms(coln_var)))) |> 
        dplyr::anti_join(
          existing_tbl |> dplyr::mutate(id = base::trimws(base::tolower(!!!rlang::syms(coln_var)))) |> dplyr::select(-dplyr::all_of(coln_var)), 
          by = "id"
        )
      
      if(base::nrow(table) == 0){
        base::warnings(base::sprintf("All records of this dataset already existed in the '%s' table of the database.\n", db_table_name))
      }
    }
    
  }
  
  # Return table
  return(table)
  
}
