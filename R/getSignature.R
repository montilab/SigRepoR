#' @title getSignature
#' @description Get a list of signatures uploaded by a specified user in the database.
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param signature_name Name of signature to be returned (required)
#' @param signature_id Database ID of signature to be returned (required)
#' @param verbose Logical; whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
#' @examples
#' \dontrun{
#' SigRepo::getSignature(conn_handler = conn_handler,
#'                       signature_id = 56,
#'                       signature_name = "test_signature",
#'                       verbose = TRUE
#' )
#' }
#'       
getSignature <- function(
    conn_handler = NULL,
    signature_name = NULL,
    signature_id = NULL,
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
  
  # Look up signatures
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signatures", 
    return_var = "*", 
    check_db_table = TRUE
  ) 
  
  # Get a list of filtered variables
  filter_var_list <- base::list(
    "signature_id" = base::unique(signature_id),
    "signature_name" = base::unique(signature_name)
  )
  
  # Filter table with given search variables
  for(r in base::seq_along(filter_var_list)){
    #r=1;
    filter_status <- base::ifelse(base::length(filter_var_list[[r]]) == 0 || base::all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
    if(filter_status == TRUE){
      filter_var <- base::names(filter_var_list)[r]
      filter_val <- filter_var_list[[r]][base::which(!filter_var_list[[r]] %in% c(NA, ""))]
      signature_tbl <- signature_tbl |> dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }
  
  # If user_role is not admin, check if user has the permission to access the signature ####
  if(user_role != "admin"){
    # Get a list of signature with visibility = FALSE
    signature_visibility <- signature_tbl |> dplyr::filter(.data$visibility == FALSE) |> dplyr::distinct(.data$signature_id, .data$visibility) 
    # Check if user has the permission to view the signatures ####
    for(w in base::seq_len(base::nrow(signature_visibility))){
      #w=1;
      signature_access_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_access", 
        return_var = "*", 
        filter_coln_var = c("signature_id", "user_name", "access_type"),
        filter_coln_val = list("signature_id" = signature_visibility$signature_id[w], "user_name" = user_name, "access_type" = c("owner", "editor", "viewer")),
        filter_var_by = c("AND", "AND"),
        check_db_table = TRUE
      ) 
      # If user does not have owner or editor permission, remove from the returned list
      if(base::nrow(signature_access_tbl) == 0){
        signature_tbl <- signature_tbl |> dplyr::filter(!.data$signature_id %in% signature_visibility$signature_id[w])
      }
    }
  }
  
  # Check if signature exists
  if(base::nrow(signature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    SigRepo::verbose("There are no signatures returned from the search parameters.\n")
    
    # Return NULL
    return(NULL)
    
  }else{
    
    # Look up organism ####
    lookup_organism_id <- base::unique(signature_tbl$organism_id)
    
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = c("organism_id", "organism"), 
      filter_coln_var = "organism_id", 
      filter_coln_val = list("organism_id" = lookup_organism_id),
      check_db_table = TRUE
    ) 
    
    # Look up phenotype id ####
    lookup_phenotype_id <- base::unique(signature_tbl$phenotype_id)
    
    phenotype_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "phenotypes", 
      return_var = c("phenotype_id", "phenotype"), 
      filter_coln_var = "phenotype_id", 
      filter_coln_val = list("phenotype_id" = lookup_phenotype_id),
      check_db_table = TRUE
    ) 
    
    # Look up sample_type_id ####
    lookup_sample_type_id <- base::unique(signature_tbl$sample_type_id)
    
    sample_type_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "sample_types", 
      return_var = c("sample_type_id", "sample_type"), 
      filter_coln_var = "sample_type_id", 
      filter_coln_val = list("sample_type_id" = lookup_sample_type_id),
      check_db_table = TRUE
    ) 
    
    # Look up platform_id ####
    lookup_platform_id <- base::unique(signature_tbl$platform_id)
    
    platform_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "platforms", 
      return_var = c("platform_id", "platform_name"), 
      filter_coln_var = "platform_id", 
      filter_coln_val = base::list("platform_id" = lookup_platform_id),
      check_db_table = TRUE
    ) 
    
    # Add variables to table
    signature_tbl <- signature_tbl |> 
      dplyr::left_join(organism_id_tbl, by = "organism_id") |> 
      dplyr::left_join(phenotype_id_tbl, by = "phenotype_id") |> 
      dplyr::left_join(sample_type_id_tbl, by = "sample_type_id") |>
      dplyr::left_join(platform_id_tbl, by = "platform_id")
    
    # Rename table with appropriate column names 
    coln_names <- base::colnames(signature_tbl) |> 
      base::replace(base::match(c("organism_id", "phenotype_id", "sample_type_id", "platform_id"), base::colnames(signature_tbl)), c("organism", "phenotype", "sample_type", "platform_name"))   
    
    # Extract the table with appropriate column names ####
    signature_tbl <- signature_tbl |> dplyr::select(dplyr::all_of(coln_names))
    
    # Create a place holder to store signatures
    omic_signature_list <- base::list()
    
    # Create an omic signature object for each signature id ####
    for(r in 1:base::nrow(signature_tbl)){
      #r=1;
      db_signature_tbl <- signature_tbl |> dplyr::slice(r) 
      
      # Create an OmicSignature object
      omic_signature <- createOmicSignature(
        conn_handler = conn_handler,
        db_signature_tbl = db_signature_tbl
      )
      
      # Append OmicSignature object to overall list
      omic_signature_list <- c(
        omic_signature_list, 
        omic_signature
      )
    }
    
    # Add names to signatures
    base::names(omic_signature_list) <- signature_tbl$signature_name
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))
    
    # Return table
    return(omic_signature_list)
    
  }  
}
