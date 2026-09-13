#' @title addProteomicsSignatureSet
#' @description Add proteomics signature feature set to database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param signature_id Database ID of the signature (required) 
#' @param organism_id Database ID of the organism (required) 
#' @param signature_set A Data Frame; must contain the following column names:
#' feature_name, probe_id, score, group_label (required) 
#' @param verbose Logical; whether to print diagnostic messages. 
#' Defaults to 'TRUE'.
#' 
#' @export
addProteomicsSignatureSet <- function(
    conn_handler = NULL,
    signature_id,
    organism_id,
    signature_set,
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
  
  # Check signature_id ####
  if(!base::length(signature_id) == 1 || signature_id %in% c(NA, "")){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show error message
    base::stop("\n'signature_id' must have a length of 1 and cannot be empty.\n")
  }
  
  # Check organism_id ####
  if(!base::length(organism_id) == 1 || organism_id %in% c(NA, "")){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show error message
    base::stop("\n'organism_id' must have a length of 1 and cannot be empty.\n")
  }
  
  # Check if signature is a data frame ####
  if(!methods::is(signature_set, "data.frame") || base::length(signature_set) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show error message
    base::stop("\n'signature_set' must be a data frame and cannot be empty.\n")
  }
  
  # Check required signature fields ####
  signature_fields <- c('feature_name', 'probe_id', 'score', 'group_label')
  
  if(base::any(!signature_fields %in% base::colnames(signature_set))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show error message
    base::stop(base::sprintf("\n'signature_set' must have the following column names: %s.\n", base::paste0(signature_fields, collapse = ", ")))
  }
  
  # Make sure required column fields do not have any empty values ####
  if(base::any(base::is.na(signature_set[,signature_fields]) == TRUE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show error message
    base::stop(base::sprintf("\nAll required column names in 'signature_set': %s cannot contain any empty values.\n", base::paste0(signature_fields, collapse = ", ")))
  }
  
  # Define table in database ####
  db_table_name <- "signature_feature_set"
  
  # Define reference table in database
  ref_table <- "proteomics_features"
  
  # Check if signature exists ####
  if(user_role != "admin"){
    
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = c("signature_id", "user_name"),
      filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name),
      filter_var_by = "AND",
      check_db_table = TRUE
    )
    
  }else{
    
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = "signature_id",
      filter_coln_val = list("signature_id" = signature_id),
      check_db_table = TRUE
    )
    
  }
  
  # If signature exists, return the signature table else throw an error message
  if(base::nrow(signature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show error message
    base::stop(base::sprintf("\nThere is no signature_id = '%s' belonged to user = '%s' in the 'signatures' table of the SigRepo database.\n", signature_id, user_name))
    
  }else{
    
    # Create signature set table to look up feature id
    table <- signature_set |> 
      dplyr::mutate(
        signature_id = signature_id,
        organism_id = organism_id,
        assay_type = "proteomics",
        nomenclature_type = "uniprot",
        match_status = "resolved"
      ) 
    
    # Create a hash key to look up feature id in the transcriptomics reference table ####
    table <- SigRepo::createHashKey(
      table = table,
      hash_var = "feature_hashkey",
      hash_columns = c("feature_name", "organism_id"),
      hash_method = "md5"
    )
    
    # Look up feature id by its hash key
    lookup_hashkey <- base::unique(table$feature_hashkey)
    
    lookup_feature_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = ref_table, 
      return_var = c("feature_id", "feature_name", "organism_id", "feature_hashkey"),
      filter_coln_var = "feature_hashkey",
      filter_coln_val = list("feature_hashkey" = lookup_hashkey),
      check_db_table = TRUE
    )
    
    # If any ID is missing, produce an error message
    if(base::nrow(lookup_feature_id_tbl) != base::length(lookup_hashkey)){
      
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      
      # Get the unknown values
      unknown_values <- table$feature_name[base::which(!table$feature_hashkey %in% lookup_feature_id_tbl$feature_hashkey)]
      
      # Show error message 
      SigRepo::showProteomicsErrorMessage(
        db_table_name = ref_table,
        unknown_values = unknown_values
      )
      
      # Return a list of unknown values
      return(base::data.frame(table = ref_table, "unknown_values" = unknown_values))
      
    }
    
    # Add feature id to table
    table <- table |> 
      dplyr::left_join(
        lookup_feature_id_tbl |> dplyr::select(c("feature_hashkey", "feature_id")),
        by = "feature_hashkey"
      )
    
    # Create a hash key to look up signature feature set in database ####
    table <- SigRepo::createHashKey(
      table = table,
      hash_var = "sig_feature_hashkey",
      hash_columns = c("signature_id", "feature_id", "assay_type", "probe_id"),
      hash_method = "md5"
    )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn,
      db_table_name = db_table_name,
      table = table, 
      check_db_table = TRUE
    )
    
    # Remove duplicates from table before inserting into database ####
    table <- SigRepo::removeDuplicates(
      conn = conn,
      db_table_name = db_table_name,
      table = table,
      coln_var = "sig_feature_hashkey",
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = db_table_name, 
      table = table,
      check_db_table = FALSE
    )  
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return 
    return(base::invisible())
    
  }  
}
