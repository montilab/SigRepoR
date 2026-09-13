#' @title addGeneticVariantsSignatureSet
#' @description Add GeneticVariants signature feature set to database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param signature_id Database ID of the signature (required) 
#' @param organism_id Database ID of the organism (required) 
#' @param signature_set A Data Frame; must contain the following column names:
#' feature_name, probe_id, score, group_label (required) 
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#'  
#' @keywords internal
#' 
#' @export
addGeneticVariantsSignatureSet <- function(
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
  user_name <- conn_info$user[1]
  
  # Validate signature_id and organism_id ####
  if(!length(signature_id) == 1 || signature_id %in% c(NA, "")){
    suppressWarnings(DBI::dbDisconnect(conn)) 
    stop("\n'signature_id' must have a length of 1 and cannot be empty.\n")
  }
  if(!length(organism_id) == 1 || organism_id %in% c(NA, "")){
    suppressWarnings(DBI::dbDisconnect(conn)) 
    stop("\n'organism_id' must have a length of 1 and cannot be empty.\n")
  }
  
  # Validate signature_set ####
  required_fields <- c('feature_name', 'probe_id', 'score', 'group_label')
  if(!methods::is(signature_set, "data.frame") || nrow(signature_set) == 0){
    suppressWarnings(DBI::dbDisconnect(conn)) 
    stop("\n'signature_set' must be a data frame and cannot be empty.\n")
  }
  if(any(!required_fields %in% colnames(signature_set))){
    suppressWarnings(DBI::dbDisconnect(conn)) 
    stop(sprintf("\n'signature_set' must have the following column names: %s.\n", paste0(required_fields, collapse = ", ")))
  }
  if(any(is.na(signature_set[,required_fields]))){
    suppressWarnings(DBI::dbDisconnect(conn)) 
    stop(sprintf("\nAll required columns in 'signature_set': %s cannot contain NA values.\n", paste0(required_fields, collapse = ", ")))
  }
  
  # Database table names ####
  db_table_name <- "signature_feature_set"
  ref_table <- "genetic_variants_features"
  
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
  } else {
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = "signature_id",
      filter_coln_val = list("signature_id" = signature_id),
      check_db_table = TRUE
    )
  }
  
  if(nrow(signature_tbl) == 0){
    suppressWarnings(DBI::dbDisconnect(conn)) 
    stop(sprintf("\nNo signature_id = '%s' for user = '%s' found in 'signatures' table.\n", signature_id, user_name))
  }
  
  # -------------------------------------------
  # Step 1: Add signature info to table
  # -------------------------------------------
  table <- signature_set |> 
    dplyr::mutate(
      signature_id = signature_id,
      organism_id = organism_id,
      assay_type = "genetic_variants",
      nomenclature_type = "genetic_variant",
      match_status = "resolved"
    )
  
  # Step 2: Create feature hashkey ####
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "feature_hashkey",
    hash_columns = c("feature_name", "organism_id"),
    hash_method = "md5"
  )
  

  
  # Step 3: Pull existing feature hashkeys from DB ####
  existing_hashkeys <- DBI::dbGetQuery(
    conn,
    sprintf("SELECT feature_hashkey FROM %s", ref_table)
  )$feature_hashkey  
  

  
  # Step 4: Identify missing features efficiently ####
  missing_mask <- !table$feature_hashkey %in% existing_hashkeys
  dt_new_features <- table[missing_mask, ]
  
  
  # Step 5: Insert missing features ####
  if(nrow(dt_new_features) > 0){
    SigRepo::verbose(sprintf("Adding %d new GeneticVariants features.\n", nrow(dt_new_features)))
    batch_size <- 10000
    for(i in seq(1, nrow(dt_new_features), by = batch_size)){
      batch <- dt_new_features[i:min(i+batch_size-1, nrow(dt_new_features)), ]
      SigRepo::insert_table_sql(
        conn = conn,
        db_table_name = ref_table,
        table = batch,
        check_db_table = FALSE
      )
    }
  }
  
  
  # Look up feature id by its hash key
  lookup_hashkey <- base::unique(table$feature_hashkey)
  
  # Step 6: Lookup feature IDs ####
  lookup_feature_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = ref_table,
    return_var = c("feature_id", "feature_name", "organism_id", "feature_hashkey"),
    filter_coln_var = "feature_hashkey",
    filter_coln_val = list("feature_hashkey" = table$feature_hashkey),
    check_db_table = TRUE
  )
  
  # Step 7: Merge feature IDs into table ####
  table <- table |>
    dplyr::left_join(
      lookup_feature_id_tbl |> dplyr::select(feature_hashkey, feature_id),
      lookup_feature_id_tbl |>
        dplyr::select(dplyr::all_of(c("feature_hashkey", "feature_id"))),
      by = "feature_hashkey"
    )
  
  
  # Step 8: Create signature-feature hashkey ####
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "sig_feature_hashkey",
    hash_columns = c("signature_id", "feature_id", "assay_type"),
    hash_method = "md5"
  )
  
  # Step 9: Validate & remove duplicates ####
  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table, 
    check_db_table = TRUE
  )
  
  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "sig_feature_hashkey",
    check_db_table = FALSE
  )
  
  # Step 10: Insert signature-feature set ####
  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )
  
  # Disconnect ####
  suppressWarnings(DBI::dbDisconnect(conn)) 
  return(invisible())
}
