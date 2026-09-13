#' @title createSignatureMetadata
#' @description Create a metadata object for a signature
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param omic_signature An OmicSignature R6 object from the
#' OmicSignature package (required).
#' @param verbose Logical; whether to print diagnostic messages. 
#' Defaults to 'TRUE'.
#' 
#' @keywords internal
#'
#' @export
createSignatureMetadata <- function(
    conn_handler = NULL,
    omic_signature,
    verbose = FALSE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  
  # If yes, return whether it has difexp included ####
  has_difexp <- base::ifelse(!base::is.null(omic_signature$difexp), 1, 0)
  
  # If has_difexp = TRUE, then get number of difexp
  if(has_difexp == TRUE){
    # Extract difexp from omic_signature ####
    difexp <- omic_signature$difexp
    # Get number of difexp
    num_of_difexp <- base::nrow(difexp)
  }else{
    num_of_difexp <- 0
  }
  
  # Extract signature table from omic_signature ####
  signature <- omic_signature$signature 
  
  # Create number of up regulated features
  num_up_regulated <- base::length(base::which(base::as.numeric(signature$score) > 0))

  # Create number of up regulated features
  num_down_regulated <- base::length(base::which(as.numeric(signature$score) < 0))
  
  # Extract metadata from omic_signature ####
  metadata <- omic_signature$metadata
  
  # Get signature_name ####
  signature_name <- metadata$signature_name[1]
  
  # Get direction_type ####
  direction_type <- metadata$direction_type[1]
  
  # Get assay_type ####
  assay_type <- metadata$assay_type[1]
  
  # Check if assay type if valid
  if(assay_type %in% c("methylomics")){
    # If assay_type is not valid, throw an error message
    base::suppressWarnings(DBI::dbDisconnect(conn))
    SigRepo::showAssayTypeErrorMessage(
      unknown_values = assay_type
    )
  }

  # Look up organism id (required) ####
  lookup_organism <- metadata$organism[1]
  
  organism_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "organisms", 
    return_var = c("organism_id", "organism"), 
    filter_coln_var = "organism", 
    filter_coln_val = base::list("organism" = lookup_organism),
    check_db_table = TRUE
  ) 
  
  # If organism does not existed in database, throw an error message
  if(base::nrow(organism_id_tbl) == 0){
    # Disconnect from database
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show error message ####
    SigRepo::showOrganismErrorMessage(
      db_table_name = "organisms",
      unknown_values = lookup_organism
    )
  }else{
    organism_id <- organism_id_tbl$organism_id[1]
  }
  
  # Look up phenotype id (required) #####
  lookup_phenotype <- metadata$phenotype[1]
  
  phenotype_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "phenotypes", 
    return_var = c("phenotype", "phenotype_id"), 
    filter_coln_var = "phenotype", 
    filter_coln_val = base::list("phenotype" = lookup_phenotype),
    check_db_table = TRUE
  ) 
  
  # If phenotype does not existed in database, add to database
  if(base::nrow(phenotype_id_tbl) == 0){
    # Add phenotype to database ####
    SigRepo::addPhenotype(
      conn_handler = conn_handler,
      phenotype_tbl = base::data.frame(phenotype = lookup_phenotype),
      verbose = FALSE
    )
    # Get the updated phenotype id
    phenotype_id <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "phenotypes", 
      return_var = c("phenotype", "phenotype_id"), 
      filter_coln_var = "phenotype", 
      filter_coln_val = base::list("phenotype" = lookup_phenotype),
      check_db_table = FALSE
    )$phenotype_id[1]
  }else{
    phenotype_id <- phenotype_id_tbl$phenotype_id[1]
  }
  
  # Look up platform id ####
  lookup_platform <- metadata$platform[1]
  
  # Look up ID
  platform_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "platforms",
    return_var = "platform_id",
    filter_coln_var = "platform_name",
    filter_coln_val = base::list("platform_name" = lookup_platform),
    check_db_table = TRUE
  )

  # If ID not exists in database, throw an error message
  if(base::nrow(platform_id_tbl) == 0){
    # Disconnect from database
    base::suppressWarnings(DBI::dbDisconnect(conn))
    # Show error message ####
    SigRepo::showPlatformErrorMessage(
      db_table_name = "platforms",
      unknown_values = lookup_platform
    )
  }else{
    platform_id <- platform_id_tbl$platform_id[1]
  }

  # Look up sample_type id ####
  lookup_sample_type <- metadata$sample_type[1]
  
  # Look up ID
  sample_type_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "sample_types", 
    return_var = "sample_type_id", 
    filter_coln_var = "sample_type", 
    filter_coln_val = base::list("sample_type" = lookup_sample_type),
    check_db_table = TRUE
  ) 
  
  # If ID not exists in database, throw an error message
  if(base::nrow(sample_type_tbl) == 0){
    # Disconnect from database
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show error message ####
    SigRepo::showSampleTypeErrorMessage(
      db_table_name = "platforms",
      unknown_values = lookup_platform
    )
  }else{
    sample_type_id <- sample_type_tbl$sample_type_id[1]
  }
  
  # covariates ####
  if("covariates" %in% base::names(metadata)){
    if(base::length(metadata$covariates) == 0){
      covariates <- 'NULL'
    }else{
      covariates <- base::paste0(metadata$covariates, collapse = ",")
    }
  }else{
    covariates <- 'NULL'
  }
  
  # description ####
  if("description" %in% base::names(metadata)){
    if(base::length(metadata$description) == 0){
      description <- 'NULL'
    }else{
      description <- metadata$description[1]
    }
  }else{
    description <- 'NULL'
  }
  
  # score_cutoff ####
  if("score_cutoff" %in% base::names(metadata)){
    if(base::length(metadata$score_cutoff) == 0){
      score_cutoff <- 'NULL'
    }else{
      score_cutoff <-  metadata$score_cutoff[1]
    }
  }else{
    score_cutoff <- 'NULL'
  }
  
  # logfc_cutoff ####
  if("logfc_cutoff" %in% base::names(metadata)){
    if(base::length(metadata$logfc_cutoff) == 0){
      logfc_cutoff <- 'NULL'
    }else{
      logfc_cutoff <- metadata$logfc_cutoff[1]
    }
  }else{
    logfc_cutoff <- 'NULL'
  }
  
  # p_value_cutoff ####
  if("p_value_cutoff" %in% base::names(metadata)){
    if(base::length(metadata$p_value_cutoff) == 0){
      p_value_cutoff <- 'NULL'
    }else{
      p_value_cutoff <- metadata$p_value_cutoff[1]
    }
  }else{
    p_value_cutoff <- 'NULL'
  }
  
  # adj_p_cutoff ####
  if("adj_p_cutoff" %in% base::names(metadata)){
    if(base::length(metadata$adj_p_cutoff) == 0){
      adj_p_cutoff <- 'NULL'
    }else{
      adj_p_cutoff <- metadata$adj_p_cutoff[1]
    }
  }else{
    adj_p_cutoff <- 'NULL'
  }
  
  # cutoff_description ####
  if("cutoff_description" %in% base::names(metadata)){
    if(base::length(metadata$cutoff_description) == 0){
      cutoff_description <- 'NULL'
    }else{
      cutoff_description <- metadata$cutoff_description[1]
    }
  }else{
    cutoff_description <- 'NULL'
  }
  
  # keywords ####
  if("keywords" %in% base::names(metadata)){
    if(base::length(metadata$keywords) == 0){
      keywords <- 'NULL'
    }else{
      keywords <- base::paste0(metadata$keywords, collapse = ",")
      # Create keyword table
      keyword_tbl <- base::data.frame(
        keyword = base::strsplit(metadata$keywords, split = ",", fixed = TRUE) |> base::unlist() |> base::trimws(),
        stringsAsFactors = FALSE
      )
      # Add keyword to database
      SigRepo::addKeyword(
        conn_handler = conn_handler, 
        keyword_tbl = keyword_tbl,
        verbose = FALSE
      )
    }
  }else{
    keywords <- 'NULL'
  }
  
  # PMID ####
  if("PMID" %in% base::names(metadata)){
    if(base::length(metadata$PMID) == 0){
      PMID <- 'NULL'
    }else{
      PMID <- base::paste0(metadata$PMID, collapse = ",")
    }
  }else{
    PMID <- 'NULL'
  }
  
  # year ####
  if("year" %in% base::names(metadata)){
    if(base::length(metadata$year) == 0){
      year <- 'NULL'
    }else{
      year <- base::paste0(metadata$year, collapse = ",")
    }
  }else{
    year <- 'NULL'
  }
  
  # others ####
  if("others" %in% base::names(metadata)){
    if(base::length(metadata$others) == 0){
      others <- 'NULL'
    }else{
      others <- base::seq_along(metadata$others) |> 
        purrr::map_chr(
          function(l){
            #l=1;
            list_name <- base::names(metadata$others)[l]
            base::paste0(list_name, ": ", base::paste0("<", metadata$others[[l]], ">", collapse = ","))
          }
        ) |> 
        base::paste0(collapse = ";")
    }
  }else{
    others <- 'NULL'
  } 
  
  # Create signature metadata table ####
  metadata_tbl <- base::data.frame(
    signature_name = signature_name,
    organism_id = organism_id,
    direction_type = direction_type,
    assay_type = assay_type,
    phenotype_id = phenotype_id,
    platform_id = platform_id,
    sample_type_id = sample_type_id,
    covariates = covariates,
    description = description,
    score_cutoff = score_cutoff,
    logfc_cutoff = logfc_cutoff,
    p_value_cutoff = p_value_cutoff,
    adj_p_cutoff = adj_p_cutoff,
    cutoff_description = cutoff_description,
    keywords = keywords,
    PMID = PMID,
    year = year,
    others = others,
    has_difexp = has_difexp,
    num_of_difexp = num_of_difexp,
    num_up_regulated = num_up_regulated,
    num_down_regulated = num_down_regulated
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return the metadata tbl
  return(metadata_tbl)
  
}




