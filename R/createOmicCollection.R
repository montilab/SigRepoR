#' @title createOmicCollection
#' @description Get the collection set uploaded by a specific user in the database.
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param db_collection_tbl A Data Frame; must have the following column name(s):
#' 'collecion_id'.
#'
#' @import OmicSignature
#' 
#' @keywords internal
#' 
#' @export
createOmicCollection <- function(
    conn_handler = NULL,
    db_collection_tbl
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  
  # Check if table is a data frame object and not empty
  if(!methods::is(db_collection_tbl, "data.frame") || base::length(db_collection_tbl) == 0){
    # Disconnect from database ####
    DBI::dbDisconnect(conn)    
    # Show message
    base::stop("'db_collection_tbl' must be a data frame and cannot be empty.")
  }
  
  # Create metadata
  metadata <- db_collection_tbl |> base::as.list()
  
  # Look up signatures in the signature collection table
  signature_collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signature_collection_access", 
    return_var = "*", 
    filter_coln_var = "collection_id", 
    filter_coln_val = base::list("collection_id" = base::unique(db_collection_tbl$collection_id)),
    check_db_table = TRUE
  ) 
  
  # Add signatures to collection table
  signature_collection_tbl <- db_collection_tbl |> 
    dplyr::left_join(signature_collection_tbl, by = "collection_id")  
  
  # Create a place holder to store signatures
  omic_signature_list <- base::list()
  
  # Create an omic signature object for each signature id ####
  for(r in 1:base::nrow(signature_collection_tbl)){
    #r=1;
    db_signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      filter_coln_var = "signature_id", 
      filter_coln_val = base::list("signature_id" = signature_collection_tbl$signature_id[r]),
      check_db_table = TRUE
    ) 
    
    # Look up organism ####
    lookup_organism_id <- base::unique(db_signature_tbl$organism_id)
    
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = c("organism_id", "organism"), 
      filter_coln_var = "organism_id", 
      filter_coln_val = base::list("organism_id" = lookup_organism_id),
      check_db_table = TRUE
    ) 
    
    # Look up phenotype id ####
    lookup_phenotype_id <- base::unique(db_signature_tbl$phenotype_id)
    
    phenotype_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "phenotypes", 
      return_var = c("phenotype_id", "phenotype"), 
      filter_coln_var = "phenotype_id", 
      filter_coln_val = base::list("phenotype_id" = lookup_phenotype_id),
      check_db_table = TRUE
    ) 
    
    # Look up sample_type_id ####
    lookup_sample_type_id <- base::unique(db_signature_tbl$sample_type_id)
    
    sample_type_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "sample_types", 
      return_var = c("sample_type_id", "sample_type"), 
      filter_coln_var = "sample_type_id", 
      filter_coln_val = base::list("sample_type_id" = lookup_sample_type_id),
      check_db_table = TRUE
    ) 
    
    # Look up platform_id ####
    lookup_platform_id <- base::unique(db_signature_tbl$platform_id)
    
    platform_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "platforms", 
      return_var = c("platform_id", "platform_name"), 
      filter_coln_var = "platform_id", 
      filter_coln_val = base::list("platform_id" = lookup_platform_id),
      check_db_table = TRUE
    ) 
    
    # Add variables to table
    db_signature_tbl <- db_signature_tbl |> 
      dplyr::left_join(organism_id_tbl, by = "organism_id") |> 
      dplyr::left_join(phenotype_id_tbl, by = "phenotype_id") |> 
      dplyr::left_join(sample_type_id_tbl, by = "sample_type_id") |>
      dplyr::left_join(platform_id_tbl, by = "platform_id")
    
    # Rename table with appropriate column names 
    coln_names <- base::colnames(db_signature_tbl) |> 
      base::replace(base::match(c("organism_id", "phenotype_id", "sample_type_id", "platform_id"), base::colnames(db_signature_tbl)), c("organism", "phenotype", "sample_type", "platform_name"))
    
    # Extract the table with appropriate column names ####
    db_signature_tbl <- db_signature_tbl |> dplyr::select(dplyr::all_of(coln_names))
    
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
  base::names(omic_signature_list) <- db_signature_tbl$signature_name
  
  # Create an OmicSignatureCollection object
  OmicCol <- base::tryCatch({
    OmicSignature::OmicSignatureCollection$new(
      metadata = metadata,
      OmicSigList = omic_signature_list
    )
  }, error = function(e){
    base::stop("OmicSignatureCollection Error: ", e, "\n")
  })
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return collection
  return(OmicCol)
  
}






