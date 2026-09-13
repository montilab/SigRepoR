#' @title createCollectionMetadata
#' @description Create a metadata object for a signature
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required) 
#' @param omic_collection A collection of OmicSignature objects from 
#' OmicSignature package (required) 
#' 
#' @keywords internal
#' 
#' @export
createCollectionMetadata <- function(
    conn_handler = NULL,
    omic_collection
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  
  # Check if omic_signature is a valid R6 object ####
  # If yes, return whether it has difexp included ####
  SigRepo::checkOmicCollection(
    omic_collection = omic_collection
  )
  
  # Extract metadata from omic_collection ####
  metadata <- omic_collection$metadata
  
  # Create signature metadata table ####
  metadata_tbl <- base::data.frame(
    collection_name = metadata$collection_name,
    description = metadata$description
  )
  
  # Extract omic_sig_list from omic_collection ####
  omic_sig_list <- omic_collection$OmicSigList
  
  # Check required signature fields ####
  purrr::walk(
    base::seq_along(omic_sig_list),
    function(c){
      #c=1;
      SigRepo::checkOmicSignature(
        omic_signature = omic_sig_list[[c]]
      )
    }
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return the metadata tbl ####
  return(metadata_tbl)
  
}






