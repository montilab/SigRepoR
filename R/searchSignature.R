#' @title searchSignature
#' @description Search for a list of signatures in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param signature_id Database ID of the signatures to be looked up by.
#' @param signature_name Name of the signatures to be looked up by.
#' @param user_name The name of the user to be looked up by.
#' @param organism The organism to be looked up by.
#' @param phenotype The phenotype to be looked up by.
#' @param sample_type The sample type to be looked up by.
#' @param platform_name The platform name to be looked up by.
#' @param verbose Logical; whether or not to print the diagnostic messages.
#' Defaults to 'TRUE'.
#'
#' @examples
#'
#' \dontrun{
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
#' # Search for a list of signatures in the database
#' SigRepo::searchSignature(
#'   conn_handler = conn_handler,
#'   signature_id = "test_signature",
#'   user_name = "John_Doe"
#' )
#'
#' }
#'
#'
#' @export
searchSignature <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    user_name = NULL,
    organism = NULL,
    phenotype = NULL,
    sample_type = NULL,
    platform_name = NULL,
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

  # Resolve organism/phenotype/sample_type/platform_name filters against
  # their (small) vocabulary tables into id values first, so the main
  # signatures query below can filter on organism_id/phenotype_id/
  # sample_type_id/platform_id directly instead of pulling every signature
  # and filtering in R. ####
  resolve_id_filter <- function(value, db_table_name, name_col, id_col){
    value <- base::unique(value[base::which(!value %in% c(NA, ""))])
    if(base::length(value) == 0) return(NULL)

    id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      return_var = id_col,
      filter_coln_var = name_col,
      filter_coln_val = stats::setNames(base::list(value), name_col),
      check_db_table = TRUE
    )

    id_tbl[[id_col]]
  }

  organism_id <- resolve_id_filter(organism, "organisms", "organism", "organism_id")
  phenotype_id <- resolve_id_filter(phenotype, "phenotypes", "phenotype", "phenotype_id")
  sample_type_id <- resolve_id_filter(sample_type, "sample_types", "sample_type", "sample_type_id")
  platform_id <- resolve_id_filter(platform_name, "platforms", "platform_name", "platform_id")

  # If a vocabulary filter was supplied but didn't resolve to any id, the
  # search is guaranteed to match no signatures. Force that outcome with an
  # impossible signature_id filter (0 -- signature_id is an AUTO_INCREMENT
  # column starting at 1) rather than special-casing the return shape below. ####
  impossible_match <-
    (base::length(organism) > 0 && base::length(organism_id) == 0) ||
    (base::length(phenotype) > 0 && base::length(phenotype_id) == 0) ||
    (base::length(sample_type) > 0 && base::length(sample_type_id) == 0) ||
    (base::length(platform_name) > 0 && base::length(platform_id) == 0)

  # Build the signatures WHERE clause directly from the caller's search
  # parameters, pushed down to SQL. ####
  filter_list <- base::list(
    "signature_id" = base::unique(signature_id),
    "signature_name" = base::unique(signature_name),
    "user_name" = base::unique(user_name),
    "organism_id" = organism_id,
    "phenotype_id" = phenotype_id,
    "sample_type_id" = sample_type_id,
    "platform_id" = platform_id
  )

  if(impossible_match){
    filter_list[["signature_id"]] <- 0L
  }

  has_filter_value <- base::vapply(
    filter_list,
    function(x) base::length(x) > 0 && !base::all(x %in% c(NA, "")),
    logical(1)
  )
  filter_list <- filter_list[has_filter_value]

  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = base::names(filter_list),
    filter_coln_val = filter_list,
    filter_var_by = if(base::length(filter_list) > 1) base::rep("AND", base::length(filter_list) - 1) else NULL,
    check_db_table = TRUE
  )

  # Check if signature exists
  if(base::nrow(signature_tbl) == 0){

    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))

    # Show message
    SigRepo::verbose("There are no signatures returned from the search parameters.\n")

    # Return table
    return(signature_tbl)

  }else{

    # Look up organism id -- scoped to the ids present in the already-
    # filtered result, not the whole organisms table. ####
    lookup_organism_id <- base::unique(signature_tbl$organism_id)

    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "organisms",
      return_var = c("organism_id", "organism"),
      filter_coln_var = "organism_id",
      filter_coln_val = base::list("organism_id" = lookup_organism_id),
      check_db_table = TRUE
    )

    # Look up phenotype id ####
    lookup_phenotype_id <- base::unique(signature_tbl$phenotype_id)

    phenotype_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "phenotypes",
      return_var = c("phenotype_id", "phenotype"),
      filter_coln_var = "phenotype_id",
      filter_coln_val = base::list("phenotype_id" = lookup_phenotype_id),
      check_db_table = TRUE
    )

    # Look up sample_type_id ####
    lookup_sample_type_id <- base::unique(signature_tbl$sample_type_id)

    sample_type_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "sample_types",
      return_var = c("sample_type_id", "sample_type"),
      filter_coln_var = "sample_type_id",
      filter_coln_val = base::list("sample_type_id" = lookup_sample_type_id),
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

    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))

    # Return table
    return(signature_tbl)

  }
}
