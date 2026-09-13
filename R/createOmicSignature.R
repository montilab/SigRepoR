# Internal helpers for reconstructing user-facing OmicSignature objects.
parseRetrievedMetadataList <- function(value, split_values = TRUE){
  if(base::length(value) == 0 || base::all(value %in% c(NA, "", "NULL"))){
    return(NULL)
  }

  if(!split_values){
    return(utils::type.convert(base::as.character(value[1]), as.is = TRUE))
  }

  parsed_value <- base::strsplit(base::as.character(value[1]), split = ",", fixed = TRUE) |>
    base::unlist() |>
    base::trimws()

  parsed_value <- parsed_value[!parsed_value %in% c(NA, "", "NULL")]

  if(base::length(parsed_value) == 0){
    return(NULL)
  }

  utils::type.convert(parsed_value, as.is = TRUE)
}

parseRetrievedScalarString <- function(value){
  if(base::length(value) == 0 || base::all(value %in% c(NA, "", "NULL"))){
    return(NULL)
  }

  base::as.character(value[1])
}

parseRetrievedOthers <- function(value){
  if(base::length(value) == 0 || base::all(value %in% c(NA, "", "NULL"))){
    return(NULL)
  }

  others_entries <- base::strsplit(base::as.character(value[1]), split = ";", perl = TRUE) |>
    base::unlist() |>
    base::trimws()

  others_entries <- others_entries[others_entries != ""]

  if(base::length(others_entries) == 0){
    return(NULL)
  }

  others <- purrr::map(
    others_entries,
    function(entry){
      entry_name <- base::sub(":.*$", "", entry, perl = TRUE) |> base::trimws()
      entry_values <- base::sub("^[^:]+:\\s*", "", entry, perl = TRUE) |>
        base::strsplit(split = ",", fixed = TRUE) |>
        base::unlist() |>
        base::trimws() |>
        base::gsub(pattern = "^<|>$", replacement = "", x = _, perl = TRUE)

      entry_values <- entry_values[entry_values != ""]

      if(base::length(entry_values) == 0){
        return(NULL)
      }

      utils::type.convert(entry_values, as.is = TRUE)
    }
  )

  base::names(others) <- purrr::map_chr(
    others_entries,
    function(entry){
      base::sub(":.*$", "", entry, perl = TRUE) |> base::trimws()
    }
  )

  others <- others[!purrr::map_lgl(others, base::is.null)]

  if(base::length(others) == 0){
    return(NULL)
  }

  others
}

buildRetrievedMetadata <- function(db_signature_tbl){
  metadata <- base::list(
    signature_name = db_signature_tbl$signature_name[1],
    organism = db_signature_tbl$organism[1],
    direction_type = db_signature_tbl$direction_type[1],
    assay_type = db_signature_tbl$assay_type[1],
    phenotype = db_signature_tbl$phenotype[1]
  )

  optional_metadata <- base::list(
    platform = parseRetrievedMetadataList(db_signature_tbl$platform_name, split_values = FALSE),
    sample_type = parseRetrievedMetadataList(db_signature_tbl$sample_type, split_values = FALSE),
    covariates = parseRetrievedScalarString(db_signature_tbl$covariates),
    description = parseRetrievedMetadataList(db_signature_tbl$description, split_values = FALSE),
    score_cutoff = parseRetrievedMetadataList(db_signature_tbl$score_cutoff, split_values = FALSE),
    logfc_cutoff = parseRetrievedMetadataList(db_signature_tbl$logfc_cutoff, split_values = FALSE),
    p_value_cutoff = parseRetrievedMetadataList(db_signature_tbl$p_value_cutoff, split_values = FALSE),
    adj_p_cutoff = parseRetrievedMetadataList(db_signature_tbl$adj_p_cutoff, split_values = FALSE),
    cutoff_description = parseRetrievedMetadataList(db_signature_tbl$cutoff_description, split_values = FALSE),
    keywords = parseRetrievedScalarString(db_signature_tbl$keywords),
    PMID = parseRetrievedScalarString(db_signature_tbl$PMID),
    year = parseRetrievedScalarString(db_signature_tbl$year),
    others = parseRetrievedOthers(db_signature_tbl$others)
  )

  optional_metadata <- optional_metadata[!purrr::map_lgl(optional_metadata, base::is.null)]

  if("others" %in% base::names(optional_metadata) &&
     "metabolomics_nomenclature" %in% base::names(optional_metadata$others)){
    optional_metadata$others$metabolomics_nomenclature <- NULL

    if(base::length(optional_metadata$others) == 0){
      optional_metadata$others <- NULL
      optional_metadata <- optional_metadata[!purrr::map_lgl(optional_metadata, base::is.null)]
    }
  }

  c(metadata, optional_metadata)
}

sanitizeRetrievedSignature <- function(signature, direction_type){
  signature_columns <- c("probe_id", "feature_name", "score", "group_label")
  keep_columns <- signature_columns[signature_columns %in% base::colnames(signature)]

  signature <- signature |>
    dplyr::select(dplyr::all_of(keep_columns))

  if("group_label" %in% base::colnames(signature) &&
     direction_type %in% c("bi-directional", "categorical")){
    signature <- signature |>
      dplyr::mutate(
        group_label = base::factor(
          .data$group_label,
          levels = base::unique(.data$group_label),
          labels = base::unique(.data$group_label)
        )
      )
  }

  signature
}

# signature_feature_set stores only feature_id, so a retrieved signature's
# feature_name carries the reference table's spelling, while the difexp keeps
# the uploaded spelling. Uploads match features case-insensitively, so the two
# can differ only in case, and OmicSignature$new() then rejects the pair. Where
# a signature name is missing from the difexp but the difexp row with the same
# probe_id has the same name ignoring case, use the difexp spelling.
alignSignatureFeatureNames <- function(signature, difexp){
  key_columns <- c("probe_id", "feature_name")
  if(base::is.null(difexp) ||
     !base::all(key_columns %in% base::colnames(signature)) ||
     !base::all(key_columns %in% base::colnames(difexp))){
    return(signature)
  }

  mismatched <- base::which(!signature$feature_name %in% difexp$feature_name)
  if(base::length(mismatched) == 0) return(signature)

  difexp_names <- base::as.character(difexp$feature_name)[
    base::match(signature$probe_id[mismatched], difexp$probe_id)
  ]
  same_name <- (base::tolower(difexp_names) == base::tolower(signature$feature_name[mismatched])) %in% TRUE
  signature$feature_name[mismatched[same_name]] <- difexp_names[same_name]

  signature
}

#' @title createOmicSignature
#' @description Get the signature set uploaded by a specific user in the database.
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required)
#' @param db_signature_tbl A Data Frame; must contain the following column names:
#' 'signature_id'
#' @param difexp Optional pre-loaded difexp data frame. Only used when
#'   \code{fetch_difexp = FALSE}; lets a caller that already has the difexp on
#'   hand (e.g. the SigRepo API, reading it from disk) inject it instead of
#'   having createOmicSignature fetch it over HTTP.
#' @param fetch_difexp Logical; when \code{TRUE} (default, the original
#'   behavior) difexp is retrieved from the SigRepo API's \code{get_difexp}
#'   endpoint. Set \code{FALSE} to skip that HTTP round-trip and use the
#'   \code{difexp} argument as-is -- required when calling this from inside the
#'   API process itself, where an HTTP call back to the same single-process
#'   server would deadlock.
#'
#' @import OmicSignature
#'
#' @keywords internal
#'
createOmicSignature <- function(
    conn_handler = NULL,
    db_signature_tbl,
    difexp = NULL,
    fetch_difexp = TRUE
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Check if table is a data frame object and not empty
  if(!methods::is(db_signature_tbl, "data.frame") || base::nrow(db_signature_tbl) != 1){
    # Disconnect from database ####
    DBI::dbDisconnect(conn)    
    # Show message
    base::stop("\n'db_signature_tbl' must be a data frame and cannot be empty.\n")
  }
  
  # Create metadata
  metadata <- buildRetrievedMetadata(db_signature_tbl = db_signature_tbl)
  metadata_lookup <- metadata
  metadata_lookup$others <- parseRetrievedOthers(db_signature_tbl$others)

  # Get assay_type
  assay_type <- db_signature_tbl$assay_type[1]

  # Get reference table
  if(assay_type == "transcriptomics"){
    ref_table <- "transcriptomics_features"
  }else if(assay_type == "proteomics"){
    ref_table <- "proteomics_features"
  }else if(assay_type == "metabolomics"){
    metabolomics_config <- resolveMetabolomicsFeatureConfig(metadata = metadata_lookup)
    ref_table <- metabolomics_config$reference_table
  }else if(assay_type == "methylomics"){
    SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
  }else if(assay_type == "genetic_variants"){
    ref_table <- "genetic_variants_features"
  }
  
  # If signature has difexp, get a copy by its signature hash key ####
  # When fetch_difexp = FALSE the caller has already supplied `difexp`
  # directly (e.g. the API loads it from disk to avoid an HTTP round-trip back
  # to itself), so use it as-is and skip the get_difexp API call.
  if(!fetch_difexp){
    # difexp keeps whatever was passed in (possibly NULL).
  }else if(db_signature_tbl$has_difexp[1] == TRUE){
    # Get API URL
    api_url <- SigRepo::build_api_url(
      conn_handler = conn_handler,
      endpoint = "get_difexp",
      query = base::list(
        api_key = conn_info$api_key[1],
        signature_hashkey = db_signature_tbl$signature_hashkey[1]
      )
    )
    # Get difexp in database
    res <- httr::GET(url = api_url)
    # Check status code
    if(res$status_code != 200){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Show message
      SigRepo::stop_for_api_error(
        res = res,
        api_url = api_url,
        action = "retrieve the difexp table from the SigRepo API"
      )
    }else{
      difexp <- jsonlite::fromJSON(jsonlite::fromJSON(base::rawToChar(res$content)))
    }
  }else{
    difexp <- NULL
  }
  
  # Create signature set
  signature <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signature_feature_set", 
    return_var = "*", 
    filter_coln_var = "signature_id", 
    filter_coln_val = base::list("signature_id" = db_signature_tbl$signature_id[1]),
    check_db_table = TRUE
  )
  
  # Look up feature_id ####
  lookup_feature_id <- base::unique(signature$feature_id[!base::is.na(signature$feature_id)])
  
  if (assay_type == "metabolomics" && base::length(lookup_feature_id) > 0) {
    feature_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = ref_table,
      return_var = c("metabolite_id", "refmet_id", "refmet_name", "hmdb_id", "smiles", "inchikey"),
      filter_coln_var = "metabolite_id",
      filter_coln_val = list("metabolite_id" = lookup_feature_id),
      check_db_table = TRUE
    ) |>
      dplyr::rename(feature_id = .data$metabolite_id)

    if ("feature_name" %in% base::colnames(signature) && base::any(!signature$feature_name %in% c(NA, "", "NULL"))) {
      feature_id_tbl <- feature_id_tbl |>
        dplyr::mutate(resolved_feature_name = NA_character_)
    } else if (metabolomics_config$feature_database == "refmet") {
      feature_id_tbl <- feature_id_tbl |>
        dplyr::mutate(resolved_feature_name = .data$refmet_name)
    } else if (metabolomics_config$feature_database == "refmet_id") {
      xref_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = metabolomics_config$xref_table,
        return_var = c("metabolite_id", "source_db", "source_value", "is_primary"),
        filter_coln_var = c("source_db", "metabolite_id"),
        filter_coln_val = list("source_db" = metabolomics_config$xref_source_db, "metabolite_id" = lookup_feature_id),
        filter_var_by = "AND",
        check_db_table = TRUE
      ) |>
        dplyr::arrange(dplyr::desc(.data$is_primary), .data$source_value) |>
        dplyr::group_by(.data$metabolite_id) |>
        dplyr::summarise(
          resolved_feature_name = base::paste0(base::unique(.data$source_value), collapse = ","),
          .groups = "drop"
        ) |>
        dplyr::rename(feature_id = .data$metabolite_id)

      feature_id_tbl <- feature_id_tbl |>
        dplyr::left_join(
          xref_tbl,
          by = "feature_id"
        )
    } else {
      xref_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = metabolomics_config$xref_table,
        return_var = c("metabolite_id", "source_db", "source_value", "is_primary"),
        filter_coln_var = c("source_db", "metabolite_id"),
        filter_coln_val = list("source_db" = metabolomics_config$xref_source_db, "metabolite_id" = lookup_feature_id),
        filter_var_by = "AND",
        check_db_table = TRUE
      ) |>
        dplyr::arrange(dplyr::desc(.data$is_primary), .data$source_value) |>
        dplyr::group_by(.data$metabolite_id) |>
        dplyr::summarise(
          resolved_feature_name = base::paste0(base::unique(.data$source_value), collapse = ","),
          .groups = "drop"
        ) |>
        dplyr::rename(feature_id = .data$metabolite_id)

      feature_id_tbl <- feature_id_tbl |>
        dplyr::left_join(
          xref_tbl,
          by = "feature_id"
        )
    }
  } else if (assay_type != "metabolomics") {
    feature_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = ref_table, 
      return_var = c("feature_id", "feature_name"), 
      filter_coln_var = "feature_id", 
      filter_coln_val = list("feature_id" = lookup_feature_id),
      check_db_table = TRUE
    )
  } else {
    feature_id_tbl <- base::data.frame(
      feature_id = numeric(),
      resolved_feature_name = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # Add variables to table
  signature <- signature |> 
    dplyr::left_join(
      feature_id_tbl,
      by = "feature_id"
    )

  if (assay_type == "metabolomics") {
    ambiguity_lookup_tbl <- base::data.frame(
      sig_feature_hashkey = character(),
      ambiguity_feature_name = character(),
      stringsAsFactors = FALSE
    )

    all_tables <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
    if ("signature_feature_set_ambiguity" %in% all_tables[[1]] &&
        "sig_feature_hashkey" %in% base::colnames(signature)) {
      ambiguity_hashkeys <- base::unique(signature$sig_feature_hashkey[
        base::is.na(signature$feature_id) & !base::is.na(signature$sig_feature_hashkey)
      ])

      if (base::length(ambiguity_hashkeys) > 0) {
        ambiguity_lookup_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "signature_feature_set_ambiguity",
          return_var = c("sig_feature_hashkey", "candidate_metabolite_id"),
          filter_coln_var = "sig_feature_hashkey",
          filter_coln_val = base::list("sig_feature_hashkey" = ambiguity_hashkeys),
          check_db_table = FALSE
        ) |>
          dplyr::group_by(.data$sig_feature_hashkey) |>
          dplyr::summarise(
            ambiguity_feature_name = base::paste0(
              "ambiguous_metabolite_ids:",
              base::paste0(base::unique(.data$candidate_metabolite_id), collapse = ",")
            ),
            .groups = "drop"
          )
      }
    }

    signature <- signature |>
      dplyr::left_join(
        ambiguity_lookup_tbl,
        by = "sig_feature_hashkey"
      )

    if ("feature_name" %in% base::colnames(signature)) {
      signature <- signature |>
        dplyr::mutate(
          feature_name = dplyr::if_else(
            base::is.na(.data$feature_name) | .data$feature_name %in% c("", "NULL"),
            dplyr::coalesce(.data$resolved_feature_name, .data$ambiguity_feature_name),
            .data$feature_name
          )
        )
    } else {
      signature <- signature |>
        dplyr::mutate(feature_name = dplyr::coalesce(.data$resolved_feature_name, .data$ambiguity_feature_name))
    }
  }
  
  # Rename table with appropriate column names 
  if(!"feature_name" %in% base::colnames(signature) && "feature_id" %in% base::colnames(signature)){
    base::colnames(signature)[base::match("feature_id", base::colnames(signature))] <- "feature_name"
  }

  # Extract the user-facing table ####
  signature <- sanitizeRetrievedSignature(
    signature = signature,
    direction_type = metadata$direction_type[1]
  )
  
  # Extract difexp with appropriate column names ####
  # Mirrors sanitizeRetrievedSignature()'s guard above: only touch group_label
  # when it's both present (uni-directional difexp rows may not have it) and
  # semantically meaningful for this signature's direction_type. Referencing
  # .data$group_label unconditionally errors ("Column not found") for
  # uni-directional signatures whose difexp table never got a group_label
  # column in the first place.
  if(db_signature_tbl$has_difexp[1] == TRUE){
    if("group_label" %in% base::colnames(difexp) &&
       metadata$direction_type[1] %in% c("bi-directional", "categorical")){
      difexp <- difexp |>
        dplyr::mutate(
          group_label = base::factor(
            .data$group_label,
            levels = base::unique(.data$group_label),
            labels = base::unique(.data$group_label)
          )
        )
    }
  }

  # Normalize probe_id to a single type across signature and difexp ####
  # signature$probe_id always comes back as character (the underlying
  # 'signature_feature_set.probe_id' column is VARCHAR). difexp$probe_id
  # round-trips through the get_difexp API as JSON, so it keeps whatever
  # type it was originally uploaded with (e.g. integer). OmicSignature$new()
  # compares these two columns with %in%, which normally coerces safely, but
  # only if both sides are coerced the same way; forcing both to character
  # here removes any dependence on that implicit coercion.
  if("probe_id" %in% base::colnames(signature)){
    signature <- signature |>
      dplyr::mutate(probe_id = base::as.character(.data$probe_id))
  }
  if(!base::is.null(difexp) && "probe_id" %in% base::colnames(difexp)){
    difexp <- difexp |>
      dplyr::mutate(probe_id = base::as.character(.data$probe_id))
  }

  # Reconcile feature_name spelling between signature and difexp ####
  signature <- alignSignatureFeatureNames(signature = signature, difexp = difexp)

  # Create the OmicSignature object
  OmS <- base::tryCatch({
    OmicSignature::OmicSignature$new(
      metadata = metadata,
      signature = signature,
      difexp = difexp
    )
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("OmicSignature Error: ", e, "\n")
  })
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return Signature
  return(OmS)
  
}
