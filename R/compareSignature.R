#' Compare signatures from the SigRepo database
#'
#' @description
#' A SigRepo front end to \code{OmicSignature::compare_omic_signatures()}. It
#' takes every argument that function takes, with the same defaults, and adds
#' a way to assemble the two signature lists from the database.
#'
#' Each list is built from up to three sources, kept in this order: database
#' signature ids (\code{signature_ids}), database signature names
#' (\code{signature_names}), and OmicSignature objects supplied directly
#' (\code{omic_signatures}). The \code{*2} arguments build the optional second
#' list, the counterpart of \code{sig_list2}. With only the first list every
#' signature is compared against every other one (a self-comparison, which
#' needs at least two signatures). With both lists each signature in the
#' first is compared against each in the second, giving rectangular result
#' matrices with the first list as rows and the second as columns; one
#' signature per side is enough. For \code{method = "ks_rank"},
#' \code{"ks_score"} and \code{"gsea"} the second list is the ranking side and
#' needs bi-directional signatures with a difexp table, exactly as for
#' \code{sig_list2}.
#'
#' Ids and names are resolved with \code{searchSignature()} and then fetched
#' one at a time with \code{getSignature()}, so the outcome of every request
#' is known. Ids or names that do not exist, and signatures the connected
#' account is not allowed to see, are reported in a warning and left out, and
#' the comparison goes ahead on whatever could be assembled for that list;
#' only when that leaves the list empty is it an error instead. Names are
#' matched case-insensitively, and a name shared by several users' signatures
#' matches all of them. A signature requested by both id and name is included
#' once. Fetched signatures are named by their \code{signature_name}; because
#' names are only unique per user, fetched signatures that share a name are
#' told apart as \code{"name (id N)"}. Supplied objects keep their list
#' names, or fall back to their metadata \code{signature_name}.
#'
#' The rank-based methods rank each ranking signature's difexp table by
#' \code{p_value_col}. Stored tables do not all name that column the same way,
#' so a ranking signature whose table has no \code{p_value_col} is ranked by
#' its \code{pvalue} column, the same raw p-values under another name. A
#' table with neither is ranked by \code{adj_p_col}, and a warning names those
#' signatures: adjustment keeps the order of the p-values, so KS results stay
#' close to a raw p-value ranking, but GSEA scores can shift. This is done on
#' copies; nothing is written to the database and supplied objects are not
#' modified.
#'
#' Everything else -- cutoff validation, label pairing, the comparison itself
#' and its warnings and errors -- is
#' \code{OmicSignature::compare_omic_signatures()}'s, unchanged. The value is
#' that function's list, so it can be passed straight to
#' \code{OmicSignature::signature_similarity_heatmap()}.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#'   Required whenever signatures are requested by id or name.
#' @param signature_ids Database signature ids for the first list.
#' @param signature_names Database signature names for the first list.
#' @param omic_signatures An OmicSignature object, a list of OmicSignature
#'   objects, or an OmicSignatureCollection for the first list.
#' @param signature_ids2 Database signature ids for the optional second list.
#' @param signature_names2 Database signature names for the optional second list.
#' @param omic_signatures2 An OmicSignature object, a list of OmicSignature
#'   objects, or an OmicSignatureCollection for the optional second list.
#' @param method Comparison method: \code{"overlap"}, \code{"ks_rank"},
#'   \code{"ks_score"} or \code{"gsea"}; \code{"ks"} is accepted as an alias
#'   for \code{"ks_rank"}. The rank-based methods need difexp tables.
#' @param background Optional background feature vector for overlap tests.
#' @param score_cutoff Minimum absolute score to include in a signature.
#' @param adj_p_cutoff Maximum adjusted p-value to include in a signature.
#' @param min_features Minimum number of features retained per label-specific
#'   signature (at least 3).
#' @param max_feature Maximum number of features retained per label-specific
#'   signature.
#' @param label_pairing Optional named list giving the two group-label levels
#'   to compare for signatures in the first list. Names refer to the signature
#'   names used in the result: the list names of supplied objects (their
#'   metadata \code{signature_name} when unnamed), and the database
#'   \code{signature_name} of fetched ones, including the \code{"name (id N)"}
#'   form given to fetched signatures that share a name.
#' @param label_pairing2 The same for the second list.
#' @param feature_col Column containing feature identifiers.
#' @param score_col Column containing scores in signature and difexp tables.
#' @param adj_p_col Column containing adjusted p-values in difexp tables.
#' @param p_value_col Column containing raw p-values used to rank difexp
#'   tables. A ranking signature without it is ranked by a \code{pvalue}
#'   column instead, or failing that by \code{adj_p_col}, with a warning.
#' @param group_col Column containing phenotype group labels.
#' @param adjust Logical; adjust p-values within each returned comparison.
#' @param p_adjust_method Multiple-testing correction method.
#' @param alternative Alternative hypothesis for the Fisher and KS tests.
#' @param gsea_score Column of the fgsea output returned as the score matrix.
#' @param minSize Minimum pathway size passed to fgsea.
#' @param maxSize Maximum pathway size passed to fgsea.
#' @param nproc Number of fgsea workers.
#' @param verbose Logical; whether to print diagnostic messages while
#'   resolving and fetching signatures. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed on to fgsea for
#'   \code{method = "gsea"}.
#'
#' @details See \code{?OmicSignature::compare_omic_signatures} for the full
#'   description of every comparison argument, of how uni-directional
#'   signatures are handled, and of the result.
#'
#' @return The list returned by
#'   \code{OmicSignature::compare_omic_signatures()}: \code{method},
#'   \code{comparisons}, \code{label_order} and \code{background}. For a
#'   two-list comparison \code{label_order} holds both \code{sig_list1} and
#'   \code{sig_list2}.
#'
#' @seealso \code{OmicSignature::compare_omic_signatures()},
#'   \code{OmicSignature::signature_similarity_heatmap()},
#'   \code{getSignature()}, \code{searchSignature()}.
#'
#' @examples
#' # The bundled example signatures need no database
#' utils::data("omic_signature_1", "omic_signature_2", "omic_signature_3", package = "SigRepo")
#'
#' res <- SigRepo::compareSignatures(
#'   omic_signatures = list(v1 = omic_signature_1, v2 = omic_signature_2, v3 = omic_signature_3),
#'   method = "overlap",
#'   min_features = 3,
#'   max_feature = 10
#' )
#' res$comparisons$level1_vs_level1$jaccard
#'
#' # Query versus reference: rows are the first list, columns the second
#' res2 <- SigRepo::compareSignatures(
#'   omic_signatures = list(v1 = omic_signature_1),
#'   omic_signatures2 = list(v2 = omic_signature_2, v3 = omic_signature_3),
#'   method = "ks_rank",
#'   min_features = 3,
#'   max_feature = 10
#' )
#' res2$comparisons$level1_vs_level1$score
#'
#' \dontrun{
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", host = "localhost", port = 3306,
#'   user = "montilab", password = "sigrepo"
#' )
#'
#' # Signatures stored in the database, by id and by name
#' res <- SigRepo::compareSignatures(
#'   conn_handler = conn_handler,
#'   signature_ids = c(12, 34),
#'   signature_names = "my_signature",
#'   method = "overlap"
#' )
#'
#' # A query signature against a reference set, mixing stored and local ones
#' res <- SigRepo::compareSignatures(
#'   conn_handler = conn_handler,
#'   signature_ids = 12,
#'   signature_ids2 = c(34, 56),
#'   omic_signatures2 = list(local = my_omic_signature),
#'   method = "gsea"
#' )
#' }
#'
#' @export
compareSignatures <- function(
    conn_handler = NULL,
    signature_ids = NULL,
    signature_names = NULL,
    omic_signatures = NULL,
    signature_ids2 = NULL,
    signature_names2 = NULL,
    omic_signatures2 = NULL,
    method = c("overlap", "ks_rank", "ks_score", "ks", "gsea"),
    background = NULL,
    score_cutoff = 0,
    adj_p_cutoff = 0.05,
    min_features = 5,
    max_feature = 500,
    label_pairing = NULL,
    label_pairing2 = NULL,
    feature_col = "feature_name",
    score_col = "score",
    adj_p_col = "adj_p",
    p_value_col = "p_value",
    group_col = "group_label",
    adjust = FALSE,
    p_adjust_method = "BH",
    alternative = "greater",
    gsea_score = "NES",
    minSize = 1,
    maxSize = Inf,
    nproc = 0,
    verbose = FALSE,
    ...) {

  method <- base::match.arg(method)

  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)

  signature_ids <- cleanCompareRequest(signature_ids)
  signature_names <- cleanCompareRequest(signature_names)
  signature_ids2 <- cleanCompareRequest(signature_ids2)
  signature_names2 <- cleanCompareRequest(signature_names2)

  # A connection is only needed when something has to come from the database;
  # check it up front so the message is about the connection, not about an
  # empty list.
  needs_database <- base::length(c(signature_ids, signature_names, signature_ids2, signature_names2)) > 0
  if (needs_database && base::is.null(conn_handler)) {
    base::stop(
      "\n'conn_handler' is required to fetch signatures by 'signature_ids', 'signature_names', ",
      "'signature_ids2' or 'signature_names2'.\n"
    )
  }

  sig_list1 <- resolveCompareSignatureList(
    conn_handler = conn_handler,
    signature_ids = signature_ids,
    signature_names = signature_names,
    omic_signatures = omic_signatures,
    arg_suffix = "",
    verbose = verbose
  )
  if (base::length(sig_list1) == 0) {
    base::stop(
      "\nProvide signatures for the first list through 'signature_ids', 'signature_names' ",
      "and/or 'omic_signatures'.\n"
    )
  }

  # The second list is only in play when the caller asked for one.
  sig_list2 <- NULL
  two_lists <- base::length(c(signature_ids2, signature_names2)) > 0 || !base::is.null(omic_signatures2)
  if (two_lists) {
    sig_list2 <- resolveCompareSignatureList(
      conn_handler = conn_handler,
      signature_ids = signature_ids2,
      signature_names = signature_names2,
      omic_signatures = omic_signatures2,
      arg_suffix = "2",
      verbose = verbose
    )
    if (base::length(sig_list2) == 0) {
      base::stop("\nThe second list ('omic_signatures2') is empty.\n")
    }
  } else if (base::length(sig_list1) < 2) {
    base::stop(
      "\nAt least two signatures are required for a self-comparison; got ", base::length(sig_list1), ". ",
      "Add more signatures to the first list, or supply a second list through ",
      "'signature_ids2', 'signature_names2' or 'omic_signatures2'.\n"
    )
  }

  # The rank-based methods rank the ranking side's difexp tables by p-value:
  # the second list, or the first list itself in a self-comparison.
  if (method %in% c("ks_rank", "ks_score", "ks", "gsea")) {
    if (two_lists) {
      sig_list2 <- fillRankingPValues(sig_list2, p_value_col = p_value_col, adj_p_col = adj_p_col)
    } else {
      sig_list1 <- fillRankingPValues(sig_list1, p_value_col = p_value_col, adj_p_col = adj_p_col)
    }
  }

  SigRepo::verbose(base::sprintf(
    "Comparing %d signature(s)%s with method '%s'.\n",
    base::length(sig_list1),
    if (two_lists) base::sprintf(" against %d reference signature(s)", base::length(sig_list2)) else "",
    method
  ))

  OmicSignature::compare_omic_signatures(
    sig_list1 = sig_list1,
    sig_list2 = sig_list2,
    method = method,
    background = background,
    score_cutoff = score_cutoff,
    adj_p_cutoff = adj_p_cutoff,
    min_features = min_features,
    max_feature = max_feature,
    label_pairing = label_pairing,
    label_pairing2 = label_pairing2,
    feature_col = feature_col,
    score_col = score_col,
    adj_p_col = adj_p_col,
    p_value_col = p_value_col,
    group_col = group_col,
    adjust = adjust,
    p_adjust_method = p_adjust_method,
    alternative = alternative,
    gsea_score = gsea_score,
    minSize = minSize,
    maxSize = maxSize,
    nproc = nproc,
    ...
  )
}


#' Turn a vector of requested ids or names into clean character strings
#'
#' @param x A vector of ids or names, or NULL.
#' @return A unique character vector, trimmed, with NA and empty entries
#'   removed; zero-length when nothing usable was given.
#' @noRd
cleanCompareRequest <- function(x) {
  if (base::length(x) == 0) {
    return(base::character())
  }
  if (base::is.numeric(x)) {
    # as.character() prints large numbers in scientific notation
    # ("1e+05"), which the database's string comparison would never match;
    # write every number out in full instead.
    x <- base::vapply(x, function(v) {
      if (base::is.na(v)) NA_character_ else base::format(v, scientific = FALSE, trim = TRUE)
    }, base::character(1))
  } else {
    x <- base::as.character(x)
  }
  x <- base::trimws(x)
  x <- x[!base::is.na(x) & x != ""]
  base::unique(x)
}


#' Give ranking signatures the p-value column compare_omic_signatures() ranks by
#'
#' compare_omic_signatures() stops when a ranking signature's difexp table has
#' no \code{p_value_col}. Many stored tables name the raw p-value
#' \code{pvalue}, and some keep only adjusted p-values. A table without
#' \code{p_value_col} gets it filled from \code{pvalue} when present, which is
#' the same quantity, and otherwise from \code{adj_p_col}, with a warning
#' naming those signatures: adjustment keeps the order of the p-values, which
#' is what the ranking uses, but GSEA also uses their size. Tables with none
#' of these are left for compare_omic_signatures() to report.
#'
#' Changed signatures are copies, so the caller's objects are untouched.
#'
#' @param sig_list Named list of OmicSignature objects on the ranking side.
#' @param p_value_col The raw p-value column compare_omic_signatures() ranks by.
#' @param adj_p_col The adjusted p-value column to fall back to.
#' @return \code{sig_list}, with copies in place of the signatures filled in.
#' @noRd
fillRankingPValues <- function(sig_list, p_value_col, adj_p_col) {
  fell_back <- base::character()
  for (i in base::seq_along(sig_list)) {
    difexp <- sig_list[[i]]$difexp
    if (base::is.null(difexp) || p_value_col %in% base::colnames(difexp)) {
      next
    }
    source_col <- base::intersect(c("pvalue", adj_p_col), base::colnames(difexp))[1]
    if (base::is.na(source_col)) {
      next
    }
    difexp[[p_value_col]] <- difexp[[source_col]]
    filled <- sig_list[[i]]$clone(deep = TRUE)
    filled$difexp <- difexp
    sig_list[[i]] <- filled
    if (source_col == adj_p_col) {
      fell_back <- c(fell_back, base::names(sig_list)[i])
    }
  }

  if (base::length(fell_back) > 0) {
    base::warning(
      "\nRanking by '", adj_p_col, "' for signature(s) whose difexp table has no '", p_value_col,
      "' or 'pvalue' column: ", base::paste(fell_back, collapse = ", "), ".\n",
      "Adjusted p-values keep the order of the raw p-values, so KS results stay close to a ",
      "raw p-value ranking; GSEA scores can shift.\n",
      call. = FALSE
    )
  }
  sig_list
}


#' Assemble one signature list for compareSignatures()
#'
#' Fetched signatures come first, in the order they were requested (ids, then
#' names), followed by the supplied objects. Requests that could not be
#' honoured are reported with a warning, or with an error when they leave
#' the whole list empty.
#'
#' @param conn_handler Connection handler; only used when ids or names are given.
#' @param signature_ids Cleaned ids to fetch.
#' @param signature_names Cleaned names to fetch.
#' @param omic_signatures Supplied objects, list, collection, or NULL.
#' @param arg_suffix "" for the first list, "2" for the second; used in messages.
#' @param verbose Passed on to the database helpers.
#' @return A named list of OmicSignature objects, possibly empty.
#' @noRd
resolveCompareSignatureList <- function(conn_handler, signature_ids, signature_names, omic_signatures,
                                        arg_suffix = "", verbose = FALSE) {

  fetched <- base::list()
  problems <- base::character()
  if (base::length(c(signature_ids, signature_names)) > 0) {
    got <- fetchCompareSignatures(
      conn_handler = conn_handler,
      signature_ids = signature_ids,
      signature_names = signature_names,
      arg_suffix = arg_suffix,
      verbose = verbose
    )
    fetched <- got$signatures
    problems <- got$problems
  }

  supplied <- asCompareSignatureList(omic_signatures, arg_name = base::paste0("omic_signatures", arg_suffix))
  signatures <- c(fetched, supplied)

  # Failed requests are only fatal when nothing at all is left to compare;
  # otherwise the comparison goes ahead on what could be assembled.
  if (base::length(problems) > 0) {
    if (base::length(signatures) == 0) {
      base::stop(
        "\nNone of the signatures requested through 'signature_ids", arg_suffix, "' or 'signature_names",
        arg_suffix, "' could be retrieved.\n",
        base::paste(problems, collapse = "\n"), "\n"
      )
    }
    base::warning(
      "Some requested signatures were left out of the comparison:\n",
      base::paste(problems, collapse = "\n"),
      call. = FALSE
    )
  }

  signatures
}


#' Fetch requested signatures from the database and report what could not be
#'
#' @return A list with `signatures` (a named list of OmicSignature objects,
#'   possibly empty) and `problems` (a character vector, one entry per id or
#'   name that could not be honoured).
#' @noRd
fetchCompareSignatures <- function(conn_handler, signature_ids, signature_names, arg_suffix = "", verbose = FALSE) {

  ids_arg <- base::paste0("signature_ids", arg_suffix)
  names_arg <- base::paste0("signature_names", arg_suffix)
  problems <- base::character()

  # Resolve ids and names to signature rows separately: searchSignature()
  # combines its filters with AND, so one call with both would only return
  # signatures matching an id *and* a name.
  rows <- NULL
  if (base::length(signature_ids) > 0) {
    by_id <- searchSignature(conn_handler = conn_handler, signature_id = signature_ids, verbose = verbose)
    found <- base::as.character(by_id$signature_id)
    unknown <- signature_ids[!signature_ids %in% found]
    if (base::length(unknown) > 0) {
      problems <- c(problems, base::sprintf(
        "%s: no signature with id %s exists.", ids_arg, base::paste(unknown, collapse = ", ")
      ))
    }
    # Keep the caller's order.
    rows <- by_id[base::match(signature_ids[signature_ids %in% found], found), , drop = FALSE]
  }
  if (base::length(signature_names) > 0) {
    by_name <- searchSignature(conn_handler = conn_handler, signature_name = signature_names, verbose = verbose)
    found <- base::tolower(base::trimws(base::as.character(by_name$signature_name)))
    wanted <- base::tolower(signature_names)
    unknown <- signature_names[!wanted %in% found]
    if (base::length(unknown) > 0) {
      problems <- c(problems, base::sprintf(
        "%s: no signature named %s exists.", names_arg,
        base::paste(base::sprintf("'%s'", unknown), collapse = ", ")
      ))
    }
    # Every match for each requested name, in the order the names were given.
    ordered <- base::unlist(base::lapply(wanted, function(w) base::which(found == w)), use.names = FALSE)
    rows <- base::rbind(rows, by_name[ordered, , drop = FALSE])
  }

  # A signature requested by both id and name is fetched once.
  if (!base::is.null(rows) && base::nrow(rows) > 0) {
    rows <- rows[!base::duplicated(base::as.character(rows$signature_id)), , drop = FALSE]
  }

  fetched <- base::list()
  fetched_ids <- base::character()
  fetched_names <- base::character()
  for (i in base::seq_len(base::NROW(rows))) {
    row_id <- base::as.character(rows$signature_id[i])
    row_name <- base::as.character(rows$signature_name[i])
    got <- getSignature(conn_handler = conn_handler, signature_id = row_id, verbose = verbose)
    if (base::is.null(got) || base::length(got) == 0) {
      problems <- c(problems, base::sprintf(
        "signature id %s ('%s') could not be retrieved: it is not visible to this account.", row_id, row_name
      ))
      next
    }
    fetched[[base::length(fetched) + 1L]] <- got[[1]]
    fetched_ids <- c(fetched_ids, row_id)
    fetched_names <- c(fetched_names, row_name)
  }

  # Names are unique per user, not globally: tell duplicates apart by id so
  # the result matrices stay addressable.
  duplicated_names <- fetched_names %in% fetched_names[base::duplicated(fetched_names)]
  fetched_names[duplicated_names] <- base::sprintf("%s (id %s)", fetched_names[duplicated_names], fetched_ids[duplicated_names])
  base::names(fetched) <- fetched_names

  SigRepo::verbose(base::sprintf("Retrieved %d signature(s) from the database.\n", base::length(fetched)))

  base::list(signatures = fetched, problems = problems)
}


#' Normalise supplied signature objects into a named list
#'
#' @param x An OmicSignature, a list of them, an OmicSignatureCollection, or NULL.
#' @param arg_name Argument name for error messages.
#' @return A named list of OmicSignature objects (possibly empty). Names the
#'   caller gave are kept and only blank ones are filled from the metadata
#'   \code{signature_name}. (compare_omic_signatures() itself replaces every
#'   name from metadata as soon as one is blank; keeping the caller's names
#'   is friendlier and is what the user-facing documentation promises.)
#' @noRd
asCompareSignatureList <- function(x, arg_name) {
  if (base::is.null(x)) {
    return(base::list())
  }
  if (methods::is(x, "OmicSignatureCollection")) {
    x <- x$OmicSigList
  }
  if (methods::is(x, "OmicSignature")) {
    x <- base::list(x)
  }
  if (!base::is.list(x)) {
    base::stop(
      "\n'", arg_name, "' must be an OmicSignature object, a list of OmicSignature objects, ",
      "or an OmicSignatureCollection.\n"
    )
  }
  if (base::length(x) == 0) {
    return(base::list())
  }

  is_signature <- base::vapply(x, function(s) methods::is(s, "OmicSignature"), base::logical(1))
  if (!base::all(is_signature)) {
    bad <- base::names(x)[!is_signature]
    if (base::is.null(bad) || base::any(bad %in% c("", NA))) {
      bad <- base::which(!is_signature)
    }
    base::stop(
      "\n'", arg_name, "' contains elements that are not OmicSignature objects: ",
      base::paste(bad, collapse = ", "), "\n"
    )
  }

  list_names <- base::names(x)
  if (base::is.null(list_names)) {
    list_names <- base::rep("", base::length(x))
  }
  blank <- base::is.na(list_names) | list_names == ""
  list_names[blank] <- base::vapply(x[blank], function(s) {
    resolveSignatureLabel(omic_signature = s, label = NULL, fallback = "signature")
  }, base::character(1))
  base::names(x) <- list_names

  x
}
