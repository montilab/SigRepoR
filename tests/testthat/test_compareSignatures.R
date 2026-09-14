# Offline tests for compareSignatures(). The two database helpers it relies
# on -- searchSignature() to resolve ids/names to signatures, getSignature()
# to fetch them -- are replaced with local mocks, so these tests exercise the
# wrapper's own input resolution and its pass-through to
# OmicSignature::compare_omic_signatures(). test_compareSignatures_db.R
# covers the live round-trip.

# The four bundled versions of the Myc signature: bi-directional, with difexp,
# so every method (including the rank-based ones) can run on them. ####
example_signatures <- function(){
  env <- base::environment()
  utils::data(
    list = c("omic_signature_1", "omic_signature_2", "omic_signature_3", "omic_signature_4"),
    package = "SigRepo",
    envir = env
  )
  base::list(
    v1 = env$omic_signature_1,
    v2 = env$omic_signature_2,
    v3 = env$omic_signature_3,
    v4 = env$omic_signature_4
  )
}

# What the mocked database "contains": ids, names, whether the caller may see
# each one, and which example object stands in for it. Ids are integers, as
# the MySQL driver returns them, and deliberately do not start at 1, so an
# index/id mix-up in the wrapper would show. ####
default_rows <- function(){
  base::data.frame(
    signature_id = c(11L, 12L, 13L, 14L),
    signature_name = c(
      "Myc_reduce_mice_liver_24m_v1", "Myc_reduce_mice_liver_24m_v2",
      "Myc_reduce_mice_liver_24m_v3", "Myc_reduce_mice_liver_24m_v4"
    ),
    user_name = "tester",
    visible = TRUE,
    key = c("v1", "v2", "v3", "v4"),
    stringsAsFactors = FALSE
  )
}

# Mocks with the same calling conventions as the real helpers. searchSignature
# ANDs its filters and returns a (possibly empty) metadata table; getSignature
# returns NULL for anything the caller can't see, otherwise a list named by
# signature_name. Ids are matched as strings, like the SQL the real helpers
# build (trim(lower(signature_id)) IN (...)). Every call is recorded so tests
# can check what the wrapper passed down. ####
mock_database <- function(sigs, rows = default_rows()){
  calls <- base::new.env()
  calls$search <- base::list()
  calls$get <- base::list()

  filter_rows <- function(signature_id, signature_name){
    hits <- rows
    if(base::length(signature_id) > 0){
      hits <- hits[base::as.character(hits$signature_id) %in% base::trimws(base::as.character(signature_id)), , drop = FALSE]
    }
    if(base::length(signature_name) > 0){
      hits <- hits[base::tolower(hits$signature_name) %in% base::tolower(base::trimws(signature_name)), , drop = FALSE]
    }
    hits
  }

  search_fn <- function(conn_handler = NULL, signature_id = NULL, signature_name = NULL, ..., verbose = TRUE){
    calls$search[[base::length(calls$search) + 1L]] <- base::list(
      conn_handler = conn_handler, signature_id = signature_id, signature_name = signature_name, verbose = verbose
    )
    hits <- filter_rows(signature_id, signature_name)
    hits[, c("signature_id", "signature_name", "user_name"), drop = FALSE]
  }

  get_fn <- function(conn_handler = NULL, signature_name = NULL, signature_id = NULL, verbose = TRUE){
    calls$get[[base::length(calls$get) + 1L]] <- base::list(
      conn_handler = conn_handler, signature_id = signature_id, signature_name = signature_name, verbose = verbose
    )
    hits <- filter_rows(signature_id, signature_name)
    hits <- hits[hits$visible, , drop = FALSE]
    if(base::nrow(hits) == 0) return(NULL)
    stats::setNames(sigs[hits$key], hits$signature_name)
  }

  base::list(calls = calls, search = search_fn, get = get_fn)
}

mock_conn_handler <- base::list(dbname = "sigrepo", host = "mock", port = 3306, user = "tester", password = "x", api_host = "http://mock", api_port = 8020)

first_matrix <- function(res){
  res$comparisons$level1_vs_level1$jaccard
}

## Pass-through to OmicSignature ####

test_that("a single list of objects gives the same result as compare_omic_signatures", {
  sigs <- example_signatures()

  direct <- OmicSignature::compare_omic_signatures(sigs[1:3], method = "overlap", min_features = 3, max_feature = 10)
  wrapped <- SigRepo::compareSignatures(omic_signatures = sigs[1:3], method = "overlap", min_features = 3, max_feature = 10)

  expect_identical(wrapped, direct)
  expect_equal(base::rownames(first_matrix(wrapped)), c("v1", "v2", "v3"))
})

test_that("a second list of objects gives a rectangular query-vs-reference result", {
  sigs <- example_signatures()

  direct <- OmicSignature::compare_omic_signatures(
    sig_list1 = sigs[1:2], sig_list2 = sigs[3:4], method = "overlap", min_features = 3, max_feature = 10
  )
  wrapped <- SigRepo::compareSignatures(
    omic_signatures = sigs[1:2], omic_signatures2 = sigs[3:4], method = "overlap", min_features = 3, max_feature = 10
  )

  expect_identical(wrapped, direct)
  expect_equal(base::rownames(first_matrix(wrapped)), c("v1", "v2"))
  expect_equal(base::colnames(first_matrix(wrapped)), c("v3", "v4"))
  expect_named(wrapped$label_order, c("sig_list1", "sig_list2"))
})

test_that("one signature per side is enough for a two-list comparison", {
  sigs <- example_signatures()

  res <- SigRepo::compareSignatures(
    omic_signatures = sigs[1], omic_signatures2 = sigs[2], method = "overlap", min_features = 3, max_feature = 10
  )

  expect_equal(base::dim(first_matrix(res)), c(1L, 1L))
  expect_equal(base::rownames(first_matrix(res)), "v1")
  expect_equal(base::colnames(first_matrix(res)), "v2")
})

test_that("a self-comparison needs at least two signatures", {
  sigs <- example_signatures()

  expect_error(SigRepo::compareSignatures(omic_signatures = sigs[1], method = "overlap"), "two")
})

test_that("the two lists must not share a signature name", {
  sigs <- example_signatures()

  expect_error(
    SigRepo::compareSignatures(omic_signatures = sigs[1:2], omic_signatures2 = sigs[2:3], method = "overlap", min_features = 3, max_feature = 10),
    "share"
  )
})

test_that("the legacy ks method alias is accepted", {
  sigs <- example_signatures()

  res <- SigRepo::compareSignatures(omic_signatures = sigs[1:2], method = "ks", min_features = 3, max_feature = 10)

  expect_equal(res$method, "ks_rank")
  expect_named(res$comparisons$level1_vs_level1, c("score", "pvalue"))
})

test_that("explicit tuning parameters are forwarded unchanged", {
  sigs <- example_signatures()

  # A background wider than the observed features changes the Fisher tests,
  # and pinning v1's level order flips which of its levels is compared first.
  background <- base::unique(c(
    base::unlist(base::lapply(sigs[1:3], function(s) s$difexp$feature_name), use.names = FALSE),
    base::paste0("background_only_", base::seq_len(200))
  ))
  tuned <- base::list(
    method = "overlap",
    background = background,
    score_cutoff = 0.3,
    adj_p_cutoff = 0.5,
    min_features = 4,
    max_feature = 8,
    label_pairing = base::list(v1 = c("WT", "MYC Reduce")),
    adjust = TRUE,
    p_adjust_method = "bonferroni",
    alternative = "two.sided"
  )

  direct <- base::do.call(OmicSignature::compare_omic_signatures, c(base::list(sig_list1 = sigs[1:3]), tuned))
  wrapped <- base::do.call(SigRepo::compareSignatures, c(base::list(omic_signatures = sigs[1:3]), tuned))
  defaults <- SigRepo::compareSignatures(omic_signatures = sigs[1:3], method = "overlap", min_features = 3, max_feature = 10)

  expect_identical(wrapped, direct)
  expect_false(base::identical(wrapped$comparisons, defaults$comparisons))
  expect_equal(wrapped$label_order$sig_list1["v1", ], c(level1 = "WT", level2 = "MYC Reduce"))
})

test_that("column-name parameters are forwarded", {
  sigs <- example_signatures()
  run <- function(...){
    SigRepo::compareSignatures(omic_signatures = sigs[1:2], min_features = 3, max_feature = 10, ...)
  }

  expect_error(run(method = "overlap", feature_col = "nope"), "nope")
  expect_error(run(method = "overlap", score_col = "nope"), "nope")
  expect_error(run(method = "overlap", adj_p_col = "nope"), "nope")
  expect_error(run(method = "overlap", group_col = "nope"), "nope")
  # A missing p-value column falls back to adj_p, and the warning names it.
  expect_warning(run(method = "ks_rank", p_value_col = "nope"), "nope")
})

## The ranking p-value column ####

# A copy of `sig` whose difexp table has been changed by `edit`, leaving the
# original object untouched.
edit_difexp <- function(sig, edit){
  copy <- sig$clone(deep = TRUE)
  copy$difexp <- edit(copy$difexp)
  copy
}

drop_cols <- function(cols){
  function(difexp) difexp[, !base::colnames(difexp) %in% cols, drop = FALSE]
}

# adj_p in reverse p-value order, so ranking by adj_p and by p_value give
# visibly different results.
reverse_adj_p <- function(difexp){
  difexp$adj_p <- 1 - difexp$p_value
  difexp
}

test_that("a ranking signature that names its raw p-values pvalue is ranked by them, without a warning", {
  sigs <- example_signatures()
  renamed <- base::lapply(sigs[3:4], edit_difexp, edit = function(difexp){
    base::names(difexp)[base::names(difexp) == "p_value"] <- "pvalue"
    difexp
  })

  expect_warning(
    wrapped <- SigRepo::compareSignatures(
      omic_signatures = sigs[1:2], omic_signatures2 = renamed, method = "ks_rank", min_features = 3, max_feature = 10
    ),
    regexp = NA
  )
  direct <- OmicSignature::compare_omic_signatures(
    sig_list1 = sigs[1:2], sig_list2 = sigs[3:4], method = "ks_rank", min_features = 3, max_feature = 10
  )
  expect_identical(wrapped, direct)
})

test_that("a ranking signature with only adj_p is ranked by adj_p, with a warning naming it", {
  sigs <- example_signatures()
  adj_only <- edit_difexp(edit_difexp(sigs$v3, reverse_adj_p), drop_cols("p_value"))

  expect_warning(
    wrapped <- SigRepo::compareSignatures(
      omic_signatures = sigs[1:2], omic_signatures2 = base::list(v3 = adj_only), method = "ks_rank", min_features = 3, max_feature = 10
    ),
    "v3"
  )
  ranked_by_adj_p <- edit_difexp(adj_only, function(difexp){
    difexp$p_value <- difexp$adj_p
    difexp
  })
  direct <- OmicSignature::compare_omic_signatures(
    sig_list1 = sigs[1:2], sig_list2 = base::list(v3 = ranked_by_adj_p), method = "ks_rank", min_features = 3, max_feature = 10
  )
  ranked_by_p <- OmicSignature::compare_omic_signatures(
    sig_list1 = sigs[1:2], sig_list2 = base::list(v3 = edit_difexp(sigs$v3, reverse_adj_p)), method = "ks_rank", min_features = 3, max_feature = 10
  )
  expect_identical(wrapped, direct)
  expect_false(base::identical(wrapped$comparisons, ranked_by_p$comparisons))
})

test_that("only the ranking signatures without raw p-values fall back to adj_p", {
  sigs <- example_signatures()
  adj_only <- edit_difexp(sigs$v3, drop_cols("p_value"))

  warnings <- base::character()
  wrapped <- base::withCallingHandlers(
    SigRepo::compareSignatures(
      omic_signatures = sigs[1:2], omic_signatures2 = base::list(v3 = adj_only, v4 = sigs$v4),
      method = "ks_rank", min_features = 3, max_feature = 10
    ),
    warning = function(w){
      warnings <<- c(warnings, base::conditionMessage(w))
      base::invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1)
  expect_match(warnings, "v3")
  expect_no_match(warnings, "v4")
  expect_true(base::all(base::is.finite(wrapped$comparisons$level1_vs_level1$score)))
})

test_that("in a self-comparison the first list is the ranking side that falls back", {
  sigs <- example_signatures()
  adj_only <- edit_difexp(sigs$v1, drop_cols("p_value"))

  expect_warning(
    res <- SigRepo::compareSignatures(
      omic_signatures = base::list(v1 = adj_only, v2 = sigs$v2), method = "ks_score", min_features = 3, max_feature = 10
    ),
    "v1"
  )
  expect_true(base::all(base::is.finite(res$comparisons$level1_vs_level1$score)))
})

test_that("gsea falls back to adj_p the same way", {
  testthat::skip_if_not_installed("fgsea")
  sigs <- example_signatures()
  adj_only <- edit_difexp(sigs$v3, drop_cols("p_value"))

  warnings <- base::character()
  base::withCallingHandlers(
    SigRepo::compareSignatures(
      omic_signatures = sigs[1:2], omic_signatures2 = base::list(v3 = adj_only),
      method = "gsea", min_features = 3, max_feature = 10, gsea_score = "ES", nproc = 1
    ),
    warning = function(w){
      warnings <<- c(warnings, base::conditionMessage(w))
      base::invokeRestart("muffleWarning")
    }
  )
  expect_true(base::any(base::grepl("adj_p", warnings) & base::grepl("v3", warnings)))
})

test_that("the caller's OmicSignature objects are not modified", {
  sigs <- example_signatures()
  renamed <- edit_difexp(sigs$v3, function(difexp){
    base::names(difexp)[base::names(difexp) == "p_value"] <- "pvalue"
    difexp
  })
  adj_only <- edit_difexp(sigs$v4, drop_cols("p_value"))
  before <- base::list(base::colnames(renamed$difexp), base::colnames(adj_only$difexp))

  base::suppressWarnings(SigRepo::compareSignatures(
    omic_signatures = sigs[1:2], omic_signatures2 = base::list(v3 = renamed, v4 = adj_only),
    method = "ks_rank", min_features = 3, max_feature = 10
  ))

  expect_identical(base::list(base::colnames(renamed$difexp), base::colnames(adj_only$difexp)), before)
})

test_that("an overlap comparison never looks for a p-value column", {
  sigs <- example_signatures()
  adj_only <- base::lapply(sigs[1:2], edit_difexp, edit = drop_cols("p_value"))

  expect_warning(
    wrapped <- SigRepo::compareSignatures(omic_signatures = adj_only, method = "overlap", min_features = 3, max_feature = 10),
    regexp = NA
  )
  expect_identical(wrapped, OmicSignature::compare_omic_signatures(adj_only, method = "overlap", min_features = 3, max_feature = 10))
})

test_that("a ranking signature with neither raw nor adjusted p-values still errors", {
  sigs <- example_signatures()
  # OmicSignature requires a difexp table to keep one of p_value, q_value or
  # adj_p, so the only table with neither p_value nor adj_p has q_value.
  no_p <- edit_difexp(sigs$v3, function(difexp){
    difexp$q_value <- difexp$adj_p
    drop_cols(c("p_value", "adj_p"))(difexp)
  })

  expect_error(
    SigRepo::compareSignatures(
      omic_signatures = sigs[1:2], omic_signatures2 = base::list(v3 = no_p), method = "ks_rank", min_features = 3, max_feature = 10
    ),
    "p_value"
  )
})

test_that("gsea parameters and extra fgsea arguments are forwarded", {
  testthat::skip_if_not_installed("fgsea")
  sigs <- example_signatures()
  run <- function(...){
    base::suppressWarnings(SigRepo::compareSignatures(omic_signatures = sigs[1:2], method = "gsea", min_features = 3, max_feature = 10, ...))
  }

  # ES is deterministic (unlike the permutation p-values), so the score
  # matrix can be compared exactly against the direct call.
  direct <- base::suppressWarnings(OmicSignature::compare_omic_signatures(
    sigs[1:2], method = "gsea", min_features = 3, max_feature = 10, gsea_score = "ES", minSize = 3, maxSize = 200, nproc = 1
  ))
  wrapped <- run(gsea_score = "ES", minSize = 3, maxSize = 200, nproc = 1)
  expect_equal(wrapped$method, "gsea")
  expect_identical(wrapped$comparisons$level1_vs_level1$score, direct$comparisons$level1_vs_level1$score)
  expect_true(base::all(base::is.finite(wrapped$comparisons$level1_vs_level1$score)))

  # A different score column gives different numbers, and a minSize above the
  # retained set size leaves every cell NA: both prove the arguments arrived.
  nes <- run(gsea_score = "NES", minSize = 3, maxSize = 200, nproc = 1)
  expect_false(base::identical(nes$comparisons$level1_vs_level1$score, wrapped$comparisons$level1_vs_level1$score))
  too_big <- run(minSize = 100, nproc = 1)
  expect_true(base::all(base::is.na(too_big$comparisons$level1_vs_level1$score)))

  # Anything unknown in ... reaches fgsea itself.
  expect_error(run(nproc = 1, not_an_fgsea_argument = 1), "unused argument")
})

## Fetching from the database ####

test_that("signature_ids are fetched with the caller's connection and compared by their database names", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  res <- SigRepo::compareSignatures(
    conn_handler = mock_conn_handler, signature_ids = c(11, 13),
    method = "overlap", min_features = 3, max_feature = 10, verbose = FALSE
  )

  expect_equal(base::rownames(first_matrix(res)), c("Myc_reduce_mice_liver_24m_v1", "Myc_reduce_mice_liver_24m_v3"))
  expect_equal(base::colnames(first_matrix(res)), base::rownames(first_matrix(res)))

  expect_gt(base::length(db$calls$get), 0)
  for(call in c(db$calls$search, db$calls$get)){
    expect_identical(call$conn_handler, mock_conn_handler)
    expect_false(call$verbose)
  }
})

test_that("ids may be given as character strings, with surrounding whitespace", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  res <- SigRepo::compareSignatures(
    conn_handler = mock_conn_handler, signature_ids = c("11", " 13 "),
    method = "overlap", min_features = 3, max_feature = 10
  )

  expect_equal(base::rownames(first_matrix(res)), c("Myc_reduce_mice_liver_24m_v1", "Myc_reduce_mice_liver_24m_v3"))
})

test_that("large numeric ids are not turned into scientific notation", {
  sigs <- example_signatures()
  rows <- default_rows()
  rows$signature_id[rows$key == "v4"] <- 100000L
  db <- mock_database(sigs, rows)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  # as.character(100000) is "1e+05", which the database's string comparison
  # would never match.
  expect_warning(
    res <- SigRepo::compareSignatures(
      conn_handler = mock_conn_handler, signature_ids = c(11, 100000),
      method = "overlap", min_features = 3, max_feature = 10
    ),
    regexp = NA
  )
  expect_equal(base::rownames(first_matrix(res)), c("Myc_reduce_mice_liver_24m_v1", "Myc_reduce_mice_liver_24m_v4"))
})

test_that("signature_names resolve case-insensitively and combine with ids in one list", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  res <- SigRepo::compareSignatures(
    conn_handler = mock_conn_handler, signature_ids = 11, signature_names = " MYC_reduce_mice_liver_24m_v2 ",
    method = "overlap", min_features = 3, max_feature = 10
  )

  expect_equal(base::rownames(first_matrix(res)), c("Myc_reduce_mice_liver_24m_v1", "Myc_reduce_mice_liver_24m_v2"))
})

test_that("a name shared by several signatures matches all of them", {
  sigs <- example_signatures()
  rows <- default_rows()
  rows$signature_name[rows$signature_id %in% c(12L, 13L)] <- "shared_name"
  db <- mock_database(sigs, rows)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  res <- SigRepo::compareSignatures(
    conn_handler = mock_conn_handler, signature_names = "shared_name",
    method = "overlap", min_features = 3, max_feature = 10
  )

  expect_equal(base::rownames(first_matrix(res)), c("shared_name (id 12)", "shared_name (id 13)"))
})

test_that("a signature requested by both id and name is only included once", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  res <- SigRepo::compareSignatures(
    conn_handler = mock_conn_handler, signature_ids = c(11, 12), signature_names = "Myc_reduce_mice_liver_24m_v1",
    method = "overlap", min_features = 3, max_feature = 10
  )

  expect_equal(base::rownames(first_matrix(res)), c("Myc_reduce_mice_liver_24m_v1", "Myc_reduce_mice_liver_24m_v2"))
})

test_that("ids and names that do not exist are reported and the rest are compared", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  expect_warning(
    res <- SigRepo::compareSignatures(
      conn_handler = mock_conn_handler, signature_ids = c(11, 12, 999),
      method = "overlap", min_features = 3, max_feature = 10
    ),
    "999"
  )
  expect_equal(base::dim(first_matrix(res)), c(2L, 2L))

  expect_warning(
    res2 <- SigRepo::compareSignatures(
      conn_handler = mock_conn_handler, signature_ids = c(11, 12), signature_names = "no_such_signature",
      method = "overlap", min_features = 3, max_feature = 10
    ),
    "no_such_signature"
  )
  expect_equal(base::dim(first_matrix(res2)), c(2L, 2L))
})

test_that("signatures the caller cannot see are reported and the rest are compared", {
  sigs <- example_signatures()
  rows <- default_rows()
  rows$visible[rows$signature_id == 14L] <- FALSE
  db <- mock_database(sigs, rows)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  expect_warning(
    res <- SigRepo::compareSignatures(
      conn_handler = mock_conn_handler, signature_ids = c(11, 12, 14),
      method = "overlap", min_features = 3, max_feature = 10
    ),
    "14"
  )
  expect_equal(base::rownames(first_matrix(res)), c("Myc_reduce_mice_liver_24m_v1", "Myc_reduce_mice_liver_24m_v2"))
})

test_that("when every database request fails but objects were supplied, the objects are compared with a warning", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  expect_warning(
    res <- SigRepo::compareSignatures(
      conn_handler = mock_conn_handler, signature_ids = 999, omic_signatures = sigs[2:3],
      method = "overlap", min_features = 3, max_feature = 10
    ),
    "999"
  )
  expect_equal(base::rownames(first_matrix(res)), c("v2", "v3"))
})

test_that("an error names the request when nothing can be fetched", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  expect_error(
    SigRepo::compareSignatures(conn_handler = mock_conn_handler, signature_ids = c(998, 999), method = "overlap"),
    "999"
  )
  expect_error(
    SigRepo::compareSignatures(conn_handler = mock_conn_handler, signature_names = "no_such_signature", method = "overlap"),
    "no_such_signature"
  )
})

test_that("the second list can be fetched from the database", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")
  db_names <- default_rows()$signature_name

  direct <- OmicSignature::compare_omic_signatures(
    sig_list1 = stats::setNames(sigs[1:2], db_names[1:2]), sig_list2 = stats::setNames(sigs[3:4], db_names[3:4]),
    method = "overlap", min_features = 3, max_feature = 10
  )
  wrapped <- SigRepo::compareSignatures(
    conn_handler = mock_conn_handler, signature_ids = c(11, 12), signature_ids2 = c(13, 14),
    method = "overlap", min_features = 3, max_feature = 10
  )

  expect_identical(wrapped, direct)
  expect_equal(base::rownames(first_matrix(wrapped)), db_names[1:2])
  expect_equal(base::colnames(first_matrix(wrapped)), db_names[3:4])
})

test_that("a reference list that cannot be fetched at all is an error", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  expect_error(
    SigRepo::compareSignatures(
      conn_handler = mock_conn_handler, signature_ids = c(11, 12), signature_ids2 = 999,
      method = "overlap", min_features = 3, max_feature = 10
    ),
    "999"
  )
})

test_that("fetched signatures that share a name are told apart by id", {
  sigs <- example_signatures()
  rows <- default_rows()
  rows$signature_name[rows$signature_id %in% c(12L, 13L)] <- "shared_name"
  db <- mock_database(sigs, rows)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  res <- SigRepo::compareSignatures(
    conn_handler = mock_conn_handler, signature_ids = c(11, 12, 13),
    method = "overlap", min_features = 3, max_feature = 10
  )

  expect_equal(
    base::rownames(first_matrix(res)),
    c("Myc_reduce_mice_liver_24m_v1", "shared_name (id 12)", "shared_name (id 13)")
  )
})

test_that("fetched and supplied signatures combine into one list", {
  sigs <- example_signatures()
  db <- mock_database(sigs)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  res <- SigRepo::compareSignatures(
    conn_handler = mock_conn_handler, signature_ids = 11, omic_signatures = sigs["v2"],
    method = "overlap", min_features = 3, max_feature = 10
  )

  expect_equal(base::rownames(first_matrix(res)), c("Myc_reduce_mice_liver_24m_v1", "v2"))
})

## Supplied objects ####

test_that("unnamed supplied objects are named from their metadata", {
  sigs <- example_signatures()

  res <- SigRepo::compareSignatures(omic_signatures = base::unname(sigs[1:2]), method = "overlap", min_features = 3, max_feature = 10)

  expect_equal(base::rownames(first_matrix(res)), c("Myc_reduce_mice_liver_24m_v1", "Myc_reduce_mice_liver_24m_v2"))
})

test_that("a single OmicSignature object can be supplied on either side", {
  sigs <- example_signatures()

  res <- SigRepo::compareSignatures(
    omic_signatures = sigs$v1, omic_signatures2 = sigs$v2, method = "overlap", min_features = 3, max_feature = 10
  )

  expect_equal(base::rownames(first_matrix(res)), "Myc_reduce_mice_liver_24m_v1")
  expect_equal(base::colnames(first_matrix(res)), "Myc_reduce_mice_liver_24m_v2")
})

test_that("an OmicSignatureCollection is accepted as a list", {
  sigs <- example_signatures()
  collection <- OmicSignature::OmicSignatureCollection$new(
    metadata = base::list(collection_name = "toy", description = "toy collection"),
    OmicSigList = sigs[1:2]
  )

  direct <- OmicSignature::compare_omic_signatures(collection, method = "overlap", min_features = 3, max_feature = 10)
  wrapped <- SigRepo::compareSignatures(omic_signatures = collection, method = "overlap", min_features = 3, max_feature = 10)

  expect_identical(wrapped, direct)
})

## Input validation ####

test_that("a connection handler is required to fetch by id or name", {
  expect_error(SigRepo::compareSignatures(signature_ids = 11, method = "overlap"), "conn_handler")
  expect_error(SigRepo::compareSignatures(signature_names2 = "x", method = "overlap"), "conn_handler")
})

test_that("objects that are not OmicSignatures are rejected", {
  expect_error(
    SigRepo::compareSignatures(omic_signatures = base::list(a = base::data.frame(), b = base::data.frame()), method = "overlap"),
    "OmicSignature"
  )
})

test_that("calling with no signatures at all is an error", {
  expect_error(SigRepo::compareSignatures(method = "overlap"), "omic_signatures")
})
