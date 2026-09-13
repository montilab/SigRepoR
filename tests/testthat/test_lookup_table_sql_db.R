# lookup_table_sql() against a real MySQL: lookups still match exactly what
# trim(lower(col)) matched, MySQL now uses the indexes, and the collations the
# case-insensitive matching depends on are still in place. Read-only, but only
# run against a test stack (CI's ephemeral one, or the local Docker stack via
# SIGREPO_TEST_*), never production.

skip_unless_test_database <- function(){
  test_conn <- SigRepo::test_conn_handler
  testthat::skip_if(
    test_conn$host %in% c("sigrepo.org", "142.93.67.157"),
    "refusing to run database tests against production at sigrepo.org / 142.93.67.157"
  )
  test_conn
}

open_test_connection <- function(test_conn, env = parent.frame()){
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  withr::defer(base::suppressWarnings(DBI::dbDisconnect(conn)), envir = env)
  conn
}

test_that("text lookups still ignore case and surrounding spaces", {
  test_conn <- skip_unless_test_database()
  conn <- open_test_connection(test_conn)

  organisms <- DBI::dbGetQuery(conn, "SELECT organism FROM organisms LIMIT 1")
  testthat::skip_if(base::nrow(organisms) == 0, "the test database has no organisms")
  stored <- organisms$organism[1]

  found <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "organisms",
    return_var = "organism",
    filter_coln_var = "organism",
    filter_coln_val = base::list(organism = base::paste0("  ", base::toupper(stored), "  "))
  )

  expect_identical(found$organism, stored)
})

test_that("a malformed signature id matches nothing, as it did before", {
  test_conn <- skip_unless_test_database()
  conn <- open_test_connection(test_conn)

  one <- DBI::dbGetQuery(conn, "SELECT CAST(signature_id AS CHAR) AS signature_id FROM signatures LIMIT 1")
  testthat::skip_if(base::nrow(one) == 0, "the test database has no signatures")
  id <- one$signature_id[1]

  lookup <- function(value){
    SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "signature_id",
      filter_coln_var = "signature_id",
      filter_coln_val = base::list(signature_id = value)
    )
  }

  # MySQL converts '1abc' and '01' to the number 1 when compared with an
  # integer column, so a bare signature_id IN (...) would match these. ####
  expect_identical(base::nrow(lookup(base::paste0(id, "abc"))), 0L)
  expect_identical(base::nrow(lookup(base::paste0("0", id))), 0L)
  expect_identical(base::nrow(lookup(id)), 1L)
})

test_that("a lookup by signature_id uses the index instead of scanning signature_feature_set", {
  test_conn <- skip_unless_test_database()
  conn <- open_test_connection(test_conn)

  one <- DBI::dbGetQuery(conn, "SELECT CAST(signature_id AS CHAR) AS signature_id FROM signature_feature_set LIMIT 1")
  testthat::skip_if(base::nrow(one) == 0, "the test database has no signature features")

  clause <- SigRepo:::build_lookup_where_clause(
    conn = conn,
    filter_coln_var = "signature_id",
    filter_coln_val = base::list(signature_id = one$signature_id[1])
  )
  plan <- DBI::dbGetQuery(conn, base::paste("EXPLAIN SELECT * FROM signature_feature_set WHERE", clause))

  expect_false(base::identical(plan$type[1], "ALL"))
  expect_false(base::is.na(plan$key[1]))
})

test_that("every text column lookup_table_sql() filters on is case-insensitive and PAD SPACE", {
  test_conn <- skip_unless_test_database()
  conn <- open_test_connection(test_conn)

  filtered_text_columns <- c(
    "user_name", "organism", "feature_name", "source_db", "signature_hashkey", "phenotype",
    "feature_hashkey", "sig_feature_hashkey", "sample_type", "platform_name", "collection_hashkey",
    "user_email", "signature_collection_hashkey", "metabolite_hashkey", "keyword", "access_type",
    "source_value", "signature_name", "collection_name", "refmet_name", "api_key"
  )
  collations <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT c.table_name AS table_name, c.column_name AS column_name,
            c.collation_name AS collation_name, co.pad_attribute AS pad_attribute
     FROM information_schema.columns c
     JOIN information_schema.collations co ON c.collation_name = co.collation_name
     WHERE c.table_schema = DATABASE() AND c.collation_name IS NOT NULL AND c.column_name IN (%s)",
    base::paste0(base::as.character(DBI::dbQuoteString(conn, filtered_text_columns)), collapse = ", ")
  ))
  testthat::skip_if(base::nrow(collations) == 0, "the test database has no schema")

  describe <- function(rows){
    base::paste(
      base::sprintf("%s.%s is %s (%s)", rows$table_name, rows$column_name, rows$collation_name, rows$pad_attribute),
      collapse = "; "
    )
  }

  not_ci <- collations[!base::grepl("_ci$", collations$collation_name), , drop = FALSE]
  expect_identical(base::nrow(not_ci), 0L, info = describe(not_ci))

  not_pad_space <- collations[collations$pad_attribute != "PAD SPACE", , drop = FALSE]
  expect_identical(base::nrow(not_pad_space), 0L, info = describe(not_pad_space))
})
