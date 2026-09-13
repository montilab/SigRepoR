# lookup_table_sql() against a real MySQL: bare-column lookups still match the
# way trim(lower(col)) did, MySQL now uses the indexes, and the collations the
# case-insensitive matching depends on are still in place. Read-only, but only
# run against a test stack (CI's ephemeral one, or the local Docker stack via
# SIGREPO_TEST_*), never production.

skip_unless_test_database <- function(){
  test_conn <- SigRepo::test_conn_handler
  testthat::skip_if(
    base::identical(test_conn$host, "sigrepo.org"),
    "refusing to run database tests against production at sigrepo.org"
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

test_that("every text column lookup_table_sql() filters on is case-insensitive", {
  test_conn <- skip_unless_test_database()
  conn <- open_test_connection(test_conn)

  filtered_text_columns <- c(
    "user_name", "organism", "feature_name", "source_db", "signature_hashkey", "phenotype",
    "feature_hashkey", "sig_feature_hashkey", "sample_type", "platform_name", "collection_hashkey",
    "user_email", "signature_collection_hashkey", "metabolite_hashkey", "keyword", "access_type",
    "source_value", "signature_name", "collection_name"
  )
  collations <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT table_name, column_name, collation_name FROM information_schema.columns
     WHERE table_schema = DATABASE() AND collation_name IS NOT NULL AND column_name IN (%s)",
    base::paste0(base::as.character(DBI::dbQuoteString(conn, filtered_text_columns)), collapse = ", ")
  ))
  testthat::skip_if(base::nrow(collations) == 0, "the test database has no schema")

  not_ci <- collations[!base::grepl("_ci$", collations$collation_name), , drop = FALSE]
  expect_identical(
    base::nrow(not_ci), 0L,
    info = base::paste(base::sprintf("%s.%s is %s", not_ci$table_name, not_ci$column_name, not_ci$collation_name), collapse = "; ")
  )
})
