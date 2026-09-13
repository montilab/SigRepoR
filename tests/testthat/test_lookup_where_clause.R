# The WHERE clause lookup_table_sql() sends to MySQL, built without a database:
# DBI::ANSI() quotes strings the same way a MySQL connection does for these
# values. The clause must compare bare columns -- trim(lower(col)) stops MySQL
# using indexes and turned a 2 ms lookup into a 1.3 s full table scan.

where_clause <- function(...) SigRepo:::build_lookup_where_clause(DBI::ANSI(), ...)

test_that("a filter compares the bare column so MySQL can use its index", {
  expect_identical(
    where_clause("signature_id", list(signature_id = 275)),
    "signature_id IN ('275')"
  )
})

test_that("no filter wraps the column in a function", {
  clause <- where_clause(c("signature_id", "user_name"), list(signature_id = 1, user_name = "devadmin"), "AND")
  expect_no_match(clause, "trim\\(|lower\\(", ignore.case = TRUE)
})

test_that("values are trimmed but their case is left to the column's collation", {
  expect_identical(
    where_clause("organism", list(organism = "  Homo Sapiens ")),
    "organism IN ('Homo Sapiens')"
  )
})

test_that("every value is quoted, including quotes and injection attempts", {
  clause <- where_clause("user_name", list(user_name = c("O'Brien", "x'); DROP TABLE users; --")))
  expect_identical(clause, "user_name IN ('O''Brien', 'x''); DROP TABLE users; --')")
})

test_that("a filter with no values matches nothing instead of producing invalid SQL", {
  expect_identical(where_clause("signature_id", list(signature_id = character())), "1 = 0")
})

test_that("several filters are joined with the given logical operators", {
  expect_identical(
    where_clause(
      c("signature_id", "user_name", "access_type"),
      list(signature_id = c(1, 2), user_name = "devadmin", access_type = "owner"),
      c("AND", "OR")
    ),
    "signature_id IN ('1', '2') AND user_name IN ('devadmin') OR access_type IN ('owner')"
  )
})

test_that("thousands of values all reach the clause in order", {
  ids <- as.character(seq_len(15000))
  clause <- where_clause("feature_id", list(feature_id = ids))
  expect_identical(clause, paste0("feature_id IN (", paste0("'", ids, "'", collapse = ", "), ")"))
})
