# The WHERE clause lookup_table_sql() sends to MySQL. DBI::ANSI() is used so
# the SQL can be checked without a database: it escapes a single quote by
# doubling it, whereas an RMySQL connection escapes it with a backslash. Both
# are safe; these tests pin ANSI's output. Each filter is a pair -- the bare
# column first, so MySQL can use its index (trim(lower(col)) alone turned a
# 2 ms lookup into a 1.3 s full table scan), then the original
# trim(lower(col)) comparison, so exactly the rows the old query matched
# are returned.

where_clause <- function(...) SigRepo:::build_lookup_where_clause(DBI::ANSI(), ...)

test_that("a filter pairs the bare column with the original comparison", {
  expect_identical(
    where_clause("signature_id", list(signature_id = 275)),
    "(signature_id IN ('275') AND trim(lower(signature_id)) IN ('275'))"
  )
})

test_that("each filter starts with the bare column, which lets MySQL use the index", {
  clause <- where_clause(c("signature_id", "user_name"), list(signature_id = 1, user_name = "devadmin"), "AND")
  for(col in c("signature_id", "user_name")){
    bare <- regexpr(paste0("(", col, " IN ("), clause, fixed = TRUE)
    wrapped <- regexpr(paste0("trim(lower(", col, ")) IN ("), clause, fixed = TRUE)
    expect_true(bare > 0)
    expect_true(wrapped > 0)
    expect_true(bare < wrapped)
  }
})

test_that("values are trimmed and lower-cased as the original lookup did", {
  expect_identical(
    where_clause("organism", list(organism = "  Homo Sapiens ")),
    "(organism IN ('homo sapiens') AND trim(lower(organism)) IN ('homo sapiens'))"
  )
})

test_that("every value is quoted, including quotes and injection attempts", {
  clause <- where_clause("user_name", list(user_name = c("O'Brien", "x'); DROP TABLE users; --")))
  expect_identical(
    clause,
    "(user_name IN ('o''brien', 'x''); drop table users; --') AND trim(lower(user_name)) IN ('o''brien', 'x''); drop table users; --'))"
  )
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
    paste(
      "(signature_id IN ('1', '2') AND trim(lower(signature_id)) IN ('1', '2'))",
      "AND (user_name IN ('devadmin') AND trim(lower(user_name)) IN ('devadmin'))",
      "OR (access_type IN ('owner') AND trim(lower(access_type)) IN ('owner'))"
    )
  )
})

test_that("thousands of values all reach both predicates in order", {
  ids <- as.character(seq_len(15000))
  values <- paste0("'", ids, "'", collapse = ", ")
  clause <- where_clause("feature_id", list(feature_id = ids))
  expect_identical(
    clause,
    paste0("(feature_id IN (", values, ") AND trim(lower(feature_id)) IN (", values, "))")
  )
})
