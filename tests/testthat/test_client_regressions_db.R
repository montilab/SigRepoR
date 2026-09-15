# Database-backed regressions for client bugs found by an end-to-end run of
# every client function (#202-#209). These upload and delete signatures, so
# they only run against a test stack (CI's ephemeral one, or the local Docker
# stack via SIGREPO_TEST_*), never production.

skip_unless_test_database <- function(){
  test_conn <- SigRepo::test_conn_handler
  testthat::skip_if(
    test_conn$host %in% c("sigrepo.org", "142.93.67.157"),
    "refusing to run database tests against production at sigrepo.org / 142.93.67.157"
  )
  test_conn
}

db_query <- function(test_conn, sql){
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  base::suppressWarnings(DBI::dbGetQuery(conn, sql))
}

db_execute <- function(test_conn, sql){
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  base::suppressWarnings(DBI::dbExecute(conn, sql))
}

open_connection_count <- function(){
  base::length(DBI::dbListConnections(RMySQL::MySQL()))
}

# Reference rows whose feature_hashkey matches createHashKey(), i.e. rows an
# upload can resolve. Skips when the database has too few of them.
matchable_features <- function(test_conn, table, organism, n){
  rows <- db_query(test_conn, base::sprintf(
    "SELECT f.feature_name FROM %s f JOIN organisms o ON o.organism_id = f.organism_id
     WHERE o.organism = '%s' AND f.feature_hashkey = MD5(LOWER(CONCAT(f.feature_name, f.organism_id)))
     LIMIT %d",
    table, organism, n
  ))
  testthat::skip_if(base::nrow(rows) < n, base::sprintf("the test database has fewer than %d matchable %s rows for %s", n, table, organism))
  rows$feature_name
}

# A Mus musculus transcriptomics signature built on the fixture's metadata,
# with a difexp, from features the database can resolve.
build_signature <- function(name, feature_names, direction = "bi-directional", group_label = TRUE){
  fixture <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))
  n <- base::length(feature_names)

  score <- base::round(base::seq(3, 0.5, length.out = n), 4)
  if (direction == "bi-directional") score <- score * base::rep(c(1, -1), length.out = n)

  difexp <- base::data.frame(
    probe_id = base::sprintf("probe_%03d", base::seq_len(n)),
    feature_name = feature_names,
    score = score,
    p_value = base::seq(0.0001, 0.04, length.out = n),
    adj_p = base::seq(0.001, 0.049, length.out = n),
    stringsAsFactors = FALSE
  )
  if (direction == "bi-directional") difexp$group_label <- base::factor(base::ifelse(score > 0, "up", "down"), levels = c("up", "down"))
  if (direction == "uni-directional" && group_label) difexp$group_label <- "All Features"

  signature_columns <- base::intersect(c("probe_id", "feature_name", "score", "group_label"), base::colnames(difexp))
  signature <- difexp[base::seq_len(base::min(10, n)), signature_columns]

  metadata <- fixture$metadata
  metadata$signature_name <- name
  metadata$direction_type <- direction
  metadata$PMID <- base::as.character(metadata$PMID)
  metadata$year <- base::as.character(metadata$year)
  OmicSignature::OmicSignature$new(metadata = metadata, signature = signature, difexp = difexp, print_message = FALSE)
}

unique_name <- function(label){
  base::paste0("test_regression_", label, "_", base::format(base::Sys.time(), "%Y%m%d%H%M%S"), "_", base::sample.int(1e6, 1))
}

upload_or_skip <- function(test_conn, sig){
  id <- SigRepo::addSignature(conn_handler = test_conn, omic_signature = sig, return_signature_id = TRUE, verbose = FALSE)
  if (base::is.null(id) || base::length(id) != 1) {
    testthat::skip("the test database could not accept the signature (reference features missing)")
  }
  id
}

delete_quietly <- function(test_conn, id){
  base::try(SigRepo::deleteSignature(conn_handler = test_conn, signature_id = id, verbose = FALSE), silent = TRUE)
}

test_that("searchProteomicsFeatureSet searches by feature_name (#202)", {
  test_conn <- skip_unless_test_database()
  feature_name <- matchable_features(test_conn, "proteomics_features", "Homo sapiens", 1)

  found <- SigRepo::searchProteomicsFeatureSet(conn_handler = test_conn, feature_name = feature_name, verbose = FALSE)
  expect_true(feature_name %in% found$feature_name)
  expect_true("organism" %in% base::colnames(found))

  found_hs <- SigRepo::searchProteomicsFeatureSet(conn_handler = test_conn, feature_name = feature_name, organism = "Homo sapiens", verbose = FALSE)
  expect_true(base::nrow(found_hs) >= 1)
  expect_true(base::all(found_hs$organism == "Homo sapiens"))
})

test_that("searchGeneticVariantsFeatureSet searches by feature_name (#205)", {
  test_conn <- skip_unless_test_database()
  variant_name <- unique_name("rs")

  # Seeded with SQL rather than addGeneticVariantsFeatureSet(), which needs the
  # SigRepo admin role; CI's test user is not an admin. The hashkey matches
  # createHashKey(). ####
  inserted <- db_execute(test_conn, base::sprintf(
    "INSERT INTO genetic_variants_features (feature_name, chromosome, position, annotation, organism_id, is_current, feature_hashkey)
     SELECT '%s', '1', 12345, 'SigRepo regression test', organism_id, 1, MD5(LOWER(CONCAT('%s', organism_id)))
     FROM organisms WHERE organism = 'Homo sapiens'",
    variant_name, variant_name
  ))
  on.exit(db_execute(test_conn, base::sprintf("DELETE FROM genetic_variants_features WHERE feature_name = '%s'", variant_name)), add = TRUE)
  testthat::skip_if(inserted == 0, "the test database has no Homo sapiens organism")

  found <- SigRepo::searchGeneticVariantsFeatureSet(
    conn_handler = test_conn,
    feature_name = variant_name,
    organism = "Homo sapiens",
    verbose = FALSE
  )
  expect_equal(found$feature_name, variant_name)
  expect_equal(found$organism, "Homo sapiens")
})

test_that("createCollectionMetadata builds the collection metadata table (#203)", {
  test_conn <- skip_unless_test_database()
  collection <- base::readRDS(testthat::test_path("test_data", "test_data_collection.rds"))

  metadata_tbl <- SigRepo::createCollectionMetadata(conn_handler = test_conn, omic_collection = collection)

  expect_s3_class(metadata_tbl, "data.frame")
  expect_equal(metadata_tbl$collection_name[1], collection$metadata$collection_name)
})

test_that("updateSignature keeps the difexp, so the signature can still be retrieved (#204)", {
  test_conn <- skip_unless_test_database()
  features <- matchable_features(test_conn, "transcriptomics_features", "Mus musculus", 30)
  sig <- build_signature(unique_name("update"), features)

  id <- upload_or_skip(test_conn, sig)
  on.exit(delete_quietly(test_conn, id), add = TRUE)

  SigRepo::updateSignature(conn_handler = test_conn, signature_id = id, omic_signature = sig, verbose = FALSE)

  retrieved <- SigRepo::getSignature(conn_handler = test_conn, signature_id = id, verbose = FALSE)[[1]]
  expect_equal(base::nrow(retrieved$difexp), 30)
  expect_setequal(retrieved$signature$probe_id, sig$signature$probe_id)
})

test_that("addSignatureWithID uploads the difexp (#207)", {
  test_conn <- skip_unless_test_database()
  features <- matchable_features(test_conn, "transcriptomics_features", "Mus musculus", 30)
  sig <- build_signature(unique_name("with_id"), features)

  max_id <- db_query(test_conn, "SELECT COALESCE(MAX(signature_id), 0) AS id FROM signatures")$id[1]
  id <- base::as.numeric(max_id) + 1000
  SigRepo::addSignatureWithID(
    conn_handler = test_conn,
    omic_signature = sig,
    assign_signature_id = id,
    assign_user_name = test_conn$user,
    verbose = FALSE
  )
  on.exit(delete_quietly(test_conn, id), add = TRUE)

  retrieved <- SigRepo::getSignature(conn_handler = test_conn, signature_id = id, verbose = FALSE)
  testthat::skip_if(!base::is.list(retrieved) || base::length(retrieved) == 0, "the test database could not accept the signature")
  expect_equal(base::nrow(retrieved[[1]]$difexp), 30)
})

test_that("a uni-directional signature without group_label can be added and retrieved (#208)", {
  test_conn <- skip_unless_test_database()
  features <- matchable_features(test_conn, "transcriptomics_features", "Mus musculus", 20)
  sig <- build_signature(unique_name("uni"), features, direction = "uni-directional", group_label = FALSE)
  expect_false("group_label" %in% base::colnames(sig$signature))

  id <- upload_or_skip(test_conn, sig)
  on.exit(delete_quietly(test_conn, id), add = TRUE)

  retrieved <- SigRepo::getSignature(conn_handler = test_conn, signature_id = id, verbose = FALSE)[[1]]
  expect_equal(base::nrow(retrieved$signature), 10)
  expect_true(base::all(base::as.character(retrieved$signature$group_label) == "All Features"))
})

test_that("a signature whose feature names differ in case from the reference can be retrieved (#209)", {
  test_conn <- skip_unless_test_database()
  features <- matchable_features(test_conn, "transcriptomics_features", "Mus musculus", 30)
  testthat::skip_if(base::identical(features, base::tolower(features)), "reference names are already lower case")
  sig <- build_signature(unique_name("case"), base::tolower(features))

  id <- upload_or_skip(test_conn, sig)
  on.exit(delete_quietly(test_conn, id), add = TRUE)

  retrieved <- SigRepo::getSignature(conn_handler = test_conn, signature_id = id, verbose = FALSE)[[1]]
  expect_true(base::all(retrieved$signature$feature_name %in% retrieved$difexp$feature_name))
})

test_that("client functions close their connections when they error (#206)", {
  test_conn <- skip_unless_test_database()

  before <- open_connection_count()
  expect_error(SigRepo::searchMetabolomicsFeatureSet(conn_handler = test_conn, feature_database = "not_a_database", verbose = FALSE))
  expect_equal(open_connection_count(), before)

  # The API is unreachable, so the difexp request fails after both
  # getSignature() and createOmicSignature() have opened a connection.
  signature <- db_query(test_conn, "SELECT signature_id FROM signatures WHERE has_difexp = 1 AND visibility = 1 LIMIT 1")
  testthat::skip_if(base::nrow(signature) == 0, "the test database has no public signature with a difexp")
  unreachable_api <- test_conn
  unreachable_api$api_host <- "http://127.0.0.1"
  unreachable_api$api_port <- 1

  before <- open_connection_count()
  expect_error(SigRepo::getSignature(conn_handler = unreachable_api, signature_id = signature$signature_id[1], verbose = FALSE))
  expect_equal(open_connection_count(), before)
})

# A users row for a second account, seeded with SQL because CI has a single
# non-admin login. It never logs in; it only has to satisfy the foreign keys. ####
seed_user <- function(test_conn, user_name){
  db_execute(test_conn, base::sprintf(
    "INSERT INTO users (user_name, user_password_hashkey, user_email, user_role, api_key, active, user_hashkey)
     VALUES ('%1$s', MD5('%1$s'), 'regression@sigrepo.org', 'editor', MD5(CONCAT('%1$s', 'api')), 1, MD5(CONCAT('%1$s', 'user')))",
    user_name
  ))
  user_name
}

# The rows an update must leave alone when it is refused or rolled back. ####
signature_state <- function(test_conn, id){
  base::list(
    signature = db_query(test_conn, base::sprintf("SELECT signature_name, description, user_name, visibility, has_difexp, signature_hashkey FROM signatures WHERE signature_id = %s", id)),
    features = db_query(test_conn, base::sprintf("SELECT feature_id, probe_id FROM signature_feature_set WHERE signature_id = %s ORDER BY feature_id, probe_id", id)),
    access = db_query(test_conn, base::sprintf("SELECT user_name, access_type FROM signature_access WHERE signature_id = %s ORDER BY user_name", id)),
    collections = db_query(test_conn, base::sprintf("SELECT collection_id FROM signature_collection_access WHERE signature_id = %s ORDER BY collection_id", id))
  )
}

test_that("updateSignature refuses a user who only has editor access, and changes nothing (#214)", {
  test_conn <- skip_unless_test_database()
  features <- matchable_features(test_conn, "transcriptomics_features", "Mus musculus", 30)
  name <- unique_name("shared_editor")
  id <- upload_or_skip(test_conn, build_signature(name, features))
  owner <- seed_user(test_conn, unique_name("owner"))

  # Hand the signature to another owner; the test user keeps editor access ####
  db_execute(test_conn, base::sprintf("UPDATE signatures SET user_name = '%s' WHERE signature_id = %s", owner, id))
  db_execute(test_conn, base::sprintf("UPDATE signature_access SET access_type = 'editor' WHERE signature_id = %s AND user_name = '%s'", id, test_conn$user))
  on.exit({
    db_execute(test_conn, base::sprintf("UPDATE signatures SET user_name = '%s' WHERE signature_id = %s", test_conn$user, id))
    db_execute(test_conn, base::sprintf("INSERT INTO signature_access (signature_id, user_name, access_type, access_sig_hashkey) VALUES (%s, '%s', 'owner', MD5('%s')) ON DUPLICATE KEY UPDATE access_type = 'owner'", id, test_conn$user, id))
    delete_quietly(test_conn, id)
    db_execute(test_conn, base::sprintf("DELETE FROM signature_access WHERE user_name = '%s'", owner))
    db_execute(test_conn, base::sprintf("DELETE FROM users WHERE user_name = '%s'", owner))
  }, add = TRUE)

  before <- signature_state(test_conn, id)
  expect_error(
    SigRepo::updateSignature(conn_handler = test_conn, signature_id = id, omic_signature = build_signature(name, features[-(1:2)]), verbose = FALSE),
    "permission to update"
  )
  expect_identical(signature_state(test_conn, id), before)
})

test_that("a failed updateSignature restores shared access and collection membership (#215)", {
  test_conn <- skip_unless_test_database()
  features <- matchable_features(test_conn, "transcriptomics_features", "Mus musculus", 30)
  name <- unique_name("rollback")
  id <- upload_or_skip(test_conn, build_signature(name, features))
  viewer <- seed_user(test_conn, unique_name("viewer"))
  collection_name <- unique_name("collection")

  # Share with a viewer and put the signature in a collection ####
  db_execute(test_conn, base::sprintf("INSERT INTO signature_access (signature_id, user_name, access_type, access_sig_hashkey) VALUES (%s, '%s', 'viewer', MD5('%s'))", id, viewer, viewer))
  db_execute(test_conn, base::sprintf("INSERT INTO collection (collection_name, description, user_name, visibility, collection_hashkey) VALUES ('%1$s', 'regression', '%2$s', 0, MD5('%1$s'))", collection_name, test_conn$user))
  collection_id <- db_query(test_conn, base::sprintf("SELECT collection_id FROM collection WHERE collection_name = '%s'", collection_name))$collection_id[1]
  db_execute(test_conn, base::sprintf("INSERT INTO signature_collection_access (collection_id, signature_id, signature_collection_hashkey) VALUES (%s, %s, MD5('%s'))", collection_id, id, collection_name))
  on.exit({
    delete_quietly(test_conn, id)
    db_execute(test_conn, base::sprintf("DELETE FROM signature_collection_access WHERE collection_id = %s", collection_id))
    db_execute(test_conn, base::sprintf("DELETE FROM collection WHERE collection_id = %s", collection_id))
    db_execute(test_conn, base::sprintf("DELETE FROM signature_access WHERE user_name = '%s'", viewer))
    db_execute(test_conn, base::sprintf("DELETE FROM users WHERE user_name = '%s'", viewer))
  }, add = TRUE)

  before <- signature_state(test_conn, id)
  testthat::expect_equal(base::nrow(before$access), 2)
  testthat::expect_equal(base::nrow(before$collections), 1)

  # Feature names the reference table cannot resolve make the update fail after it has started ####
  unknown <- build_signature(name, base::sprintf("NOT_A_FEATURE_%02d", base::seq_along(features)))
  result <- SigRepo::updateSignature(conn_handler = test_conn, signature_id = id, omic_signature = unknown, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  after <- signature_state(test_conn, id)
  expect_identical(after$signature, before$signature)
  expect_identical(after$features, before$features)
  expect_identical(after$access, before$access)
  expect_identical(after$collections, before$collections)
})
