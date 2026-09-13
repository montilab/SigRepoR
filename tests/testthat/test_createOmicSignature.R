# Offline checks for the helpers createOmicSignature() uses to rebuild a
# signature fetched from the database.

test_that("alignSignatureFeatureNames takes the difexp spelling when names differ only in case (#209)", {
  signature <- base::data.frame(
    probe_id = c("p1", "p2", "p3"),
    feature_name = c("ENSMUSG00000000001", "ENSMUSG00000000002", "ENSMUSG00000000003"),
    stringsAsFactors = FALSE
  )
  difexp <- base::data.frame(
    probe_id = c("p3", "p2", "p1", "p4"),
    feature_name = c("ENSMUSG00000000003", "ensmusg00000000002", "ensmusg00000000001", "ensmusg00000000004"),
    stringsAsFactors = FALSE
  )

  aligned <- SigRepo:::alignSignatureFeatureNames(signature = signature, difexp = difexp)

  expect_equal(aligned$feature_name, c("ensmusg00000000001", "ensmusg00000000002", "ENSMUSG00000000003"))
})

test_that("alignSignatureFeatureNames leaves names that differ by more than case alone", {
  signature <- base::data.frame(probe_id = c("p1", "p2"), feature_name = c("GeneA", NA), stringsAsFactors = FALSE)
  difexp <- base::data.frame(probe_id = c("p1", "p2"), feature_name = c("GeneB", "geneb"), stringsAsFactors = FALSE)

  aligned <- SigRepo:::alignSignatureFeatureNames(signature = signature, difexp = difexp)
  expect_equal(aligned$feature_name, c("GeneA", NA))

  expect_identical(SigRepo:::alignSignatureFeatureNames(signature = signature, difexp = NULL), signature)
})
