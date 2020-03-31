test_that("get_deprecated_fields() works and has names", {
  expect_error(a <- get_deprecated_fields(), NA)
  expect_true(!is.null(names(a)))
})
