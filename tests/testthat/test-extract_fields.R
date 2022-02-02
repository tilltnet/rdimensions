test_that("extract_fields() works", {
  expect_error({
    extracted_fields <-
      imap(responses, \(x, name) {
        #print(name)
        res <- extract_fields(x)
        res
      })
  }, NA)
})
