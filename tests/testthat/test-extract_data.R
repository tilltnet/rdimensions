test_that("data extraction works for: grants", {
  expect_error({
    epi_grants_raw_data <- readRDS("inst/epi_grants_raw_data.Rds")
    res <- purrr::map(epi_grants_raw_data$grants[1:3]
        , ~{
          extract_data(data_entry = ., data_source = "grants")
        }) %>%
      bind_extracted_data()
    expect_equal(nrow(res$main_df), 3)
    }, NA)
})

test_that("data extraction works for: grants", {
  expect_error({
    epi_grants_raw_data <- readRDS("inst/epi_grants_raw_data.Rds")
    res <- purrr::map(epi_grants_raw_data$grants[1:3]
                      , ~{
                        extract_data2(data_entry = ., data_source = "grants")
                      }) %>%
      bind_extracted_data()
    expect_equal(nrow(res$main_df), 3)
  }, NA)
})

library(purrr)

epi_grants_raw_data$grants[[1]] %>%
  map(function(x) {
    x2 <- if(is.list(x)) flatten(x)
    try(as.data.frame(x2))
  })

