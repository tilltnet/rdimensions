if (interactive()) {

  test_that("dimensions_request() works", {
    ds_req1 <- dimensions_request(dsl_query = 'search publications
                   where
                   (category_for = 3448 and year = 2022 and type = "article")
                   return publications[basics+extras+abstract+categories+book+acknowledgements] limit 100')

    ds_sources <- dimensions_request("describe", describe = TRUE)
    ds_descr <- dimensions_request("describe source publications", describe = TRUE)
  })

  test_that("dimensions_request() works with `auto.extract = TRUE`", {
    ds_req2 <- dimensions_request(dsl_query = 'search publications
                   where
                   (category_for = 3448 and year = 2022 and type = "article")
                   return publications[basics+extras+abstract+categories+book+acknowledgements] limit 100',
                                  auto.extract = TRUE)
  })

}

