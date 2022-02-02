test_that("dimensions_search() works for each data source", {

  expect_error({
    queries <- map(sources, \(x) {
      dimensions_search(
        search_source = x,
        search_term = "ego-network",
        search_filter = includes(list(year = 2015)) ,
        return_fields = descr$publications$fields$name |>
          paste(collapse = "+"),
        paginate = FALSE
      )
    })
  }, NA)


})

if (interactive()) {
  test_that("dimensions_search() works for each data source (with pagination)",
            {
              expect_error({
                queries <- map(sources, \(x) {
                  dimensions_search(
                    search_source = x,
                    search_term = "ego-network",
                    search_filter = includes(list(year = 2015)) ,
                    return_fields = descr$publications$fields$name |>
                      paste(collapse = "+"),
                    paginate = TRUE
                  )
                })
              }, NA)
            })
}



