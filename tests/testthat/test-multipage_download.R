if (FALSE) {
  test_that("multi page downloads and extraction works", {
    result <-
      dimensions_search(search_source = "publications",
                        search_filter = includes(list(year = 2015,
                                                      category_for = 3448,
                                                      category_rcdc = 507,
                                                      type = "article"))) |>
      map(dimensions_request) |>
      bind_extracted_data()
  })
}


descr$publications$fields$name
