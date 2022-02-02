if (interactive()) {
  test_that("pagination works", {
    res1 <- paginate_query_text('search publications
                   where
                   (category_for = 3448 and year = 1970 and type = "article")
                   return publications[id + abstract]')

    expect_true(length(res1) > 1)
  })
}


