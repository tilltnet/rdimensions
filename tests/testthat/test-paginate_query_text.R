test_that("multiplication works", {
  paginate_query_text('search publications
                   where
                   (category_for = 3448 and year = 1970 and type = "article")
                   return publications[all]')

  pasta('search publications
                   where
                   (category_for = 3448 and year = 1970 and type = "article")
                   return publications[all]')
})
