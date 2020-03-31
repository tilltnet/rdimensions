test_that("multiplication works", {
  asd <- dimensions_request(dsl_query = 'search publications
                   where
                   (category_for = 3448 and year = 1970 and type = "article")
                   return publications[all] limit 1')
})
