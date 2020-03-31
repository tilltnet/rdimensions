library(tidyverse)
library(rdimensions)
library(keyring)

email <- ''

session_token <- get_token(list(
  username = email,
  password = key_get("dimensions", email)
))

descr <- get_data_descriptions(session_token)
View(descr)

descr_fields <- purrr::map(descr, ~names(.$fields))
descr_fields

deprecated_fields <- get_deprecated_fields()

sociology_pubs <- dimensions_request(dsl_query = 'search publications
                   where
                   (category_for = 3448 and year = 1970 and type = "article")
                   return publications[all] limit 1')

all_query_texts <- map(2015:2020, ~{
  paginate_query_text(paste0('search publications
                      where
                      (category_for = 3448 and year = ', .,')
                      return publications[all]'))
}) %>% unlist()


raw_data <- map(all_query_texts, dimensions_request)

extracted_data <-
  map(
    raw_data,
    ~ map(.$publications, extract_data, data_source = "publications") %>% bind_extracted_data
  ) %>% bind_extracted_data()
extracted_data$main_df %>% View()
