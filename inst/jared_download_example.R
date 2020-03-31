library(tidyverse)
library(rdimensions)
library(keyring)

email <- ''

session_token <- get_token(list(
  username = email,
  password = key_get("dimensions", email)
))

descr <- get_data_descriptions(token)
descr_fields <- purrr::map(descr, ~names(.$fields))

deprecated_fields <- get_deprecated_fields()

sociology_pubs <- dimensions_request('search publications
                   where
                   (category_for = 3448 and year = 1970 and type = "article")
                   return publications[all] limit 1')

all_queries_texts <- map(1970:2020, ~{
  create_pagination_chr(paste0('search publications
                      where
                      (category_for = 3448 and year = ', .,')
                      return publications[all]'))
}) %>% unlist()


raw_data <- map(all_queries_texts, dimensions_request)

extracted_data <- map(raw_data, ~map(.$publications, extract_data, data_source = "publications") %>% bind_extracted_data) %>% bind_extracted_data()
extracted_data$main_df %>% View()
