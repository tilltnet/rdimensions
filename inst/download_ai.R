library(tidyverse)
library(rdimensions)
library(keyring)

email <- ""

session_token <- get_token(list(
  username = email,
  password = key_get("dimensions", email)
))

descr <- get_data_descriptions(session_token)
descr_fields <- purrr::map(descr, ~names(.$fields))

deprecated_fields <- get_deprecated_fields()

geospatial_science_pubs <-
  dimensions_request(
    'search publications for "deep learning" OR "artificial intelligence" OR "machine learning"
    where research_orgs = "grid.15276.37" and year > 2014 and
    (category_for = 3410 or category_for = 2207 or category_for = 2539)
    return publications[all] limit 1000'
  )

geospatial_science_pubs$publications %>% length()

geospatial_science_pubs_extracted <-
  map(geospatial_science_pubs$publications, extract_data, data_source = "publications") %>%
  bind_extracted_data()

pub_years_geospatial_science <- geospatial_science_pubs_extracted$main_df %>%
  select(xid, year)

geospatial_science_pubs_extracted$authors %>%
  filter(id == "grid.15276.37" | current_organization_id == "grid.15276.37") %>%
  count(first_name, last_name, sort = TRUE)

geospatial_science_pubs_extracted$category_for %>%
  filter(id %in% c(3410, 2207, 2539)) %>%
  mutate(field = case_when(
    id == 2539 ~ "Geography",
    id == 3410 ~ "Geography",
    id == 2207 ~ "CALS")
  ) %>%
  distinct(xid, field) %>%
  full_join(pub_years_geospatial_science) %>%
  ggplot(aes(year, fill = field)) +
  geom_bar(position = "dodge")

geospatial_science_pubs_extracted$category_for %>%
  filter(id %in% c(3410, 2207, 2539)) %>%
  mutate(field = case_when(
    id == 2539 ~ "Geography",
    id == 3410 ~ "Geography",
    id == 2207 ~ "CALS")
  ) %>%
  distinct(xid, field) %>%
  full_join(pub_years_geospatial_science) %>%
  filter(year > 2014) %>%
  ggplot(aes(year, fill = field)) +
  geom_bar(position = "dodge")

length(geospatial_science_pubs$publications)

geospatial_science_grants <-
  dimensions_request(
    'search grants in full_data for "geospatial science"
    where research_orgs = "grid.15276.37" and
    (category_for = 3410 or category_for = 2207 or category_for = 2539)
    return grants[all] limit 1000'
  )

length(geospatial_science_grants$grants)

geospatial_pubs <-
  dimensions_request(
    'search publications for "geospatial"
    where research_orgs = "grid.15276.37" and
    (category_for = 3410 or category_for = 2207 or category_for = 2539)
    return publications[all] limit 1000'
  )

geospatial_pubs_extracted <-
  map(geospatial_pubs$publications, extract_data, data_source = "publications") %>%
  bind_extracted_data()

geospatial_pubs_extracted$category_for %>%
  filter(id %in% c(3410, 2207, 2539)) %>%
  mutate(field = case_when(
    id == 2539 ~ "Geography",
    id == 3410 ~ "Geography",
    id == 2207 ~ "CALS")
  ) %>%
  distinct(xid, field) %>%
  ggplot(aes(field)) +
  geom_bar()

pub_years <- geospatial_pubs_extracted$main_df %>%
  select(xid, year)

geospatial_pubs_extracted$main_df %>%
  filter(year > 2014) %>%
  pull(xid) %>%
  unique() %>%
  length()

geospatial_pubs_extracted$category_for %>%
  filter(id %in% c(3410, 2207, 2539)) %>%
  mutate(field = case_when(
    id == 2539 ~ "Geography",
    id == 3410 ~ "Geography",
    id == 2207 ~ "CALS")
  ) %>%
  distinct(xid, field) %>%
  full_join(pub_years) %>%
  ggplot(aes(year, fill = field)) +
  geom_bar(position = "dodge")

geospatial_pubs_extracted$category_for %>%
  filter(id %in% c(3410, 2207, 2539)) %>%
  mutate(field = case_when(
    id == 2539 ~ "Geography",
    id == 3410 ~ "Geography",
    id == 2207 ~ "CALS")
  ) %>%
  distinct(xid, field) %>%
  full_join(pub_years) %>%
  filter(year>2014) %>%
  ggplot(aes(year, fill = field)) +
  geom_bar(position = "dodge")

geospatial_pubs_extracted$category_for %>%
  filter(id %in% c(3410, 2207, 2539)) %>%
  mutate(field = case_when(
    id == 2539 ~ "Geography",
    id == 3410 ~ "Geography",
    id == 2207 ~ "CALS")
  ) %>%
  distinct(xid, field) %>%
  full_join(pub_years) %>%
  filter(year > 2014) %>%
  pull(xid) %>%
  unique() %>%
  length()

geospatial_pubs_extracted$researchers %>%
  count(first_name, last_name, sort = TRUE)

geospatial_pubs_extracted$authors %>%
  filter(id == "grid.15276.37" | current_organization_id == "grid.15276.37") %>%
  count(first_name, last_name, sort = TRUE)



# testing -----------------------------------------------------------------



xxx <-
  dimensions_request(
    'search publications in full_data for "\\\"geospatial science\\\""
    where research_orgs = "grid.15276.37"
    return publications[all] limit 1000'
  )

xxx$publications %>% length()

extract_data(xxx$publications[[1]], data_source = "publications")

zzz <-
  dimensions_request(
    'search publications in full_data for "geospatial"
    where research_orgs = "grid.15276.37"
    return publications[all] limit 1000'
  )

xz <-
  dimensions_request(
    'search publications in full_data for "\\\"geospatial science\\\""
    return publications[all] limit 1000'
  )

xz$publications %>% length()