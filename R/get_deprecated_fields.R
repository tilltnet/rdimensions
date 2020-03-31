#' Get deprecated fields for dimensions.ai data sources
#'
#' @param data_source Character specifying data source, defaults to all.
#' @export
get_deprecated_fields <-
  function(data_source = c("all", "publications", "researchers", "patents", "policy_documents", "clinical_trials", "grants")) {

    data_sources <-
      switch(data_source[1],
             all = datasource_names,
             data_source)

    res <- map(data_sources, ~paste0("search ",. ," return ", ., "[all] limit 1")) %>%
      map(dimensions_request) %>%
      map( ~ .$`_warnings`) %>%
      map( ~ map(., ~ strsplit(., " ")) %>%
             map_chr( ~ .[[1]][[2]]) %>%
             gsub("'", "", .) %>%
             unique()) %>%
      setNames(data_sources)
  }

