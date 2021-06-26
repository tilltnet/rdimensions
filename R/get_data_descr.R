#' Get descriptions of all dimensions.ai data sources
#' @param token Character containg a dimensions.ai API token, use get_token() to aquire token.
#' @import purrr
#' @export
get_data_descriptions <-
  function(..., tidy_output = TRUE) {
    source_names <- dimensions_request()$sources
    source_names <- setNames(source_names, source_names)
    res <- purrr::map(source_names,
                      function(x)
                        dimensions_request(paste('describe source', x), ...))
    if (tidy_output)
      tidy_description(res)
    else
      res

  }

enframe_description_section <- function(description_section) {
  res <- purrr::map(description_section, function(x)
    unlist(x)) %>% tibble::enframe()
  res$value %>%
    purrr::map_dfr(function(x)
      tidyr::pivot_wider(tibble::enframe(x), names_from = "name", values_from = "value")) %>%
    dplyr::mutate(name = res$name) %>%
    dplyr::select(name, everything())
}

tidy_description <- function(descr)
  map(descr, function(x)
    map(x, enframe_description_section))
