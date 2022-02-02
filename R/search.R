#' Search dimensions API and compose paginated query text
#' @export
dimensions_search <-
  function(search_source = c(
    "publications",
    "grants",
    "patents",
    "clinical_trials",
    "policy_documents",
    "researchers",
    "organizations",
    "datasets"
  ),
  search_term = NULL,
  search_filter = NULL,
  return = search_source,
  return_fields = "basics+abstract+extras+book",
  sort_by = NULL,
  paginate = TRUE,
  page_length = 1000,
  print_query = FALSE,
  session_token = getOption("dimensions_token")) {
    if (!is.null(search_term))
      search_term <- paste("for", string_quote(search_term))

    if (!is.null(search_filter))
      search_filter <- paste("where", search_filter)

    if (return_fields != "all" | !grepl("id+|+id", return_fields))
      return_fields <- paste0("id + ", return_fields)

    query <- paste(search_term, search_filter)
    cq <-
      paste(
        "search",
        search_source,
        query,
        "return",
        return,
        "[",
        return_fields,
        "]"
      ) %>%
      stringr::str_squish()

    if (!is.null(sort_by))
      cq <- paste(cq, "sort by", sort_by)

    if (print_query) print(cq)
    if (paginate) return(invisible(paginate_query_text(cq, page_length, session_token = session_token)))
    invisible(paste(cq, "limit 1000 skip 0"))
  }

#' Concatenate dimensions search filters
#' @export
concat <- function(x, sep1 = c(" and ", " or "), sep2 = c(" and ", " or ")) {
  res <- map2_chr(x, names(x), function(p, q) {
    if (is.character(p))
      p <- string_quote(p)
    paste0("(", (paste(paste(q, "=", p), collapse = sep1)), ")")
  }) %>% paste(collapse = sep2)
  paste0("(", res, ")")
}

#' @describeIn concat Creates filter with ' or '.
#' @export
or <- function(x) concat(x, " or ", sep2 = c(" and ", " or "))

#' @describeIn concat Creates filter with ' and '.
#' @export
and <- function(x) concat(x, " and ", sep2 = c(" and ", " or "))

#' @describeIn concat Creates filter with 'in [...]'.
#' @export
includes <- function(x, sep1 = c(" and ", " or "), sep2 = c(" and ", " or ")) {
  res <- map2_chr(x, names(x), function(p, q) {
    if (is.character(p))
      p <- string_quote(p)
    p <- paste0("[", paste(p, collapse = ", "), "]")
    paste0("(", (paste(paste(q, "in", p), collapse = sep1)), ")")
  }) %>% paste(collapse = sep2)
  paste0("(", res, ")")
}

string_quote <- function(x) paste0("\"", x , "\"")
