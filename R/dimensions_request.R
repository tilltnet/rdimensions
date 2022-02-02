#' Send request to the Dimensions API
#'
#' Requests can be for data or API self-description ('describe' or 'describe source ...'). With `auto.extract = TRUE` the data will be returned as a list of `data.frames`.
#' @param fail_on_error `Logical`, TRUE: raise an error if HTTP request fails, FALSE: warn.
#' @param dsl_query `Character`, expressing request using [Dimensions API DSL](https://docs.dimensions.ai/dsl/api.html).
#' @param session_token `Character`, Dimensions API session token.
#' @param describe `Logical`, set true if API self-description commands ('describe' or 'describe source ...') are used.
#' @param auto.extract `Logical`, determines if the received data is processed and returned as a list of `data.frames`. If `FALSE`, data is returned as a nested list representing the JSON structure; `extract_fields()` can be used to turn it into a list of `data.frames`.
#' @export
dimensions_request <-
  function(dsl_query = 'describe',
           session_token = getOption("dimensions_token"),
           fail_on_error = TRUE,
           describe = FALSE,
           auto.extract = TRUE) {
    dsl_query <- pasta(dsl_query)
      if (is.null(session_token))
        stop(
          "No session_token found. Obtain session_token with `get_token()`."
        )

    message("Sending request...")
    a <- httr::POST(
      paste0(base_url, "dsl.json"),
      body = dsl_query,
      config = httr::add_headers(Authorization = session_token),
      encode = I("json")
    )
    message(paste(httr::http_status(a)$message))

    b <- httr::content(a, as = "parsed", simplifyVector = FALSE, flatten = TRUE)
    success <- !httr::http_error(a)
    if (!success & fail_on_error) {
      stop("Request failed with ", paste(b$errors$query$header, b$errors$query$details))
    } else if (!success)
      warning("Request failed with ", httr::content(a) %>%
                rvest::html_text())
    if (!is.null(b$`_stats`$limit) & !is.null(b$`_stats`$total_count))  {
      message(paste("Results:", b$`_stats`$total_count, " Downloading up to:", b$`_stats`$limit))
    } else if (!is.null(b$`_stats`$total_count)) {
      message(paste("Results:", b$`_stats`$total_count))
    }

    if (describe) return(b)

    # If not a description request, the content is extracted as a list and
    # the meta data is added as an attribute.
    returned_names <- names(b)
    meta_lgl <- grepl("^_", returned_names)
    res <- pluck(b[!meta_lgl], 1)
    attr(res, "meta") <- b[meta_lgl]

    # Turn all logical elements into character, to avoid merging issues with
    # partially missing data.
    res <- rapply(res,
           classes = "logical" ,
           f = \(x) as.character(x),
           how = "replace")
    if (auto.extract) extract_fields(res) else res
  }
