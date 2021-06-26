#' Send DSL query to Dimensions API and collect results as list based on json structure
#' @export
dimensions_request <-
  function(dsl_query = 'describe',
           session_token = getOption("dimensions_token"),
           fail_on_error = TRUE) {
    dsl_query <- pasta(dsl_query)

      if (is.null(session_token))
        stop(
          "No session_token found. Obtain session_token with dimensions_login()."
        )

    message("Sending request...")
    a <- httr::POST(
      paste0(base_url, "dsl.json"),
      body = dsl_query,
      config = httr::add_headers(Authorization = session_token),
      encode = "json"
    )

    message(paste(httr::http_status(a)$message))

    b <- httr::content(a, as = "parsed", simplifyVector = FALSE)
    success <- !httr::http_error(a)
    if (!success & fail_on_error) {
      stop("Request Failed with ", paste(b$errors$query$header, b$errors$query$details))
    } else if (!success)
      warning("Request Failed with ", httr::content(a) %>%
                rvest::html_text())
    message(paste("Results:", b$`_stats`$total_count, " Downloaded:", b$`_stats`$limit))
    b

  }
