#' Create a pagination character vector to add to a query with more than 1000 results
#' @export
paginate_query_text <- function(query_text, page_length = 1000, session_token = getOption("dimensions_token")) {
  query_text <- pasta(query_text)
  result_count <- dimensions_request(dsl_query = query_text, session_token = session_token)$`_stats`$total_count
  query_text <- strsplit(query_text, " limit")[[1]][1]
  iterations <- ceiling(result_count / page_length)
  skips <- c(0, 1:{
    iterations - 1
  } * page_length)[1:iterations]
  paste(query_text, "limit", page_length, "skip", skips)
}

#' @export
pasta <-
  function(...)
    paste0(stringr::str_squish(gsub("\n", "", list(...))), collapse = "")

#' @describeIn paginate_query_text Split query text entry in a vector previously created with paginate_query_text into smaller pages. Useful, when any query text fails to complete and you want to rerun only those that failed.
#' @export
split_query_text <-
  function(query_texts, query_number, page_length = 100) {
    query_limit_skip <-
      strsplit(query_texts[query_number], " limit ")[[1]]
    limit_skip <-
      as.numeric(strsplit(query_limit_skip[[2]], " skip ")[[1]])
    skips <-
      seq(limit_skip[2], (limit_skip[2] + limit_skip[1]), page_length)
    paste(query_limit_skip[1], "limit", page_length, "skip", skips[-length(skips)])
  }

#' @describeIn paginate_query_text Like `split_query_text()` but returns full list of query text including unchanged ones. Useful, when any query text fails to complete and you want to rerun all queries.
#' @export
split_and_replace_query_text <-
  function(query_texts, query_number, page_length = 100) {
    c(query_texts[1:(query_number - 1)],
      split_query_text(query_texts, query_number, page_length),
      query_texts[(query_number + 1):length(query_texts)])
  }