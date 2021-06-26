#' Get a dimensions.ai API token
#'
#' @param login A list with named entries "username" and "password"
#' @export
get_token <-
  function(login) {
    res <-
      httr::POST(
        paste0(base_url, "auth.json"),
        body = login,
        encode = "json"
      )
    message(httr::http_status(res)$message)
    stopifnot(httr::http_status(res)$categor == "Success")
    token <- httr::content(res)$token
    token <- paste0("JWT ", token)
    options(dimensions_token = token)
    message("Logged in. Token stored as global option.")
    invisible(token)
  }


