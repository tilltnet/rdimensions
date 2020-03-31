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
    paste0("JWT ", httr::content(res)$token)
  }


