#' Get descriptions of all dimensions.ai data sources
#' @param token Character containg a dimensions.ai API token, use get_token() to aquire token.
#' @import purrr
#' @export
get_data_descriptions <-
  function(token) {
    descr <- list()
    descr$publications <-
      dimensions_request('describe source publications', session_token = token)
    descr$patents <-
      dimensions_request('describe source patents', session_token = token)
    descr$clinical_trials <-
      dimensions_request('describe source clinical_trials', session_token = token)
    descr$policy_documents <-
      dimensions_request('describe source patents', session_token = token)
    descr$grants <-
      dimensions_request('describe source grants', session_token = token)
    descr$researchers <-
      dimensions_request('describe source researchers', session_token = token)
    descr
  }
