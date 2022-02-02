library(testthat)
library(rdimensions)
library(tictoc)
library(purrr)

# Run only if not on CI, else use mock data.
if (!isTRUE(as.logical(Sys.getenv("CI")))) {

  # Login, if not logged in already.
  if (is.null(getOption("dimensions_token"))) {
    library(keyring)
    token <- get_token(list(
      username = "till.krenz@ufl.edu",
      password = key_get(
        service = "dimensions",
        username =  NULL,
        keyring = "dimensions_credentials"
      )
    ))
  }

  # Get descriptions
  sources <- unlist(dimensions_request(describe = TRUE)$sources)
  names(sources) <- sources

  descr <- get_source_descriptions()

  # Make requests

  responses <-
    map(sources, \(x) {
      dimensions_search(x,
                        return_fields = descr[[x]]$fields$name |>
                          paste(collapse = "+"), paginate = FALSE) |>
        pluck(1) |>
        dimensions_request(auto.extract = FALSE)
    })

  tic()
  extracted_fields <-
    imap(responses, \(x, name) {
      print(name)
      tic()
      res <- extract_fields(x)
      toc()
      res
    })
  toc()

  test_data <- extracted_fields

  # Save as mock data

  make_mock_data <-
    function(x)
      rapply(x, f = \(y) {
        if (is.character(y))
          return("ABCDE")
        else if (is.numeric(y)) {
          1337
        } else {
          y
        }
      },
      how = "replace")

  mock_responses <-
    responses |>
    map(make_mock_data)

  usethis::use_data(mock_responses)

  mock_data <-
    mock_responses |>
    map(extract_fields)

  usethis::use_data(mock_data)

} else {

  # load mock data

  data(mock_data, package = "rdimensions")
  data(mock_responses, package = "rdimensions")

  test_data <- mock_data
  responses <- mock_responses
  message("Test data is mocked.")
}

test_check("rdimensions")
