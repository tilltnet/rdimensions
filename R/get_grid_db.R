#' Download current version of grid.ac database
#' @import dplyr
#' @export
get_grid_db <- function() {
  file_name <- tempfile("grid_db_file")
  grid_dir <- paste0(tempdir(), "/grid/")
  xml2::read_html("https://doi.org/10.6084/m9.figshare.9963719") %>%
    rvest::html_nodes("a.download-button") %>%
    rvest::html_attr("href") %>%
    utils::download.file(destfile = file_name)

  utils::unzip(file_name, exdir = grid_dir)

  dir(tempdir())

  grid_csv_l <-
    dir(grid_dir, pattern = ".csv", recursive = TRUE, full.names = TRUE) %>%
    map(readr::read_csv)

  names(grid_csv_l) <-
    gsub(".csv", "", dir(grid_dir, pattern = ".csv", recursive = TRUE)) %>%
    strsplit("/") %>%
    unlist() %>%
    .[. != "full_tables"] %>%
    {ifelse(. == "grid", ., paste0("grid_", .))}

  grid_csv_l
}
