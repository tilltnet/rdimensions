# Download current grid db
grid_db <- rdimensions::get_grid_db()

# Extract Grid Names and grid ids.
grid_ids <-
  grid_db$grid_institutes |>
  select(grid_id, name) |>
  tibble::deframe()

# Store as package data.
usethis::use_data(grid_ids, overwrite = TRUE)
