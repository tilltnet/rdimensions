#' Cycles through all results and determines the type/class of for a specified data field.
determine_field_type <-
  function(raw_elements, field_name) {
    type_df <-
      list(
        null =
          map_lgl(raw_elements, \(element) is_null(element[[field_name]]) |
                    length(element[[field_name]]) == 0),
        atomic = map_lgl(raw_elements, \(element) is.atomic(element[[field_name]])),
        has_names = map_lgl(raw_elements, \(element) all(map_lgl(
          element[[field_name]], \(y) !is.null(names(y))
        )))
      ) |>
      bind_cols()

    type_df |>
      filter(!null) |>
      distinct() |>
      summarise(atomic = atomic,
                has_names = has_names) |>
      mutate(null = list(type_df$null)) |>
      distinct()
  }

#' Extracts one field from a list of `raw_elements`.
extract_field <- function(raw_elements, field_name, atomic, has_names, id, null_lgl) {
  data_transformer <-
    if (atomic) {
      \(x) {
        as.data.frame(x) |>
        setNames(field_name)
        }
    } else if (!atomic & !has_names) {
      \(x) {
        unlist(x) |> data.frame() |>
          setNames(field_name)
        }
    } else if (!atomic & has_names) {
      \(x)
        #res <- enlist_vector(y)
        # bind_rows(x)
      map_dfr(x, \(y) map(y, \(z) if (is.list(z)) list(z) else z))

       }

  names(raw_elements) <- id
  res <- map_dfr(raw_elements[!null_lgl],
                 \(element) {
                   data_transformer(element[[field_name]])
                 }, .id = "xid")


  as_tibble(res)
}

get_all_field_names <- function(raw_elements) {
  res <- map(raw_elements, names) |>
    unlist() |>
    unique()
  names(res) <- res
  res
}

#' Extracts all data fields from a `raw_elements` list.
#'
#' `dimensions_request(..., auto.extract = FALSE)` returns a
#' nested list. `extract_fields()` can be used to turn that list
#' into a list of `data.frames` where each field is represented by
#' `one data.frame`.
#' @param raw_elements `List` of results, as returned by `dimensions_request()`
#' @export
extract_fields <- function(raw_elements) {
  ids <- map_chr(raw_elements, \(x) x[["id"]])

  field_types_df <-
    get_all_field_names(raw_elements) |>
    map_dfr(\(field_name) determine_field_type(raw_elements, field_name), .id = "field_name")


  field_names <- field_types_df$field_name
  names(field_names) <- field_names

  pmap(
    list(
      field_names,
      field_types_df$atomic,
      field_types_df$has_names,
      field_types_df$null
    ),
    \(fn, at_lgl, hn_lgl, null_lgl) {
      extract_field(
        raw_elements,
        field_name = fn,
        atomic = at_lgl,
        has_names = hn_lgl,
        id = ids,
        null_lgl = null_lgl
      )
    }
  )

}

