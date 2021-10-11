#' Faster version of `rdimensions::extract_data()``
#' @export
extract_data2 <- function(data_entry) {
  main_df <-
    data_entry[map_lgl(data_entry, is.atomic)] %>%
    as.data.frame()

  # Complex -------

  complex_elements <-
    data_entry[!map_lgl(data_entry, is.atomic)]

  ## Complex with Names ----

  complex_with_names <-
    complex_elements[!map_lgl(complex_elements, function(x) is.null(names(x)))]

  complex_with_names <-
  map2(complex_with_names,
       names(complex_with_names),
       function(x, y) if(!is.null(names(x))) {
         x <- as.data.frame(x)
         names(x) <- paste(y, names(x), sep = "_")
         x
       } )

  # unnamed complex elements --------

  unnamed_complex_elements <-
    complex_elements[map_lgl(complex_elements, function(x) is.null(names(x)))]

  # sub-atomic --------

  sub_atomic_elements_lgl <- map_lgl(unnamed_complex_elements,
                                     function(x) all(map_lgl(x, function(y) is.atomic(y))))

  sub_atomic_elements <-
    unnamed_complex_elements[sub_atomic_elements_lgl]

  sub_atomic_elements <-
    imap(sub_atomic_elements,
       function(x, y) {
         tibble(!!y := unlist(x))
       })

  # (sub-)sub-complex -----

  sub_sub_complex_lgl <-
    map_lgl(unnamed_complex_elements, function(x)all( map_lgl(x, function(y) !all(map_lgl(y, is.atomic)))))

  sub_complex_elements <-
    unnamed_complex_elements[!sub_atomic_elements_lgl & !sub_sub_complex_lgl]
  sub_complex_elements <-
  map(sub_complex_elements, function(x) map_dfr(x, as_tibble))


  # author affiliations ----

  # not_author_affiliations_lgl <-
  #   sub_sub_complex %>%
  #   map_lgl(function(x) any(map_lgl(x, function(y) any(map_lgl(y, function(z) is.atomic(z))))))

  # author_affiliations <- sub_sub_complex[!author_affiliations_lgl]
  #
  # author_affiliations1 <-
  #   author_affiliations %>%
  #   flatten() %>% flatten()
  #
  # author_affiliations1 %>%
  #   map_dfr(function(x) x[names(x) != "affiliations"] %>%
  #             map(function(y) y %>% as.character()))
  #
  # author_affiliations1 %>%
  #   map_dfr(function(x) x[names(x) == "affiliations"] %>%
  #             flatten() %>% map(function(y) as.data.frame(y)))





  # author_affiliations <-
  #   author_affiliations %>%
  #   map(function(x) x %>% bind_rows() %>%
  #             mutate(across(is.list, function(x) paste(x, collapse = " "))))
  #
  # subsub ----
  sub_sub_complex <- unnamed_complex_elements[sub_sub_complex_lgl]

  sub_sub_complex <-
    sub_sub_complex %>%
    map(function(x) map_dfr(x, function(y) map_dfr(y, function(z) if(!is.list(z)) as.character(z) else list(extra = list(as.character(z))))))



    c(list(main_df = main_df),
    complex_with_names,
    sub_atomic_elements,
    sub_complex_elements,
    # author_affiliations = list(author_affiliations),
    sub_sub_complex
    )
}

#' To be used with `extract_data2()`
#' @export
bind_extracted_data2 <-
  function(result_list) {
    ids <- map_chr(result_list, function(x) x$main_df$id)
    map2(result_list, ids,
         function(x, y) map(x, function(z) {bind_cols(xid = y, z)})) %>%
      bind_extracted_data()
  }



