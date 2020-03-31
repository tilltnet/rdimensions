extract_single_length_entries_df <-
  function(dimensions_entry) {
      map_lgl(dimensions_entry, ~!is.list(.) & !is.null(.)) %>%
      {names(.)[.]} %>%
      dimensions_entry[.] %>%
      as_tibble()
  }

extract_investigators <- function(entry) {
  a <- map(entry, ~{
    q <- .[names(.) != "affiliations"]
    q[!map_lgl(q, is.null)] %>% as_tibble()
  })
  b <- map(entry, ~{
    q <- .$affiliations[[1]]
    bind_rows(q[!map_lgl(q, is.null)])
    })
  map2_dfr(a, b, ~{
    if(nrow(.y) > 0) {
      names(.y)[names(.y) == "id"] <- "grid_id"
      cbind(.x, c(.y), stringsAsFactors = FALSE)
    } else .x
  })
}

extract_authors <-
  function(entry) {
    a <- map(entry,
               function(x) map_dfc(x, function(y) {
                 ifelse(length(y) == 1, y, tibble(y))
               }) %>% .[!map_lgl(., is.list)])
    b <- map(entry, function(p)
      names(p[map_lgl(p, ~is.list(.) & length(.) > 1)]) %>%
        map(function(x) {
          xnarg <- p[x][[1]][[1]]
          if(is.list(xnarg))
            as_tibble(xnarg[map_lgl(xnarg, ~!is.null(.))])
          else {
            c <- tibble(a = xnarg)
            names(c) <- x
            c
          }
        }) %>% bind_cols())
    map2_dfr(a, b, ~bind_cols(.x, .y))
  }

try_to_get <- function(x) ifelse(is.null(x), NA, x)

extract_researchers <-
  function(entry) {
    a <- map_dfr(entry,
                 ~bind_cols(extract_single_length_entries_df(.),
                   bind_cols(orcid = .$orcid_id[[1]])))
    b <- map_dfr(entry,
                 ~tibble(id = try_to_get(.$id),
                         research_orgs = try_to_get(unlist(.$research_orgs))))
    c <- list(a, b)
    names(c) <- c("researcher", "researcher_research_orgs")
    c
  }

extract_non_null_entries_df <-
  function(entry) entry[!map_lgl(entry, is.null)] %>%
  as.data.frame(stringsAsFactors = FALSE)

#' Extract data from a single dimensions request result list into a dataframe
#' @export
extract_data <-
  function(data_entry, data_source, deprecated_fields = NULL) {
      if (is.null(deprecated_fields)) {
        if("deprecated_fields" %in% ls(.GlobalEnv)) deprecated_fields <- .GlobalEnv$deprecated_fields
        else deprecated_fields <- get_deprecated_fields(data_source)
        remaining_entries <- data_entry[!names(data_entry) %in% deprecated_fields[[data_source]]]
      } else {
        remaining_entries <- data_entry
    }
    res <- list()
    res$main_df <-
      extract_single_length_entries_df(remaining_entries)
    remaining_entries <-
      remaining_entries[!names(remaining_entries) %in% names(res$main_df)] %>%
      rapply(function(x) {
        if(!is.na(x)) {
          if (x == "")
            NA
          else
            x
        }

      }, how = "replace")
    res$authors <- extract_authors(entry = remaining_entries$authors)
    researchers <- extract_researchers(entry = remaining_entries$researchers)
    res$researchers <- researchers[[1]]
    res$researchers_research_orgs  <- researchers[[2]]
    res$current_research_org <- remaining_entries$current_research_org %>%
      as_tibble()
    res$investigator_details <-
      if (data_source == "clinical_trials") {
        map_dfr(remaining_entries$investigator_details, ~ {
          a <- as.data.frame(., stringsAsFactors = FALSE)
          names(a) <-
            c("name", "title", "role", "dept", "org", "grid_id")
          a
        })} else {extract_investigators(entry = remaining_entries$investigator_details)}

    if(!is.null(remaining_entries[["active_year"]]))
      res$active_year <- tibble(years = unlist(remaining_entries$active_year))
    if(!is.null(remaining_entries[["active_years"]]))
      res$active_years <- tibble(years = unlist(remaining_entries$active_years))
    remaining_entries <-
      remaining_entries[!names(remaining_entries) %in% c(names(res$main_df), names(res))]

    easy_extracts_lgl <-
      map_lgl(remaining_entries, ~ {
        map_lgl(., ~is.character(.) & !is.null(.)) %>% all()
      })

    easy_extracts_lgl <-
    tibble(easy_extracts_lgl,
           not_null = !map_lgl(remaining_entries, is.null),
           not_empty = map_lgl(remaining_entries, ~length(.) > 0)) %>%
      mutate(res = ifelse(easy_extracts_lgl & not_null & not_empty, TRUE, FALSE)) %>%
      pull(res)

    easy_extracts <-
      map(remaining_entries[easy_extracts_lgl], ~{tibble(a = unlist(.))})

    remaining_entries <-
      remaining_entries[!names(remaining_entries) %in% c(names(res$main_df), names(easy_extracts))]

    easy_extracts <-
      map2(easy_extracts, names(easy_extracts), function(x, y) {
        names(x) <- y
        x
      })


    res_l <- c(
      res,
      easy_extracts,
      map(remaining_entries, ~ {
        extract_single_length_entries_df(.)
      }),
      map(remaining_entries, ~ {
        map_dfr(., function(y)
          extract_single_length_entries_df(y))
      })

    )
    res_l <- res_l[map_lgl(res_l, ~ nrow(.) > 0)]
    map(res_l, ~ cbind(xid = res$main_df$id, ., stringsAsFactors = FALSE))
  }

#' Bind dataframes with same names from a list that contains lists of dataframes
#'
#' @export
bind_extracted_data <-
  function(result_list) {
    results_concat <- do.call(c, result_list)

   res <-  names(results_concat) %>% unique() %>%
      map(~bind_rows(results_concat[names(results_concat) == .]))
   names(res) <- names(results_concat) %>% unique()
   res
  }

#' Check if extraction of data captured all available fields
#' @export
check_extracted_data_fields <- function(data_entry, extracted_data, warnings) {
  deprecated_fields <- warnings %>%
    map(~strsplit(., " ")) %>%
    map_chr(~.[[1]][[2]]) %>%
    gsub("'","",.)

  available_fields <- names(data_entry)
  available_fields <- available_fields[!available_fields %in% deprecated_fields]
  extracted_fields <- c(names(extracted_data)[-1],
                        names(extracted_data$main_df))

  available_fields[!available_fields %in% extracted_fields]
}


