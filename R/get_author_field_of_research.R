#' Extract most frequent field of research (FoR) an author published with.
#' @export
get_author_field_of_research <- function(category_for, authors){

  pubs_main_reseach_areas <-
    category_for %>%
    separate(name, into = c("code", "research_area"), extra = "merge", remove = FALSE) %>%
    filter(nchar(code) == 2)

  pubs_authors_research_area <-
    full_join(pubs_main_reseach_areas, authors)

  pubs_authors_research_area %>%
    filter(!is.na(researcher_id)) %>%
    count(researcher_id, research_area) %>%
    group_by(researcher_id) %>%
    filter(n == max(n), !is.na(research_area)) %>%
    slice(1)
}

#' Mutate author_field_of_research to contain colors for biggest fields.
#' @export
mutate_authors_field_of_research_to_colors <- function(authors_field_of_research, fields_to_color_count = 9) {

  big_area_thresshold <-
    authors_research_area %>%
    ungroup() %>%
    count(research_area, sort = TRUE) %>%
    pull(n) %>%
    .[fields_to_color_count] + 1

  big_research_areas <-
    authors_research_area %>%
    ungroup() %>%
    count(research_area, sort = TRUE) %>%
    mutate(big_field = ifelse(n > big_area_thresshold, research_area, "other")) %>%
    mutate(big_field = ifelse(is.na(research_area), "other", big_field))

  vcolors <- c("#e41a1c",
               "#377eb8",
               "#4daf4a",
               "#984ea3",
               "#ff7f00",
               "#ffff33",
               "#a65628",
               "#f781bf",
               rep("#999999", sum(big_research_areas$big_field == "other")))

  research_areas_colors <-
    big_research_areas %>%
    mutate(color = vcolors) %>%
    select(-n)

  authors_research_area_colors <-
    full_join(authors_research_area, research_areas_colors) %>%
    mutate(frame.color = case_when(big_field == "other" ~ "grey",
                                   TRUE ~ color))
}

#' Create legend_color_df to use with `legend()`.
#' @export
create_legend_color_df <- function(authors_research_colors, fields_to_color_count = 9) {
  research_areas_colors %>%
    select(big_field, color) %>%
    slice(1:fields_to_color_count)
}