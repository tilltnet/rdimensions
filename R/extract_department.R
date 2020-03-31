#' Extract the department affiliation from raw_affiliations
#' @param raw_aff Vector of raw affiliation characters.
#' @export
extract_department_name <-
  function(raw_aff) {
    raw_aff_split <- strsplit(raw_aff, ",")
    res <- map(raw_aff_split, ~.[grepl("Department|Lab", .)])
    trimws(res)
  }

#' Map/ group departments to/by college or other grouping
#'
#' @param authors_df Author dataframe extracted from dimensioans.ai data.
#' @param department_name_list List of department names. This should NOT include phrases like "Deparment of...", just the names of the disciplines.
#' @param deparment_var Name of the variable that contains the department affiliation.
#' @export
group_departments <- function(authors_df, department_name_list, department_var = "department") {
  clas_department_names <-
    mutate(department_name_list, X2 = paste("Department of", X1),
         X3 = paste(X1, "Department"))

department_names_grep_vec <-
  paste(tolower(c(clas_department_names$X2, clas_department_names$X3)), collapse = "|")

  grepl(department_names_grep_vec, tolower(authors_df[department_var]))
}