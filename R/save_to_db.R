create_new_sql_column <-
  function(connection, table, col_name, type) {
    statements <-
      paste("ALTER TABLE ",
            table,
            " ADD COLUMN ",
            col_name, " ",
            type, ";", sep = "")
    print(statements)
    map(statements, ~{
      dbSendQuery(connection, .) %>%
        dbClearResult()
    })
  }



not_all_na <- function(x) any(!is.na(x))

#' Store list of dataframes to database
#' @export
table_list_to_db <- function(table_list, data_source, db_connection) {
  # db_connection <-  dbConnect(RMariaDB::MariaDB(),
  #           user = db_login$user,
  #           port = db_login$port,
  #           host = db_login$host,
  #           password = db_login$password,
  #           dbname = db_login$dbname)

  write_success <- map2(table_list,
       names(table_list),
       ~{

    table_name <- paste(data_source, .y, sep = "_")
    table_df <- select_if(.x, not_all_na)
    tryCatch(expr =  dbWriteTable(db_connection, table_name, table_df, append = TRUE),
             error = function(e) {
               print(e)
               print(str(.x))
               missing_fields <-
                 names(table_df)[!names(table_df) %in% dbListFields(db_connection, table_name)]
               missing_field_types <-
                 map_chr(table_df[missing_fields], ~{
                 case_when(
                   class(.) == "integer" ~ "INT",
                   class(.) == "numeric" ~ "DOUBLE",
                   class(.) == "character" ~ "TEXT(65535)",
                   class(.) == "logical" ~ "BOOL",
                 )
               })
               create_new_sql_column(db_connection ,table_name , missing_fields, missing_field_types)
               dbWriteTable(db_connection, table_name, table_df, append = TRUE)
             }
    )
  })
  if (all(unlist(write_success))) "All tables written." else write_success
}
