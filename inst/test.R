library(tidyverse)
library(rdimensions)
login <- list(
  username = "",
  password = ""
)

token <- get_token(login)

descr <- get_data_descriptions(token)
descr_fields <- purrr::map(descr, ~names(.$fields))

deprecated_fields <- get_deprecated_fields()

# Publications ------------------------------------------------------------

uf_pubs <- dimensions_request('search publications where research_orgs="grid.15276.37" return publications[all] limit 10')

resa <- extract_data(data_entry = uf_pubs$publications[[3]], data_type = "publications")
resb <- extract_data(data_entry = uf_pubs$publications[[5]], data_type = "publications")

check_extracted_data_fields(data_entry = uf_pubs$publications[[3]],
                            extracted_data = resa,
                            warnings = uf_pubs$`_warnings`)

resc <- map(uf_pubs$publications, extract_data, data_source = "publications")
some_pubs <- bind_extracted_data(result_list = resc)

# Researchers -------------------------------------------------------------

uf_researchers <- dimensions_request('search researchers where research_orgs="grid.15276.37" return researchers[all] limit 10')

res_res <- extract_data(data_entry = uf_researchers$researchers[[6]],
             data_type = "researchers")

check_extracted_data_fields(data_entry = uf_researchers$researchers[[1]],
                            extracted_data = res_res,
                            warnings = uf_researchers$`_warnings`)

resd <- map(uf_researchers$researchers, extract_data, data_source = "researchers")
some_researchers <- bind_extracted_data(result_list = resd)
some_researchers$main_df
# Grants ------------------------------------------------------------------

uf_grants <- dimensions_request('search grants where research_orgs="grid.15276.37" return grants[all]')

res_gra <- extract_data(data_entry = uf_grants$grants[[1]],
                     data_type = "grants")

check_extracted_data_fields(data_entry = uf_grants$grants[[1]],
                            extracted_data = res_gra,
                            warnings = uf_grants$`_warnings`)

rese <- map(uf_grants$grants, extract_data, data_type = "grants")
resf <- bind_extracted_data(result_list = rese)


# Patents -----------------------------------------------------------------
uf_patents <- dimensions_request('search patents return patents[all]')

res_pat <- extract_data(data_entry = uf_patents$patents[[1]],
                     data_type = "patents")

check_extracted_data_fields(data_entry = uf_patents$patents[[1]],
                            extracted_data = res_pat,
                            warnings = uf_patents$`_warnings`)

pete <- map(uf_patents$patents, extract_data, data_type = "patents")
petf <- bind_extracted_data(result_list = rese)


# Clinical Trials-------------------------------------------------------------

uf_clinical_trials <- dimensions_request('search clinical_trials where organizations="grid.15276.37" return clinical_trials[all]')

res_ct <- extract_data(data_entry = uf_clinical_trials$clinical_trials[[1]],
                     data_type = "clinical_trials")

check_extracted_data_fields(data_entry = uf_clinical_trials$clinical_trials[[1]],
                            extracted_data = res_ct,
                            warnings = uf_clinical_trials$`_warnings`)
names(data_entry)
pese <- map(uf_clinical_trials$clinical_trials, extract_data, data_type = "clinical_trials")
pesf <- bind_extracted_data(result_list = rese)


# Policy Documents --------------------------------------------------------

uf_policy_documents <- dimensions_request('search policy_documents return policy_documents[all]')

res_pd <- extract_data(data_entry = uf_policy_documents$policy_documents[[1]],
                     data_type = "policy_documents")

check_extracted_data_fields(data_entry = uf_policy_documents$policy_documents[[1]],
                            extracted_data = res_pd,
                            warnings = uf_policy_documents$`_warnings`)

pese <- map(uf_policy_documents$policy_documents, extract_data, data_type = "clinical_trials")
pesf <- bind_extracted_data(result_list = pese)



# save to db --------------------------------------------------------------

# get number of items
query_text <- 'search publications
               where
               (research_orgs = "grid.15276.37" and
               year >= 2017)
               return publications[all]'

query_text <- 'search grants
               where
               (research_orgs = "grid.15276.37" and
               active_year >= 2018)
               return grants[all]'

query_text <- 'search researchers
               where
               (current_research_org = "grid.15276.37")
               return researchers[all]'
create_pagination_chr(query_text)
library(furrr)
plan(multisession)
dev_null <- future_map(create_pagination_chr(query_text),
                       ~{
                         print(.)
                         ds_res <- dimensions_request(.)
                         # con_test <- dbConnect(RMariaDB::MariaDB(),
                         #                       user = "nwsci",
                         #                       port = 3373,
                         #                       host = "ict-prod-hosting06.mysql.osg.ufl.edu",
                         #                       password = "g18,7sfgBCxMuO829d5-",
                         #                       dbname = "test")
                         future_map(ds_res$publications, ~extract_data(., data_type = "publications")) %>%
                           bind_extracted_data() #%>%
                         # table_list_to_db("publications", con_test)
                         #dbDisconnect(con_test)
                       }) %>%
  bind_extracted_data()

a <- future_map(create_pagination_chr(query_text), dimensions_request)
b <- future_map(a, ~map(.$publications, extract_data, data_source = "publications") %>% bind_extracted_data) %>% bind_extracted_data()

table_list_to_db(b, "pubs", con_test)

b$main_df %>% nrow()
extract_data(data_entry = a[[1]]$publications[[1]], "publications")

map(a, ~.$`_warnings`)
map(a, length)
a[[51]]$errors

plan(sequential)

asd <- dbReadTable(con_test, "publications_main_df")


dbDataType(con_test, "publications_main_df")



test_df <- tibble(aaa = c(1.0,2.0,3.0),
                  bbb = c("asd"),
                  ccc = c(1L, 2L, 3L)
)











saveRDS(uf_pubs1819, "uf_pubs1819.Rds")
saveRDS(uf_grants1819, "uf_grants1819.Rds")
pubs_l <- bind_extracted_data(uf_pubs1819)
grants_l <- bind_extracted_data(uf_grants1819)
names(uf_pubs1819[[1]])
names(uf_grants1819[[1]])

names(grants_l)

saveRDS(uf_researchers1819, "uf_researchers1819_2.Rds")
researchers_l2 <- bind_extracted_data(uf_researchers1819)
descr$researchers$fields$redirect
names(researchers_l)
pubs_l$main_df %>% View()
asd <- dimensions_request(create_iterator(query_text, no_of_pubs)[2])
p <- future_map2(asd$grants,
                 1:length(asd$grants),
                 ~{
                   print(.y)
                   extract_data(.x, data_type = "grants")
                 })

extract_data(data_entry = asd$grants[[81]], data_type = "grants")

q <- bind_extracted_data(p)
map2(q, names(q), ~dbWriteTable(con_test,
                                name = paste0("publications_", .y),
                                .x[0, ],
                                overwrite = TRUE))
deef <- setNames(data.frame(matrix(ncol = 24, nrow = 0)), names(q$main_df))
dbWriteTable(con_test, name = "df12345", q$main_df[1,], overwrite = TRUE)

dbListFields(con_test, )



library(DBI)
# user=nwsci
# port=3373
# host=ict-prod-hosting06.mysql.osg.ufl.edu
# database=nwsci
# password = g18,7sfgBCxMuO829d5-
db_login <- list(user = "nwsci",
                 port = 3373,
                 host = "ict-prod-hosting06.mysql.osg.ufl.edu",
                 password = "g18,7sfgBCxMuO829d5-",
                 dbname = "test")

con <- dbConnect(RMariaDB::MariaDB(),
                 user = "nwsci",
                 port = 3373,
                 host = "ict-prod-hosting06.mysql.osg.ufl.edu",
                 password = "g18,7sfgBCxMuO829d5-",
                 dbname = "nwsci")

dbListTables(con)
dbDisconnect(con)

con_test <- dbConnect(RMariaDB::MariaDB(),
                      user = "nwsci",
                      port = 3373,
                      host = "ict-prod-hosting06.mysql.osg.ufl.edu",
                      password = "g18,7sfgBCxMuO829d5-",
                      dbname = "test")
dbDisconnect(con_test)

con_test2 <- dbConnect(RMariaDB::MariaDB(),
                       db_login)
dbDisconnect(con_test)


# get grid db -------
grid_l <- get_grid_db()
list2env(grid_l, environment())

# test request error messag ------

dimensions_request("describe")
dimensions_request("describ")
dimensions_request("describ", fail_on_error = FALSE)
