find_researcher <- function(q) {
  query <- dimensions_search("researchers",
                             search_filter = includes(q),
                             paginate = FALSE)

  dimensions_request(query[1]) %>%
    {purrr::map(.$researchers, extract_data, data_source = "researchers")} %>%
    bind_extracted_data()
}

get_researchers_data <-
  function(ur_id, data_source = "publications") {
    dimensions_search(data_source,
                      search_filter = includes(list(researchers = ur_id))) %>%
      dimensions_request() %>%
      {purrr::map(.[[data_source]], extract_data, data_source = data_source)} %>%
      bind_extracted_data()
  }

#' Web Interface for finding researchers' dimensions ID
#' @export
dimensions_id_finder <- function() {
  library(shiny)
  library(DT)
  library(reader)
  library(searcher)

  ui <- fluidPage(
    tags$head(tags$style(
      HTML("
             #selected_ids {
                padding-top: 35px;
                font-size: 18px;
             }
             ")
    )),
    titlePanel("ID finder"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        uiOutput("data_columns"),
        uiOutput("google_button"),
        uiOutput("save_button"),
        uiOutput("researcher_list"),
        verbatimTextOutput("debug")
      ),

      mainPanel(tabsetPanel(
        id = "tabs",
        tabPanel(
          # file upload ------------------
          "file upload",
          uiOutput("token"),
          inputPanel(
            fileInput("file",
                      "Data file",
                      accept = c(".csv", ".xlsx", ".xls")),
            numericInput(
              "skip",
              "number of rows to skip before reading",
              min = 0,
              value = 0,
              width = "100%"
            )
          ),
          inputPanel(
            uiOutput("select_filter_column"),
            uiOutput("select_filter_criteria")
          ),
          uiOutput("select_name_columns"),
          DTOutput("rd_table_out")
        ),
        tabPanel(
          # find researcher's ID ------
          "find researcher's ID",

          fluidRow(
            column(width = 4, textInput("custom_first_name", "Customize First Name")),
            column(width = 4, textInput("custom_last_name", "Customize Last Name")),
                   column(width = 4, selectizeInput("select_org_name", "Select Organization", choices = NULL,
                         options = list(maxOptions=100)))),
          textOutput("search_text"),
          textOutput("selected_ids"),
          DTOutput("finder_ui")
        )
      ))
    )
  )

  server <- function(input, output, session) {
    values <- reactiveValues()

    # ask for API token if none is stored in options -----

    if (is.null(getOption("dimensions_token"))) {
      output$token <- renderUI({
        textInput("token", "API token", width = 200)
      })

      observeEvent(input$token, {
        options(dimensions_token = input$token)
      })
    }

    # grid_ids and organization names for org selection ----
    grid_ids_ <- {
      data(grid_ids)
      uf_gid <- grid_ids[names(grid_ids) == "University of Florida"]
      grid_ids <- grid_ids[grid_ids != uf_gid]
      c(
        "All Organzations" = "",
        uf_gid,
        grid_ids
      )
    }

    updateSelectizeInput(session,
                         inputId = "select_org_name",
                         choices = grid_ids_,
                         server = TRUE)

    output$debug <- reactive({
      names(input)
    })

    # file upload, skip, filter, etc. ------------------------------------

    observeEvent(c(input$file, input$skip, input$ur_id_column), {
      if (!is.null(input$file$datapath)) {
        if(input$file$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
          res <- readxl::read_xlsx(input$file$datapath, skip = input$skip)
        } else {
          res <- reader::reader(input$file$datapath, skip = input$skip)
        }
          if("ur_id" %in% names(res)) {
            new_name <- paste0("ur_id", 1 + sum(grepl("ur_id", names(res))))
            names(res)[names(res) == "ur_id"] <- new_name
            }

        values$researcher_data <- res
      }
    })

    observeEvent(c(input$filter_criteria,
                   input$filter_column,
                   input$ur_id_column), {
      req(values$researcher_data)
      res <- values$researcher_data
      if (!is.null(input$filter_criteria)) {
        if (input$filter_criteria == "!is.na") {
          res <- res %>%
            filter(!is.na(!!sym(input$filter_column)))
        } else if (input$filter_criteria != "no filter") {
          res <- res %>%
            filter(!!sym(input$filter_column) == input$filter_criteria)
        }
      }
      values$enriched_researcher_data <- res
      values$enriched_researcher_data$ur_id <- NA
      if(input$ur_id_column != "") {


        values$enriched_researcher_data$ur_id <-
          purrr::map2(res[[input$ur_id_column]],
               values$enriched_researcher_data$ur_id, function(x, y) c(x, y) %>%
                 unique() %>% na.omit() %>% as.vector())
      }

      values$filtered_researcher_data <- res
    })

    output$select_filter_column <- renderUI({
      req(values$researcher_data)
      selectInput("filter_column",
                  "filter column",
                  choices = names(values$researcher_data))
    })

    output$select_filter_criteria <- renderUI({
      req(values$researcher_data)
      selectInput(
        "filter_criteria",
        "filter criteria",
        choices = c(
          "!is.na",
          "no filter",
          values$researcher_data[[input$filter_column]] %>% unique()
        )
      )
    })

    # select name and data columns -----

    output$select_name_columns <-
      renderUI({
        req(values$researcher_data)
        inputPanel(
          selectInput("orcid_column",
                      "orcid column",
                      choices = c("", names(
                        values$researcher_data
                      ))),
          selectInput(
            "nih_ppid_column",
            "NIH ppid column",
            choices = c("", names(values$researcher_data))
          ),
          selectInput(
            "ur_id_column",
            "Dimensions ID column",
            choices = c("", names(values$researcher_data))
          ),
          selectInput(
            "first_name_column",
            "first name column",
            choices = c("", names(values$researcher_data))
          ),
          selectInput(
            "last_name_column",
            "last name column",
            choices = c("", names(values$researcher_data))
          )
        )
      })

    output$rd_table_out <- renderDT({
      values$enriched_researcher_data
    })

    # sidebar server code ------------------

    output$data_columns <- renderUI({
      req(values$researcher_data)
      choice_names <- names(values$researcher_data)
      choices <- 1:length(choice_names)
      names(choices) <- choice_names

      selectInput(
        "data_columns",
        "select data columns to...",
        choices = choices,
        selected = 1:length(choices),
        multiple = TRUE,
        selectize = FALSE,
        size = 5
      )
    })

    output$google_button <- renderUI({
      req(values$researcher_data)
      if (nchar(input$first_name_column) != 0 &
          nchar(input$last_name_column) != 0) {
        actionButton("search_google",
                     "... search on google",
                     icon = icon("search"))
      }
    })

    observeEvent(input$search_google, {
      res_data <- values$filtered_researcher_data %>%
        slice(as.numeric(input$researcher_list))
      searcher::search_google(paste(res_data[as.numeric(input$data_columns)], collapse = " "),
                              rlang = FALSE)
    })

    output$save_button <- renderUI({
      req(values$researcher_data)
      if (nchar(input$first_name_column) != 0 &
          nchar(input$last_name_column) != 0) {
        downloadButton("save_data", "... save data", icon = icon("save"))
      }
    })

    output$save_data <-
      downloadHandler(
        filename = function() {
          "ur_ir_data.csv"
        },
        content = function(file) {
          values$enriched_researcher_data %>%
            mutate(ur_id = purrr::map_chr(ur_id, paste, collapse = ", ")) %>%
            write_csv(file)
        }
      )

    output$researcher_list <-
      renderUI({
        req(values$researcher_data)
        if (nchar(input$first_name_column) != 0 &
            nchar(input$last_name_column) != 0) {
          choice_names <-
            mutate(values$filtered_researcher_data,
                   name = paste(
                     !!sym(input$first_name_column),!!sym(input$last_name_column)
                   )) %>%
            pull(name)
          choices <- 1:length(choice_names)
          names(choices) <- choice_names

          selectInput(
            "researcher_list",
            label = "select researcher",
            choices = choices,
            selectize = FALSE,
            size = 40
          )
        }
      })

    observeEvent(input$researcher_list, {
      updateTabsetPanel(session = session,
                        inputId = "tabs",
                        selected = "find researcher's ID")
      purrr::map(
        c("custom_first_name", "custom_last_name"),
        function(x) updateTextInput(session, inputId = x, value = "")
      )
    })

    # finder researcher's IDs server code -------

    dimensions_main_df <-
      reactive({
        res_data <- values$filtered_researcher_data %>%
          slice(as.numeric(input$researcher_list))

        first_name <-
          ifelse(
            nchar(input$custom_first_name) != 0 &
              input$custom_first_name != res_data[[input$first_name_column]],
            input$custom_first_name,
            res_data[[input$first_name_column]]
          )

        last_name <-
          ifelse(
            nchar(input$custom_last_name) != 0 &
              input$custom_last_name != res_data[[input$last_name_column]],
            input$custom_last_name,
            res_data[[input$last_name_column]]
          )

        purrr::map2(
          c(res_data[[input$first_name_column]], res_data[[input$last_name_column]]),
          c("custom_first_name", "custom_last_name"),
          function(x, y) updateTextInput(session, inputId = y, placeholder = x)
        )

        output$search_text <-
          renderText({
            out <- paste("Searching for:",
                  first_name,
                  last_name)
            if(nchar(input$select_org_name) > 0)
              out <- paste0(out, "; at ", names(grid_ids)[grid_ids == input$select_org_name], ".")
            out

          })

        q <- list(first_name = first_name,
                  last_name = last_name)

        if (nchar(input$select_org_name) > 0)
          q <- c(q, list(research_orgs = input$select_org_name))

        qres <-
          find_researcher(q)

        main_df <-
          qres$main_df %>%
          as_tibble()

        isolate(ur_ids_ <- ur_ids())


        if(length(ur_ids_) > 0) {
          if (!is.na(ur_ids_)) {

            qres2 <- find_researcher(list(id = ur_ids_))

            main_df2 <-
              qres2$main_df %>%
              as_tibble()

            if (!is.null(qres2$main_df)) {

              main_df <-
                bind_rows(main_df, main_df2) %>%
                distinct()
              qres <- bind_extracted_data(list(qres, qres2)) %>%
                purrr::map(distinct)
            }
          }
        }

        if (!is.null(qres$main_df) & !is.null(qres$research_orgs)) {
          main_df <-
            main_df %>%
            filter(obsolete != 1)

          main_df %>%
            left_join(
              qres$research_orgs %>%
                filter(xid %in% main_df$xid) %>%
                group_by(xid) %>%
                group_modify(function(x, ...) tibble(empl_history = paste(
                  "•",
                  paste(
                    x$name,
                    x$acronym,
                    x$city_name,
                    x$country_name,
                    sep = ", ",
                    collapse = "<br> • "
                  )
                )))
            )
        } else if(!is.null(qres$main_df)) {
          main_df %>%
            filter(obsolete != 1)
        } else {
          tibble(id = character(0),
                 dimensions_url = character(0),
                 first_name = character(0),
                 last_name = character(0),
                 empl_history = character(0)
          )
        }
      })

    ur_ids <- reactive({
      values$enriched_researcher_data$ur_id[[as.numeric(input$researcher_list)]]
    })

    output$finder_ui <- renderDT({
      isolate(selected <-
                dimensions_main_df()$id %in% ur_ids() %>% which())

      dimensions_main_df() %>%
        mutate(id = paste0(
          "<a target = '_blank' href=",
          dimensions_url,
          ">",
          id,
          "</a>"
        ),
        name = paste(first_name, last_name, id, sep = "<br>"),
        publications = ifelse(total_publications > 0, paste0(total_publications, " (", first_publication_year, " - ", last_publication_year,")"), "0"),
        grants = ifelse(total_grants > 0, paste0(total_grants, " (", first_grant_year, " - ", last_grant_year,")"), "0")) %>%
        select(
          name,
          contains("empl_history"),
          publications,
          grants
        ) %>%
        datatable(
          escape = FALSE,
          selection = list(mode = 'multiple', selected = selected),
          options = list(bPaginate = FALSE, bLengthChange = FALSE))
    })

    observeEvent(c(input$finder_ui_rows_selected, input$finder_ui_row_last_clicked), {
      values$enriched_researcher_data[as.numeric(input$researcher_list), ]$ur_id <-
        list(dimensions_main_df()[input$finder_ui_rows_selected, ]$id)
    })

    output$selected_ids <-
      renderText({
        sids <-
          ur_ids()
        if (length(sids) != 0 & !all(purrr::map_lgl(sids, is.na))) {
          paste("Selected researcher ID(s) for this researcher:",
                paste(sids, collapse = " "))
        } else {
          "Select row(s) that belong to this researcher."
        }
      })

  }

  # Run the application
  shinyApp(
    ui = ui,
    server = server,
    options = list(launch.browser = TRUE)
  )
}
