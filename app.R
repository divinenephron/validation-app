# Load packages ----
library(shiny)
library(DT)
library(flextable)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# Limit file size to 500 MB
options(shiny.maxRequestSize = 500*1024^2)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Reference Interval Validation Tool"),
  fluidRow(
    column(4,
           fileInput("results",
                     label = h3("Upload data"),
                     multiple = TRUE,
                     accept = c("text/plain",
                                "text/csv",
                                "application/vnd.ms-excel",
                                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
           selectInput("result_col",
                       label = "Result column:",
                       choices = character(0)),
           checkboxInput("multiple_tests",
                         label = "Multiple tests in uploaded data",
                         value = FALSE),
           conditionalPanel(
             condition = "input.multiple_tests == true",
             selectInput("test_col",
                         label = "Test ID column:",
                         choices = character(0)
             )
           ),
           checkboxInput("distinct_patients",
                         label = "Include only one result for each patient",
                         value = FALSE),
           conditionalPanel(
             condition = "input.distinct_patients == true",
             selectInput("patient_col",
                         label = "Patient ID column:",
                         choices = character(0)
             ),
             selectInput("datetime_col",
                         label = "Date/time column:",
                         choices = character(0)
             )
           ),
           actionButton("rl_reset",
                        label = "Reset reference limits"),
           br(),
           actionButton("validate",
                        label = "Validate reference limits"),
           br(),
           downloadButton("download_report",
                          label = "Download report"),
           br(),
           downloadButton("download_spreadsheet",
                          label = "Download spreadsheet of results")
    ),
    column(8,
           h3("Reference limits to validate"),
           DTOutput("rl_tab"),
    )
  ),
  hr(),
  uiOutput("validation_table"),
)

# Server logic ----
server <- function(input, output, session) {
  # Store the reference limit data
  data <- reactiveValues(rls = NULL)
  # data <- list(rls = NULL)
  
  # Read uploaded data
  uploaded_data <- reactive({
    if(is.null(input$results)) {
      NULL
    } else {
      read_data(input$results)
    }
  })
  
  # Create the reference limit table
  observeEvent(input$rl_reset, {
    if(is.null(uploaded_data())) {
      data$rls <- NULL
    } else if (input$multiple_tests) {
      test_col <- input$test_col
      test_ids <- unique(uploaded_data()[[test_col]])
      data$rls <- tibble(test_id = test_ids,
                         test_name = test_ids,
                         units = NA_character_,
                         lower_limit = NA_real_,
                         upper_limit = NA_real_,)
    } else {
      data$rls <- tibble(test_name = c("Test"),
                         units = NA_character_,
                         lower_limit = NA_real_,
                         upper_limit = NA_real_)
    }
  })
  
  # Editale  reference limit table
  rl_tab_proxy <- dataTableProxy("rl_tab")
  output$rl_tab <- renderDT({
    # Run when RL reset button pressed
    input$rl_reset
    # Don't run when data$rls is changed, because
    # observeEvent() deals with that in a way that doesn't
    # break pagination
    isolate(data$rls) 
  },
  selection = "none",
  editable = "cell",
  rownames = FALSE)
  observeEvent(input$rl_tab_cell_edit, {
    data$rls <- editData(data$rls,
                         input$rl_tab_cell_edit,
                         proxy = rl_tab_proxy,
                         rownames = FALSE,
                         resetPaging = FALSE)
  })
  
  # Update the column choices when the data is updated
  observe({
    if (!is.null(uploaded_data())) {
      col_names <- colnames(uploaded_data())
    } else {
      col_names <- character(0)
    }
    
    for (col in c("result_col", "test_col", "patient_col", "datetime_col")) {
      updateSelectInput(session, col,
                        choices = col_names)
    }
  })
  
  rls_long <- reactive({
    data$rls %>%
      pivot_longer(c(lower_limit, upper_limit),
                   names_to = "limit_name",
                   names_pattern = "([^_]+)_limit",
                   values_to = "limit")
  })
  
  cleaned_data <- eventReactive(input$validate, {
    # Get the test_id and result columns
    if (input$multiple_tests) {
      d <- uploaded_data() %>%
        select(all_of(input$test_col), all_of(input$result_col)) %>%
        setNames(c("test_id", "result"))
    } else {
      d <- uploaded_data() %>%
        select(all_of(input$result_col)) %>%
        setNames(c("result"))
    }
    # Convert the result to a number
    d <- d %>%
      mutate(result = as.numeric(str_extract(result,
                                             "(?<=^[<>]?)[\\d.]+")))
    # Choose only the earliest result for each patient
    if(input$distinct_patients) {
      d <- uploaded_data() %>%
        select(all_of(input$patient_col), all_of(input$datetime_col)) %>%
        setNames(c("patient_id", "datetime")) %>%
        mutate(datetime =
                 parse_date_time(datetime,
                                 c("YmdHMS", "dmYHMS"))) %>%
        bind_cols(d) %>%
        drop_na(result) %>%
        arrange(datetime)
      if(input$multiple_tests) {
        d <- d %>%
          distinct(test_id, patient_id, .keep_all = TRUE)
      } else {
        d <- d %>%
          distinct(patient_id, .keep_all = TRUE)
      }
    } else {
      d <- d %>%
        drop_na(result)
    }
    d
  })
  
  validation_results <- eventReactive(input$validate, {
    if(input$multiple_tests) {
      d <- cleaned_data() %>%
        left_join(rls_long(),
                  by = "test_id",
                  copy = TRUE)
    } else {
      d <- cleaned_data() %>%
        expand_grid(rls_long())
    }
    d %>%
      mutate(outside_limit = 
               if_else(limit_name == "lower",
                       result < limit,
                       result > limit)) %>%
      group_by(test_name, units, limit_name, limit) %>%
      summarise(
        n = n(),
        outside_limit =
          mean(outside_limit),
        estimated_limit =
          if (cur_group()$limit_name == "lower") {
            quantile(result, probs = 0.025, names = FALSE, type = 6)
          } else {
            quantile(result, probs = 0.975, names = FALSE, type = 6)
          },
        .groups = "drop")
  })
  
  validation_flextable <- reactive({
    validation_results() %>%
      mutate(outside_limit = round(outside_limit * 100, 2)) %>%
      pivot_wider(id_cols = c(test_name, units),
                  names_from = limit_name,
                  values_from = c(n, limit, outside_limit, estimated_limit),
                  names_glue = "{limit_name}_{.value}") %>%
      select(-upper_n) %>%
      rename(n = lower_n) %>%
      flextable() %>%
      set_header_labels(test_name = "Test",
                        units = "Units",
                        n = "n",
                        lower_limit = "Lower",
                        upper_limit = "Upper",
                        lower_outside_limit = "Lower",
                        upper_outside_limit = "Upper",
                        lower_estimated_limit = "Lower",
                        upper_estimated_limit = "Upper") %>%
      add_header_row(values = c("Test", "Units", "n",
                                "Reference limits", "Reference limits",
                                "Population outside of limit (%)",
                                "Population outside of limit (%)",
                                "Estimated reference limits",
                                "Estimated reference limits")) %>%
      merge_v(j = 1, part = "header") %>%
      merge_v(j = 2, part = "header") %>%
      merge_v(j = 3, part = "header") %>%
      merge_h(i = 1, part = "header") %>%
      bg(j = c("lower_outside_limit", "upper_outside_limit"),
         bg = percentage_highlight) %>%
      theme_box()
  })
  
  output$validation_table <- renderUI({
    validation_flextable() %>%
      htmltools_value()
  })
  
  # Download the report
  output$download_report <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      temp_report <- file.path(tempdir(), "report.Rmd")
      temp_template <- file.path(tempdir(), "template.docx")
      file.copy("report.Rmd", temp_report, overwrite = TRUE)
      file.copy("template.docx", temp_template, overwrite = TRUE)
      params <- list(validation_results = validation_results(),
                     validation_flextable = validation_flextable(),
                     distinct_patients = input$distinct_patients)
      rmarkdown::render(temp_report, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # Download the results as a .csv
  output$download_spreadsheet <- downloadHandler(
    filename = "validation_data.csv",
    content = function(file) {
      write_csv(validation_results(), file, na = "")
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
