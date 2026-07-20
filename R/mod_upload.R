# ============================================================================
# InsightFlow — File Upload Module
# Handles Excel file upload, validation, cleaning, and database storage.
# ============================================================================

#' Upload Module UI
#'
#' @param id Module namespace ID
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(12),

    # Upload card
    bslib::card(
      bslib::card_header(
        class = "bg-gradient-primary",
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::icon("cloud-upload-alt", class = "fa-lg"),
          shiny::h5("Upload Business Data", class = "mb-0 text-white")
        )
      ),
      bslib::card_body(
        shiny::p(
          class = "text-muted mb-3",
          "Upload your Excel files (.xlsx) for Sales, Inventory, Expenses, and Customers.",
          shiny::br(),
          "InsightFlow will automatically detect, clean, and validate your data."
        ),

        bslib::layout_columns(
          col_widths = c(8, 4),

          # File input
          shiny::div(
            shiny::fileInput(
              ns("file_upload"),
              label = NULL,
              multiple = TRUE,
              accept = c(".xlsx", ".xls"),
              placeholder = "Choose Excel files...",
              buttonLabel = shiny::span(shiny::icon("file-excel"), " Browse"),
              width = "100%"
            )
          ),

          # Sample data button
          shiny::div(
            class = "d-flex align-items-start",
            shiny::actionButton(
              ns("use_sample"),
              label = shiny::span(shiny::icon("database"), " Use Sample Data"),
              class = "btn-outline-info w-100",
              style = "margin-top: 0px;"
            )
          )
        )
      )
    ),

    # Status card (hidden until upload)
    shiny::conditionalPanel(
      condition = "output.has_results",
      ns = ns,
      bslib::card(
        bslib::card_header(
          class = "bg-gradient-success",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::icon("check-circle", class = "fa-lg"),
            shiny::h5("Data Processing Results", class = "mb-0 text-white")
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("processing_results"))
        )
      )
    )
  )
}

#' Upload Module Server
#'
#' @param id Module namespace ID
#' @param db_con Reactive database connection
#' @param app_data Reactive values to store loaded data status
#' @return Reactive value indicating data was loaded
mod_upload_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track processing results
    results <- shiny::reactiveVal(NULL)

    # --- Upload Excel files ---
    shiny::observeEvent(input$file_upload, {
      req(input$file_upload)

      waiter::waiter_show(
        html = waiter::spin_fading_circles(),
        color = "rgba(0, 0, 0, 0.7)"
      )

      tryCatch({
        upload_results <- list()

        for (i in seq_len(nrow(input$file_upload))) {
          file_info <- input$file_upload[i, ]
          file_path <- file_info$datapath
          file_name <- file_info$name

          # Read Excel file
          df <- readxl::read_excel(file_path, sheet = 1)

          # Auto-detect and clean
          result <- auto_detect_and_clean(as.data.frame(df), file_name)

          # Write to database
          if (result$type != "unknown") {
            con <- db_con()
            n_written <- write_clean_data(con, result$data, result$type)
            result$rows_written <- n_written
          }

          upload_results[[file_name]] <- result
        }

        results(upload_results)
        app_data$data_loaded <- TRUE
        app_data$last_refresh <- Sys.time()

      }, error = function(e) {
        shiny::showNotification(
          paste("Error processing files:", e$message),
          type = "error",
          duration = 10
        )
      })

      waiter::waiter_hide()
    })

    # --- Use sample data ---
    shiny::observeEvent(input$use_sample, {
      waiter::waiter_show(
        html = waiter::spin_fading_circles(),
        color = "rgba(0, 0, 0, 0.7)"
      )

      tryCatch({
        sample_dir <- file.path("data", "sample")
        sample_files <- c("Sales.xlsx", "Inventory.xlsx",
                          "Expenses.xlsx", "Customers.xlsx")
        upload_results <- list()

        for (fname in sample_files) {
          fpath <- file.path(sample_dir, fname)
          if (!file.exists(fpath)) {
            upload_results[[fname]] <- list(
              type = "missing",
              issues = sprintf("Sample file not found: %s. Run scripts/generate_sample_data.R first.", fpath),
              rows_written = 0
            )
            next
          }

          df <- readxl::read_excel(fpath, sheet = 1)
          result <- auto_detect_and_clean(as.data.frame(df), fname)

          if (result$type != "unknown") {
            con <- db_con()
            n_written <- write_clean_data(con, result$data, result$type)
            result$rows_written <- n_written
          }

          upload_results[[fname]] <- result
        }

        results(upload_results)
        app_data$data_loaded <- TRUE
        app_data$last_refresh <- Sys.time()

      }, error = function(e) {
        shiny::showNotification(
          paste("Error loading sample data:", e$message),
          type = "error",
          duration = 10
        )
      })

      waiter::waiter_hide()
    })

    # --- Render processing results ---
    output$processing_results <- shiny::renderUI({
      req(results())
      res <- results()

      result_cards <- lapply(names(res), function(fname) {
        r <- res[[fname]]
        type_badge <- switch(r$type,
          "sales"     = shiny::span(class = "badge bg-primary",   "Sales"),
          "inventory" = shiny::span(class = "badge bg-success",   "Inventory"),
          "expenses"  = shiny::span(class = "badge bg-warning",   "Expenses"),
          "customers" = shiny::span(class = "badge bg-info",      "Customers"),
          shiny::span(class = "badge bg-secondary", "Unknown")
        )

        rows_text <- if (!is.null(r$rows_written) && r$rows_written > 0) {
          shiny::span(class = "badge bg-dark",
                      sprintf("%s rows", format(r$rows_written, big.mark = ",")))
        }

        issues_html <- if (length(r$issues) > 0) {
          shiny::tags$ul(
            class = "list-unstyled mb-0 small text-muted",
            lapply(r$issues, function(issue) {
              icon_cls <- if (grepl("^No issues|clean", issue, ignore.case = TRUE)) {
                "check text-success"
              } else {
                "exclamation-triangle text-warning"
              }
              shiny::tags$li(
                shiny::icon(icon_cls, class = "me-1"),
                issue
              )
            })
          )
        }

        shiny::div(
          class = "mb-3 p-3 border rounded",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-2",
            shiny::div(
              shiny::strong(fname), " ",
              type_badge, " ",
              rows_text
            ),
            shiny::icon("check-circle", class = "text-success fa-lg")
          ),
          issues_html
        )
      })

      shiny::tagList(result_cards)
    })

    # --- Output flag for conditional panel ---
    output$has_results <- shiny::reactive({
      !is.null(results())
    })
    shiny::outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # Return reactive trigger
    shiny::reactive(app_data$data_loaded)
  })
}
