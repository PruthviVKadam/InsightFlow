# ============================================================================
# InsightFlow — Settings Module
# Data management, theme toggle, and app info.
# ============================================================================

mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      class = "mb-4",
      shiny::h4("Settings", class = "mb-1"),
      shiny::p(class = "text-muted", "Manage your data and application preferences.")
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),

      # Data Management
      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::icon("database"),
            shiny::h5("Data Management", class = "mb-0")
          )
        ),
        bslib::card_body(
          shiny::h6("Loaded Data Summary"),
          DT::DTOutput(ns("data_summary")),
          shiny::hr(),
          shiny::div(
            class = "d-flex gap-2",
            shiny::actionButton(
              ns("refresh_data"),
              label = shiny::span(shiny::icon("rotate"), " Refresh"),
              class = "btn-outline-info"
            ),
            shiny::actionButton(
              ns("clear_data"),
              label = shiny::span(shiny::icon("trash"), " Clear All Data"),
              class = "btn-outline-danger"
            )
          )
        )
      ),

      # App Info
      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::icon("circle-info"),
            shiny::h5("About InsightFlow", class = "mb-0")
          )
        ),
        bslib::card_body(
          shiny::div(
            shiny::h3("InsightFlow",
                       style = "background: linear-gradient(135deg, #6366f1, #06b6d4);
                                -webkit-background-clip: text;
                                -webkit-text-fill-color: transparent;
                                font-weight: 800;"),
            shiny::p(class = "text-muted",
                     "AI-Powered Business Performance & Forecasting Platform"),
            shiny::hr(),
            shiny::tags$table(
              class = "table table-sm",
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td(shiny::strong("Version")),
                  shiny::tags$td("1.0.0")
                ),
                shiny::tags$tr(
                  shiny::tags$td(shiny::strong("R Version")),
                  shiny::tags$td(paste(R.version$major, R.version$minor, sep = "."))
                ),
                shiny::tags$tr(
                  shiny::tags$td(shiny::strong("Shiny Version")),
                  shiny::tags$td(as.character(packageVersion("shiny")))
                ),
                shiny::tags$tr(
                  shiny::tags$td(shiny::strong("Database")),
                  shiny::tags$td("SQLite")
                ),
                shiny::tags$tr(
                  shiny::tags$td(shiny::strong("Dashboard")),
                  shiny::tags$td("bslib + plotly + DT")
                ),
                shiny::tags$tr(
                  shiny::tags$td(shiny::strong("Forecasting")),
                  shiny::tags$td("ARIMA, ETS, XGBoost")
                ),
                shiny::tags$tr(
                  shiny::tags$td(shiny::strong("ML")),
                  shiny::tags$td("K-Means, Isolation Forest")
                )
              )
            )
          )
        )
      )
    ),

    # Capabilities
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::icon("list-check"),
          shiny::h5("Capabilities", class = "mb-0")
        )
      ),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          shiny::div(
            shiny::h6(shiny::icon("broom"), " Data Cleaning"),
            shiny::tags$ul(class = "small text-muted",
              shiny::tags$li("Auto-detect data types"),
              shiny::tags$li("Remove duplicates"),
              shiny::tags$li("Fix formatting"),
              shiny::tags$li("Handle missing values")
            )
          ),
          shiny::div(
            shiny::h6(shiny::icon("chart-line"), " Analytics"),
            shiny::tags$ul(class = "small text-muted",
              shiny::tags$li("Revenue & Profit KPIs"),
              shiny::tags$li("Growth tracking"),
              shiny::tags$li("Customer segmentation"),
              shiny::tags$li("Anomaly detection")
            )
          ),
          shiny::div(
            shiny::h6(shiny::icon("wand-magic-sparkles"), " Forecasting"),
            shiny::tags$ul(class = "small text-muted",
              shiny::tags$li("ARIMA / ETS"),
              shiny::tags$li("XGBoost ML"),
              shiny::tags$li("Auto model selection"),
              shiny::tags$li("Confidence intervals")
            )
          ),
          shiny::div(
            shiny::h6(shiny::icon("file-export"), " Reporting"),
            shiny::tags$ul(class = "small text-muted",
              shiny::tags$li("Styled Excel reports"),
              shiny::tags$li("PDF executive reports"),
              shiny::tags$li("Scenario simulations"),
              shiny::tags$li("Automated insights")
            )
          )
        )
      )
    )
  )
}

mod_settings_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # --- Data Summary ---
    output$data_summary <- DT::renderDT({
      app_data$last_refresh
      con <- db_con()
      counts <- get_table_counts(con)

      summary_df <- data.frame(
        Table = c("Sales", "Inventory", "Expenses", "Customers"),
        Rows = as.integer(counts),
        Status = ifelse(counts > 0, "\u2705 Loaded", "\u274C Empty"),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        summary_df,
        options = list(dom = "t", pageLength = 10, ordering = FALSE),
        class = "display compact",
        rownames = FALSE,
        selection = "none"
      )
    })

    # --- Refresh ---
    shiny::observeEvent(input$refresh_data, {
      app_data$last_refresh <- Sys.time()
      shiny::showNotification("Data refreshed.", type = "message")
    })

    # --- Clear Data ---
    shiny::observeEvent(input$clear_data, {
      shiny::showModal(shiny::modalDialog(
        title = "Clear All Data?",
        "This will permanently delete all loaded data from the database.",
        shiny::br(),
        "You will need to re-upload your Excel files.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_clear"),
                               "Clear Data", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_clear, {
      clear_database(db_con())
      app_data$data_loaded <- FALSE
      app_data$last_refresh <- Sys.time()
      shiny::removeModal()
      shiny::showNotification("All data cleared.", type = "message")
    })
  })
}
