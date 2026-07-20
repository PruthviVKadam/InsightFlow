# ============================================================================
# InsightFlow — Anomaly Detection Module
# Time series with highlighted anomalies, method selection, and detail table.
# ============================================================================

mod_anomalies_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      title = "Anomaly Settings",
      width = 280,
      shiny::selectInput(
        ns("data_source"), "Data Source",
        choices = c("Sales" = "sales", "Expenses" = "expenses"),
        selected = "sales"
      ),
      shiny::selectInput(
        ns("detection_method"), "Detection Method",
        choices = c("Z-Score" = "zscore",
                    "STL Decomposition" = "stl",
                    "Isolation Forest" = "isolation_forest"),
        selected = "zscore"
      ),
      shiny::sliderInput(
        ns("sensitivity"), "Sensitivity Threshold",
        min = 1.5, max = 5.0, value = 3.0, step = 0.1
      ),
      shiny::hr(),
      shiny::actionButton(
        ns("detect"),
        label = shiny::span(shiny::icon("magnifying-glass"), " Detect Anomalies"),
        class = "btn-primary w-100"
      )
    ),

    # Results
    shiny::conditionalPanel(
      condition = "output.has_anomalies",
      ns = ns,

      # Summary cards
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        fill = FALSE,
        shiny::uiOutput(ns("anomaly_kpi_total")),
        shiny::uiOutput(ns("anomaly_kpi_critical")),
        shiny::uiOutput(ns("anomaly_kpi_method"))
      ),

      # Chart
      bslib::card(
        bslib::card_header("Time Series with Anomalies"),
        bslib::card_body(
          plotly::plotlyOutput(ns("anomaly_chart"), height = "400px")
        )
      ),

      # Table
      bslib::card(
        bslib::card_header("Detected Anomalies"),
        bslib::card_body(
          DT::DTOutput(ns("anomaly_table"))
        )
      )
    ),

    # Placeholder
    shiny::conditionalPanel(
      condition = "!output.has_anomalies",
      ns = ns,
      shiny::div(
        class = "welcome-hero",
        shiny::h2("Anomaly Detection"),
        shiny::p(
          "Detect unusual patterns, spikes, drops, and potential fraud in your data.",
          shiny::br(),
          "Select a data source and detection method, then click ",
          shiny::strong("Detect Anomalies"), "."
        ),
        shiny::tags$i(class = "fa-solid fa-triangle-exclamation fa-3x",
                       style = "color: var(--if-warning); opacity: 0.5;")
      )
    )
  )
}

mod_anomalies_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {

    anomaly_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$detect, {
      con <- db_con()
      src <- input$data_source

      data <- read_table(con, src)
      if (nrow(data) == 0) {
        shiny::showNotification("No data available for selected source.", type = "warning")
        return()
      }

      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_fading_circles(),
          shiny::h4("Scanning for anomalies...", style = "color: white; margin-top: 1rem;")
        ),
        color = "rgba(0, 0, 0, 0.7)"
      )

      tryCatch({
        value_col <- if (src == "sales") "total" else "amount"

        # Aggregate to daily for time series view
        if ("date" %in% colnames(data)) {
          daily <- data %>%
            dplyr::mutate(date = as.Date(date)) %>%
            dplyr::group_by(date) %>%
            dplyr::summarise(
              total = sum(!!rlang::sym(value_col), na.rm = TRUE),
              count = dplyr::n(),
              .groups = "drop"
            ) %>%
            dplyr::arrange(date)

          result <- detect_anomalies(daily, "total",
                                      method = input$detection_method,
                                      threshold = input$sensitivity)
        } else {
          result <- detect_anomalies(data, value_col,
                                      method = input$detection_method,
                                      threshold = input$sensitivity)
        }

        anomaly_result(result)
      }, error = function(e) {
        shiny::showNotification(paste("Detection error:", e$message), type = "error")
      })

      waiter::waiter_hide()
    })

    output$has_anomalies <- shiny::reactive({
      !is.null(anomaly_result())
    })
    shiny::outputOptions(output, "has_anomalies", suspendWhenHidden = FALSE)

    # --- KPI Cards ---
    output$anomaly_kpi_total <- shiny::renderUI({
      req(anomaly_result())
      d <- anomaly_result()
      n_anomalies <- sum(d$is_anomaly, na.rm = TRUE)
      bg <- if (n_anomalies > 0) "#dc2626" else "#059669"
      bslib::value_box(
        title = "Anomalies Found",
        value = n_anomalies,
        showcase = shiny::icon("triangle-exclamation"),
        theme = bslib::value_box_theme(bg = bg)
      )
    })

    output$anomaly_kpi_critical <- shiny::renderUI({
      req(anomaly_result())
      d <- anomaly_result()
      n_crit <- sum(d$severity == "Critical", na.rm = TRUE)
      bslib::value_box(
        title = "Critical",
        value = n_crit,
        showcase = shiny::icon("bolt"),
        theme = bslib::value_box_theme(bg = if (n_crit > 0) "#dc2626" else "#334155")
      )
    })

    output$anomaly_kpi_method <- shiny::renderUI({
      req(anomaly_result())
      method_name <- switch(input$detection_method,
        "zscore" = "Z-Score",
        "stl" = "STL Decomposition",
        "isolation_forest" = "Isolation Forest"
      )
      bslib::value_box(
        title = "Method",
        value = method_name,
        showcase = shiny::icon("microscope"),
        theme = bslib::value_box_theme(bg = "#4f46e5")
      )
    })

    # --- Anomaly Chart ---
    output$anomaly_chart <- plotly::renderPlotly({
      req(anomaly_result())
      d <- anomaly_result()

      if (!"date" %in% colnames(d)) return(plotly::plotly_empty())

      value_col <- if ("total" %in% colnames(d)) "total" else "value"
      normal <- d %>% dplyr::filter(!is_anomaly)
      anomalies <- d %>% dplyr::filter(is_anomaly)

      p <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = d, x = ~date, y = as.formula(paste0("~", value_col)),
          type = "scatter", mode = "lines",
          name = "Normal",
          line = list(color = "#6366f1", width = 2),
          hovertemplate = "%{x}<br>$%{y:,.0f}<extra></extra>"
        )

      if (nrow(anomalies) > 0) {
        colors <- ifelse(anomalies$severity == "Critical", "#ef4444", "#f59e0b")

        p <- p %>%
          plotly::add_trace(
            data = anomalies, x = ~date,
            y = as.formula(paste0("~", value_col)),
            type = "scatter", mode = "markers",
            name = "Anomaly",
            marker = list(color = colors, size = 12, symbol = "x",
                          line = list(color = "white", width = 1)),
            hovertemplate = paste(
              "%{x}<br>Value: $%{y:,.0f}<br>",
              "Type: ", anomalies$anomaly_type,
              "<br>Severity: ", anomalies$severity,
              "<extra></extra>"
            )
          )
      }

      p %>% plotly::layout(
        xaxis = list(title = "", gridcolor = "#334155",
                     tickfont = list(color = "#94a3b8")),
        yaxis = list(title = "Value ($)", gridcolor = "#334155",
                     tickfont = list(color = "#94a3b8"), tickprefix = "$"),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter", color = "#f1f5f9"),
        legend = list(font = list(color = "#94a3b8")),
        margin = list(l = 60, r = 20, t = 10, b = 40)
      )
    })

    # --- Anomaly Table ---
    output$anomaly_table <- DT::renderDT({
      req(anomaly_result())
      d <- anomaly_result() %>% dplyr::filter(is_anomaly)

      if (nrow(d) == 0) {
        return(DT::datatable(
          data.frame(Message = "No anomalies detected. Your data looks clean!"),
          rownames = FALSE
        ))
      }

      value_col <- if ("total" %in% colnames(d)) "total" else "value"

      display <- d %>%
        dplyr::select(
          dplyr::any_of(c("date", value_col, "z_score",
                           "anomaly_type", "severity", "method"))
        ) %>%
        dplyr::arrange(dplyr::desc(abs(z_score)))

      DT::datatable(
        display,
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        class = "display compact",
        rownames = FALSE
      ) %>%
        DT::formatRound("z_score", 2)
    })
  })
}
