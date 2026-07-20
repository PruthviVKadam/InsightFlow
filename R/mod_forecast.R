# ============================================================================
# InsightFlow — Forecast Module
# Interactive forecasting page with model comparison and visualization.
# ============================================================================

mod_forecast_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      title = "Forecast Settings",
      width = 280,
      shiny::selectInput(
        ns("forecast_target"), "Forecast Target",
        choices = c("Revenue" = "revenue",
                    "Units Sold" = "units",
                    "Order Count" = "orders"),
        selected = "revenue"
      ),
      shiny::sliderInput(
        ns("forecast_horizon"), "Forecast Horizon (months)",
        min = 1, max = 12, value = 6, step = 1
      ),
      shiny::sliderInput(
        ns("confidence_level"), "Confidence Level",
        min = 0.80, max = 0.99, value = 0.95, step = 0.01
      ),
      shiny::hr(),
      shiny::actionButton(
        ns("run_forecast"),
        label = shiny::span(shiny::icon("wand-magic-sparkles"), " Run Forecast"),
        class = "btn-primary w-100"
      ),
      shiny::br(), shiny::br(),
      shiny::p(
        class = "text-muted small",
        "InsightFlow compares ARIMA, ETS, and XGBoost models,",
        " automatically selecting the best performer."
      )
    ),

    # Results
    shiny::conditionalPanel(
      condition = "output.has_forecast",
      ns = ns,

      # Best model badge
      shiny::uiOutput(ns("best_model_badge")),

      # Forecast chart
      bslib::card(
        bslib::card_header("Forecast Visualization"),
        bslib::card_body(
          plotly::plotlyOutput(ns("forecast_chart"), height = "400px")
        )
      ),

      # Model comparison
      bslib::layout_columns(
        col_widths = c(5, 7),
        bslib::card(
          bslib::card_header("Model Accuracy Comparison"),
          bslib::card_body(
            DT::DTOutput(ns("accuracy_table"))
          )
        ),
        bslib::card(
          bslib::card_header("Forecast Details"),
          bslib::card_body(
            DT::DTOutput(ns("forecast_table"))
          )
        )
      ),

      # Insight
      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$i(class = "fa-solid fa-lightbulb", style = "color: var(--if-warning);"),
            "Forecast Insights"
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("forecast_insight"))
        )
      )
    ),

    # Placeholder before running
    shiny::conditionalPanel(
      condition = "!output.has_forecast",
      ns = ns,
      shiny::div(
        class = "welcome-hero",
        shiny::h2("Forecasting Engine"),
        shiny::p(
          "Configure your forecast settings in the sidebar and click ",
          shiny::strong("Run Forecast"),
          " to generate predictions using multiple models."
        ),
        shiny::tags$i(class = "fa-solid fa-wand-magic-sparkles fa-3x",
                       style = "color: var(--if-primary); opacity: 0.5;")
      )
    )
  )
}

mod_forecast_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {

    fc_result <- shiny::reactiveVal(NULL)

    # --- Run forecast ---
    shiny::observeEvent(input$run_forecast, {
      sales <- read_table(db_con(), "sales")

      if (nrow(sales) == 0) {
        shiny::showNotification("No sales data loaded. Upload data first.",
                                 type = "warning")
        return()
      }

      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_fading_circles(),
          shiny::h4("Training models...", style = "color: white; margin-top: 1rem;")
        ),
        color = "rgba(0, 0, 0, 0.7)"
      )

      tryCatch({
        result <- run_forecast_pipeline(
          sales     = sales,
          target    = input$forecast_target,
          horizon   = input$forecast_horizon,
          confidence = input$confidence_level
        )
        fc_result(result)
      }, error = function(e) {
        shiny::showNotification(
          paste("Forecast error:", e$message),
          type = "error", duration = 10
        )
      })

      waiter::waiter_hide()
    })

    # --- Has forecast flag ---
    output$has_forecast <- shiny::reactive({
      !is.null(fc_result()) && is.null(fc_result()$error)
    })
    shiny::outputOptions(output, "has_forecast", suspendWhenHidden = FALSE)

    # --- Best Model Badge ---
    output$best_model_badge <- shiny::renderUI({
      req(fc_result())
      res <- fc_result()
      if (!is.null(res$error)) return(NULL)

      best_accuracy <- res$accuracy %>%
        dplyr::filter(Model == res$best_model)

      shiny::div(
        class = "mb-3",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          fill = FALSE,
          bslib::value_box(
            title = "Best Model",
            value = res$best_model,
            showcase = shiny::icon("trophy"),
            theme = bslib::value_box_theme(bg = "#4f46e5")
          ),
          bslib::value_box(
            title = "MAPE",
            value = sprintf("%.1f%%", best_accuracy$MAPE),
            showcase = shiny::icon("bullseye"),
            theme = bslib::value_box_theme(bg = "#0891b2"),
            p("Mean Absolute % Error")
          ),
          bslib::value_box(
            title = "Forecast Horizon",
            value = sprintf("%d months", input$forecast_horizon),
            showcase = shiny::icon("calendar-days"),
            theme = bslib::value_box_theme(bg = "#334155")
          )
        )
      )
    })

    # --- Forecast Chart ---
    output$forecast_chart <- plotly::renderPlotly({
      req(fc_result())
      res <- fc_result()
      if (!is.null(res$error)) return(plotly::plotly_empty())

      df <- res$forecasts

      historical <- df %>% dplyr::filter(type == "Historical")
      forecast_data <- df %>% dplyr::filter(type == "Forecast")

      target_label <- switch(input$forecast_target,
        "revenue" = "Revenue ($)",
        "units" = "Units",
        "orders" = "Orders"
      )

      p <- plotly::plot_ly() %>%
        # Historical line
        plotly::add_trace(
          data = historical,
          x = ~date, y = ~actual,
          type = "scatter", mode = "lines+markers",
          name = "Actual",
          line = list(color = "#6366f1", width = 2.5),
          marker = list(color = "#818cf8", size = 5),
          hovertemplate = "%{x|%B %Y}<br>Actual: %{y:,.0f}<extra></extra>"
        ) %>%
        # Confidence interval
        plotly::add_trace(
          data = forecast_data,
          x = ~date, y = ~upper,
          type = "scatter", mode = "lines",
          name = "Upper Bound",
          line = list(color = "rgba(6, 182, 212, 0.3)", width = 0),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        plotly::add_trace(
          data = forecast_data,
          x = ~date, y = ~lower,
          type = "scatter", mode = "lines",
          name = sprintf("%.0f%% CI", input$confidence_level * 100),
          fill = "tonexty",
          fillcolor = "rgba(6, 182, 212, 0.15)",
          line = list(color = "rgba(6, 182, 212, 0.3)", width = 0),
          hoverinfo = "skip"
        ) %>%
        # Forecast line
        plotly::add_trace(
          data = forecast_data,
          x = ~date, y = ~predicted,
          type = "scatter", mode = "lines+markers",
          name = paste(res$best_model, "Forecast"),
          line = list(color = "#06b6d4", width = 2.5, dash = "dash"),
          marker = list(color = "#22d3ee", size = 7, symbol = "diamond"),
          hovertemplate = "%{x|%B %Y}<br>Forecast: %{y:,.0f}<extra></extra>"
        ) %>%
        plotly::layout(
          xaxis = list(title = "", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8")),
          yaxis = list(title = target_label, gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8")),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 60, r = 20, t = 10, b = 40),
          legend = list(x = 0, y = 1.15, orientation = "h",
                        font = list(color = "#94a3b8")),
          hovermode = "x unified"
        )

      p
    })

    # --- Accuracy Table ---
    output$accuracy_table <- DT::renderDT({
      req(fc_result())
      res <- fc_result()
      if (!is.null(res$error)) return(DT::datatable(data.frame()))

      acc <- res$accuracy %>%
        dplyr::mutate(
          MAPE = round(MAPE, 2),
          RMSE = round(RMSE, 2),
          MAE  = round(MAE, 2)
        )

      DT::datatable(
        acc,
        options = list(dom = "t", pageLength = 10, ordering = TRUE),
        class = "display compact",
        rownames = FALSE,
        selection = "none"
      ) %>%
        DT::formatRound(c("MAPE", "RMSE", "MAE"), 2)
    })

    # --- Forecast Details Table ---
    output$forecast_table <- DT::renderDT({
      req(fc_result())
      res <- fc_result()
      if (!is.null(res$error)) return(DT::datatable(data.frame()))

      fc_only <- res$forecasts %>%
        dplyr::filter(type == "Forecast") %>%
        dplyr::mutate(
          Month = format(date, "%B %Y"),
          Predicted = round(predicted, 0),
          Lower = round(lower, 0),
          Upper = round(upper, 0)
        ) %>%
        dplyr::select(Month, Predicted, Lower, Upper)

      DT::datatable(
        fc_only,
        options = list(dom = "t", pageLength = 12),
        class = "display compact",
        rownames = FALSE
      ) %>%
        DT::formatRound(c("Predicted", "Lower", "Upper"), 0)
    })

    # --- Forecast Insight ---
    output$forecast_insight <- shiny::renderUI({
      req(fc_result())
      res <- fc_result()
      if (!is.null(res$error)) {
        return(create_insight_block("circle-xmark", res$error, "danger"))
      }

      fc_data <- res$forecasts %>% dplyr::filter(type == "Forecast")
      hist_data <- res$forecasts %>% dplyr::filter(type == "Historical")

      last_actual <- tail(hist_data$actual, 1)
      first_forecast <- fc_data$predicted[1]
      last_forecast <- tail(fc_data$predicted, 1)

      target_label <- switch(input$forecast_target,
        "revenue" = "revenue", "units" = "unit sales", "orders" = "order volume"
      )

      # Short-term direction
      short_dir <- if (first_forecast > last_actual) "increase" else "decrease"
      short_pct <- abs((first_forecast - last_actual) / last_actual) * 100

      # Overall trend
      overall_dir <- if (last_forecast > first_forecast) "upward" else "downward"
      overall_pct <- abs((last_forecast - first_forecast) / first_forecast) * 100

      shiny::tagList(
        create_insight_block(
          "chart-line",
          sprintf(
            "The %s model projects %s to <strong>%s</strong> by %.1f%% next month.",
            res$best_model, target_label, short_dir, short_pct
          ),
          if (short_dir == "increase") "success" else "warning"
        ),
        create_insight_block(
          "arrow-trend-up",
          sprintf(
            "Over the %d-month forecast horizon, the overall trend is <strong>%s</strong> (%.1f%% change).",
            input$forecast_horizon, overall_dir, overall_pct
          ),
          if (overall_dir == "upward") "success" else "warning"
        ),
        create_insight_block(
          "bullseye",
          sprintf(
            "Model accuracy: <strong>%.1f%% MAPE</strong>. Lower MAPE = better accuracy. Confidence intervals cover %d%% of expected outcomes.",
            res$accuracy %>% dplyr::filter(Model == res$best_model) %>% dplyr::pull(MAPE),
            round(input$confidence_level * 100)
          ),
          ""
        )
      )
    })
  })
}
