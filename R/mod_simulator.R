# ============================================================================
# InsightFlow — Scenario Simulator Module
# Interactive sliders for what-if analysis on projected profit.
# ============================================================================

mod_simulator_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      title = "Scenario Parameters",
      width = 320,

      shiny::h6("Adjust parameters to see projected impact:"),

      shiny::sliderInput(
        ns("marketing_change"), "Marketing Budget Change",
        min = -50, max = 100, value = 0, step = 5, post = "%"
      ),
      shiny::sliderInput(
        ns("price_change"), "Price Change",
        min = -30, max = 50, value = 0, step = 1, post = "%"
      ),
      shiny::sliderInput(
        ns("demand_change"), "Demand Change",
        min = -50, max = 100, value = 0, step = 5, post = "%"
      ),
      shiny::sliderInput(
        ns("discount"), "Discount Applied",
        min = 0, max = 50, value = 0, step = 1, post = "%"
      ),
      shiny::sliderInput(
        ns("cost_change"), "Cost Change (COGS)",
        min = -30, max = 50, value = 0, step = 1, post = "%"
      ),

      shiny::hr(),
      shiny::actionButton(
        ns("reset_sliders"),
        label = shiny::span(shiny::icon("rotate-left"), " Reset to Baseline"),
        class = "btn-outline-secondary w-100"
      ),

      shiny::br(), shiny::br(),
      shiny::p(
        class = "text-muted small",
        shiny::icon("info-circle"),
        " Projections use a simplified linear sensitivity model based on historical averages."
      )
    ),

    # KPI Impact Cards
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      shiny::uiOutput(ns("sim_revenue")),
      shiny::uiOutput(ns("sim_profit")),
      shiny::uiOutput(ns("sim_margin")),
      shiny::uiOutput(ns("sim_roi"))
    ),

    # Charts
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Revenue: Baseline vs Projected"),
        bslib::card_body(
          plotly::plotlyOutput(ns("comparison_chart"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Profit Sensitivity"),
        bslib::card_body(
          plotly::plotlyOutput(ns("waterfall_chart"), height = "350px")
        )
      )
    ),

    # Insights
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::tags$i(class = "fa-solid fa-lightbulb", style = "color: var(--if-warning);"),
          "Scenario Analysis"
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("sim_insights"))
      )
    )
  )
}

mod_simulator_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # --- Baseline metrics ---
    baseline <- shiny::reactive({
      app_data$last_refresh
      con <- db_con()
      sales <- read_table(con, "sales")
      expenses <- read_table(con, "expenses")

      if (nrow(sales) == 0) {
        return(list(
          monthly_revenue = 10000,
          monthly_expenses = 7000,
          monthly_cogs = 4000,
          monthly_marketing = 2000,
          avg_price = 50,
          monthly_units = 200
        ))
      }

      # Calculate monthly averages
      n_months <- max(1, as.numeric(
        difftime(max(sales$date, na.rm = TRUE),
                 min(sales$date, na.rm = TRUE), units = "days")
      ) / 30.44)

      total_revenue <- sum(sales$total, na.rm = TRUE)
      total_units <- sum(sales$quantity, na.rm = TRUE)

      total_expenses <- if (nrow(expenses) > 0) sum(expenses$amount, na.rm = TRUE) else 0
      marketing_exp <- if (nrow(expenses) > 0) {
        sum(expenses$amount[tolower(expenses$category) == "marketing"], na.rm = TRUE)
      } else { 0 }

      list(
        monthly_revenue = total_revenue / n_months,
        monthly_expenses = total_expenses / n_months,
        monthly_cogs = total_revenue * 0.4 / n_months,  # estimate
        monthly_marketing = marketing_exp / n_months,
        avg_price = if (total_units > 0) total_revenue / total_units else 50,
        monthly_units = total_units / n_months
      )
    })

    # --- Projected metrics ---
    projected <- shiny::reactive({
      b <- baseline()

      # Price effect: higher price = some demand decrease (elasticity ~0.5)
      price_mult <- 1 + input$price_change / 100
      demand_from_price <- 1 - (input$price_change / 100) * 0.5

      # Demand effect
      demand_mult <- (1 + input$demand_change / 100) * demand_from_price

      # Marketing ROI (diminishing returns)
      marketing_change <- input$marketing_change / 100
      marketing_lift <- if (marketing_change > 0) {
        1 + marketing_change * 0.3  # 30% efficiency
      } else {
        1 + marketing_change * 0.5  # 50% drag when cutting
      }

      # Discount effect
      discount_rate <- input$discount / 100
      effective_price_mult <- price_mult * (1 - discount_rate)

      # New units
      proj_units <- b$monthly_units * demand_mult * marketing_lift
      proj_revenue <- proj_units * b$avg_price * effective_price_mult

      # Costs
      cost_mult <- 1 + input$cost_change / 100
      proj_cogs <- b$monthly_cogs * cost_mult * (proj_units / max(b$monthly_units, 1))
      proj_marketing <- b$monthly_marketing * (1 + marketing_change)
      proj_other_expenses <- b$monthly_expenses - b$monthly_marketing - b$monthly_cogs
      proj_total_expenses <- proj_cogs + proj_marketing + proj_other_expenses

      proj_profit <- proj_revenue - proj_total_expenses
      proj_margin <- if (proj_revenue > 0) (proj_profit / proj_revenue) * 100 else 0
      proj_roi <- if (proj_marketing > 0) {
        ((proj_revenue - b$monthly_revenue) / proj_marketing) * 100
      } else 0

      list(
        revenue = proj_revenue,
        profit = proj_profit,
        margin = proj_margin,
        roi = proj_roi,
        units = proj_units,
        expenses = proj_total_expenses,
        cogs = proj_cogs,
        marketing = proj_marketing
      )
    })

    # --- Reset button ---
    shiny::observeEvent(input$reset_sliders, {
      shiny::updateSliderInput(session, "marketing_change", value = 0)
      shiny::updateSliderInput(session, "price_change", value = 0)
      shiny::updateSliderInput(session, "demand_change", value = 0)
      shiny::updateSliderInput(session, "discount", value = 0)
      shiny::updateSliderInput(session, "cost_change", value = 0)
    })

    # --- KPI Cards ---
    output$sim_revenue <- shiny::renderUI({
      p <- projected()
      b <- baseline()
      delta <- ((p$revenue - b$monthly_revenue) / max(b$monthly_revenue, 1)) * 100
      bslib::value_box(
        title = "Projected Revenue / mo",
        value = format_currency(p$revenue),
        showcase = shiny::icon("dollar-sign"),
        theme = bslib::value_box_theme(bg = "#4f46e5"),
        p(format_delta(delta))
      )
    })

    output$sim_profit <- shiny::renderUI({
      p <- projected()
      bg <- if (p$profit >= 0) "#059669" else "#dc2626"
      bslib::value_box(
        title = "Projected Profit / mo",
        value = format_currency(p$profit),
        showcase = shiny::icon("coins"),
        theme = bslib::value_box_theme(bg = bg)
      )
    })

    output$sim_margin <- shiny::renderUI({
      p <- projected()
      bslib::value_box(
        title = "Projected Margin",
        value = sprintf("%.1f%%", p$margin),
        showcase = shiny::icon("percent"),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    output$sim_roi <- shiny::renderUI({
      p <- projected()
      bslib::value_box(
        title = "Marketing ROI",
        value = sprintf("%.0f%%", p$roi),
        showcase = shiny::icon("chart-pie"),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    # --- Comparison Chart ---
    output$comparison_chart <- plotly::renderPlotly({
      b <- baseline()
      p <- projected()

      df <- dplyr::tibble(
        metric = c("Revenue", "Expenses", "Profit"),
        Baseline = c(b$monthly_revenue, b$monthly_expenses,
                     b$monthly_revenue - b$monthly_expenses),
        Projected = c(p$revenue, p$expenses, p$profit)
      )

      plotly::plot_ly(df, x = ~metric, y = ~Baseline, type = "bar",
                       name = "Baseline",
                       marker = list(color = "#6366f1")) %>%
        plotly::add_trace(y = ~Projected, name = "Projected",
                           marker = list(color = "#06b6d4")) %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "", tickfont = list(color = "#94a3b8")),
          yaxis = list(title = "Amount ($)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"), tickprefix = "$"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          legend = list(font = list(color = "#94a3b8")),
          margin = list(l = 60, r = 20, t = 10, b = 40)
        )
    })

    # --- Waterfall / Sensitivity Chart ---
    output$waterfall_chart <- plotly::renderPlotly({
      b <- baseline()
      p <- projected()
      base_profit <- b$monthly_revenue - b$monthly_expenses

      factors <- dplyr::tibble(
        factor = c("Baseline Profit", "Price Effect", "Demand Effect",
                   "Marketing Effect", "Discount Effect", "Cost Effect",
                   "Projected Profit"),
        value = c(
          base_profit,
          p$revenue * (input$price_change / 100) * 0.8,
          p$revenue * (input$demand_change / 100) * 0.3,
          -b$monthly_marketing * (input$marketing_change / 100) * 0.7,
          -p$revenue * (input$discount / 100),
          -b$monthly_cogs * (input$cost_change / 100),
          p$profit
        ),
        type = c("total", "delta", "delta", "delta", "delta", "delta", "total")
      )

      colors <- sapply(factors$value, function(v) {
        if (abs(v) < 1) "#64748b"
        else if (v > 0) "#10b981"
        else "#ef4444"
      })
      colors[1] <- "#6366f1"
      colors[length(colors)] <- "#06b6d4"

      plotly::plot_ly(
        factors, x = ~factor, y = ~value, type = "bar",
        marker = list(color = colors),
        hovertemplate = "%{x}<br>$%{y:,.0f}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "", tickfont = list(color = "#94a3b8", size = 10),
                       tickangle = -30),
          yaxis = list(title = "Impact ($)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"), tickprefix = "$"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 60, r = 20, t = 10, b = 80)
        )
    })

    # --- Insights ---
    output$sim_insights <- shiny::renderUI({
      b <- baseline()
      p <- projected()
      base_profit <- b$monthly_revenue - b$monthly_expenses

      insights <- list()

      # Revenue comparison
      rev_pct <- ((p$revenue - b$monthly_revenue) / max(b$monthly_revenue, 1)) * 100
      if (abs(rev_pct) > 1) {
        dir <- if (rev_pct > 0) "increase" else "decrease"
        cls <- if (rev_pct > 0) "success" else "danger"
        insights <- c(insights, list(create_insight_block(
          "dollar-sign",
          sprintf("This scenario projects a <strong>%.1f%% %s</strong> in monthly revenue (%s → %s).",
                  abs(rev_pct), dir,
                  format_currency(b$monthly_revenue),
                  format_currency(p$revenue)),
          cls
        )))
      }

      # Profit comparison
      profit_pct <- ((p$profit - base_profit) / max(abs(base_profit), 1)) * 100
      if (abs(profit_pct) > 1) {
        dir <- if (profit_pct > 0) "increase" else "decrease"
        cls <- if (profit_pct > 0) "success" else "danger"
        insights <- c(insights, list(create_insight_block(
          "coins",
          sprintf("Monthly profit would <strong>%s by %.1f%%</strong> under this scenario.",
                  dir, abs(profit_pct)),
          cls
        )))
      }

      # Break-even warning
      if (p$profit < 0 && base_profit >= 0) {
        insights <- c(insights, list(create_insight_block(
          "triangle-exclamation",
          "This scenario results in a <strong>net loss</strong>. The business would be operating below break-even.",
          "danger"
        )))
      }

      # Discount warning
      if (input$discount > 20) {
        insights <- c(insights, list(create_insight_block(
          "tag",
          sprintf("A %d%% discount significantly erodes margins. Consider whether volume gains justify the revenue reduction.",
                  input$discount),
          "warning"
        )))
      }

      if (length(insights) == 0) {
        return(shiny::p(class = "text-muted", "Move the sliders to see projected impacts."))
      }

      shiny::tagList(insights)
    })
  })
}
