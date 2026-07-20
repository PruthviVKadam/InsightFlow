# ============================================================================
# InsightFlow — Dashboard Module
# Main overview page with KPI value boxes, charts, and automated insights.
# ============================================================================

mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    # Welcome / empty state
    shiny::conditionalPanel(
      condition = "!output.has_data",
      ns = ns,
      shiny::div(
        class = "welcome-hero",
        shiny::h2("Welcome to InsightFlow"),
        shiny::p(
          "Upload your business data to unlock powerful analytics, forecasts, and insights.",
          shiny::br(),
          "Go to ", shiny::strong("More → Data Upload"), " to get started,",
          " or use the sample dataset for a demo."
        ),
        shiny::tags$i(class = "fa-solid fa-chart-line fa-3x",
                       style = "color: var(--if-primary); opacity: 0.5;")
      )
    ),

    # Dashboard content
    shiny::conditionalPanel(
      condition = "output.has_data",
      ns = ns,

      # KPI Value Boxes
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,
        shiny::uiOutput(ns("kpi_revenue")),
        shiny::uiOutput(ns("kpi_profit")),
        shiny::uiOutput(ns("kpi_orders")),
        shiny::uiOutput(ns("kpi_customers"))
      ),

      # Second row of KPIs
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,
        shiny::uiOutput(ns("kpi_aov")),
        shiny::uiOutput(ns("kpi_margin")),
        shiny::uiOutput(ns("kpi_growth")),
        shiny::uiOutput(ns("kpi_burn"))
      ),

      # Charts row
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_header("Revenue Trend"),
          bslib::card_body(
            plotly::plotlyOutput(ns("revenue_trend"), height = "350px")
          )
        ),
        bslib::card(
          bslib::card_header("Revenue by Category"),
          bslib::card_body(
            plotly::plotlyOutput(ns("category_chart"), height = "350px")
          )
        )
      ),

      # Second charts row
      bslib::layout_columns(
        col_widths = c(5, 4, 3),
        bslib::card(
          bslib::card_header("Top 10 Products"),
          bslib::card_body(
            plotly::plotlyOutput(ns("top_products"), height = "350px")
          )
        ),
        bslib::card(
          bslib::card_header("Expense Breakdown"),
          bslib::card_body(
            plotly::plotlyOutput(ns("expenses_chart"), height = "350px")
          )
        ),
        bslib::card(
          bslib::card_header(
            shiny::div(
              class = "d-flex align-items-center gap-2",
              shiny::tags$i(class = "fa-solid fa-lightbulb", style = "color: var(--if-warning);"),
              "AI Insights"
            )
          ),
          bslib::card_body(
            style = "max-height: 380px; overflow-y: auto;",
            shiny::uiOutput(ns("insights_panel"))
          )
        )
      ),

      # Recent orders
      bslib::card(
        bslib::card_header("Recent Orders"),
        bslib::card_body(
          DT::DTOutput(ns("recent_orders"))
        )
      )
    )
  )
}

mod_dashboard_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # --- Load data reactively ---
    data <- shiny::reactive({
      app_data$last_refresh  # trigger refresh
      con <- db_con()
      list(
        sales     = read_table(con, "sales"),
        expenses  = read_table(con, "expenses"),
        customers = read_table(con, "customers"),
        inventory = read_table(con, "inventory")
      )
    })

    # --- KPIs ---
    kpis <- shiny::reactive({
      d <- data()
      generate_kpi_summary(d$sales, d$expenses, d$customers, d$inventory)
    })

    # --- Has data flag ---
    output$has_data <- shiny::reactive({
      isTRUE(app_data$data_loaded)
    })
    shiny::outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # --- KPI Value Boxes ---
    output$kpi_revenue <- shiny::renderUI({
      k <- kpis()
      bslib::value_box(
        title = "Total Revenue",
        value = format_currency(k$total_revenue),
        showcase = shiny::icon("dollar-sign"),
        theme = bslib::value_box_theme(bg = "#4f46e5"),
        p(format_delta(k$revenue_delta$pct))
      )
    })

    output$kpi_profit <- shiny::renderUI({
      k <- kpis()
      bg <- if (k$gross_profit >= 0) "#059669" else "#dc2626"
      bslib::value_box(
        title = "Gross Profit",
        value = format_currency(k$gross_profit),
        showcase = shiny::icon("coins"),
        theme = bslib::value_box_theme(bg = bg)
      )
    })

    output$kpi_orders <- shiny::renderUI({
      k <- kpis()
      bslib::value_box(
        title = "Total Orders",
        value = format(k$total_orders, big.mark = ","),
        showcase = shiny::icon("shopping-cart"),
        theme = bslib::value_box_theme(bg = "#0891b2"),
        p(format_delta(k$orders_delta$pct))
      )
    })

    output$kpi_customers <- shiny::renderUI({
      k <- kpis()
      bslib::value_box(
        title = "Customers",
        value = format(k$total_customers, big.mark = ","),
        showcase = shiny::icon("users"),
        theme = bslib::value_box_theme(bg = "#7c3aed")
      )
    })

    output$kpi_aov <- shiny::renderUI({
      k <- kpis()
      bslib::value_box(
        title = "Avg Order Value",
        value = sprintf("$%.2f", k$avg_order_value),
        showcase = shiny::icon("receipt"),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    output$kpi_margin <- shiny::renderUI({
      k <- kpis()
      bslib::value_box(
        title = "Gross Margin",
        value = sprintf("%.1f%%", k$gross_margin),
        showcase = shiny::icon("percent"),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    output$kpi_growth <- shiny::renderUI({
      k <- kpis()
      icon_name <- if (k$monthly_growth >= 0) "arrow-up" else "arrow-down"
      bslib::value_box(
        title = "Monthly Growth",
        value = sprintf("%.1f%%", k$monthly_growth),
        showcase = shiny::icon(icon_name),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    output$kpi_burn <- shiny::renderUI({
      k <- kpis()
      bslib::value_box(
        title = "Burn Rate / mo",
        value = format_currency(k$burn_rate),
        showcase = shiny::icon("fire"),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    # --- Revenue Trend Chart ---
    output$revenue_trend <- plotly::renderPlotly({
      d <- data()
      if (nrow(d$sales) == 0) return(plotly::plotly_empty())

      monthly <- calc_revenue_by_period(d$sales, "month")

      plotly::plot_ly(monthly, x = ~period, y = ~revenue, type = "scatter",
                       mode = "lines+markers",
                       line = list(color = "#6366f1", width = 3),
                       marker = list(color = "#818cf8", size = 6),
                       hovertemplate = "%{x|%B %Y}<br>Revenue: $%{y:,.0f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8")),
          yaxis = list(title = "Revenue ($)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"),
                       tickprefix = "$"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 60, r = 20, t = 10, b = 40),
          hovermode = "x unified"
        )
    })

    # --- Category Chart ---
    output$category_chart <- plotly::renderPlotly({
      d <- data()
      if (nrow(d$sales) == 0) return(plotly::plotly_empty())

      cats <- calc_revenue_by_category(d$sales)

      colors <- c("#6366f1", "#06b6d4", "#10b981", "#f59e0b", "#ef4444",
                  "#8b5cf6", "#ec4899", "#14b8a6", "#f97316", "#64748b")

      plotly::plot_ly(cats, labels = ~category, values = ~revenue,
                       type = "pie",
                       textinfo = "label+percent",
                       textposition = "inside",
                       marker = list(colors = colors[seq_len(nrow(cats))]),
                       hovertemplate = "%{label}<br>$%{value:,.0f}<br>%{percent}<extra></extra>") %>%
        plotly::layout(
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 10, r = 10, t = 10, b = 10)
        )
    })

    # --- Top Products Chart ---
    output$top_products <- plotly::renderPlotly({
      d <- data()
      if (nrow(d$sales) == 0) return(plotly::plotly_empty())

      top <- calc_top_products(d$sales, 10)
      top <- top %>% dplyr::arrange(revenue)  # for horizontal bar

      plotly::plot_ly(top,
                       y = ~reorder(product, revenue),
                       x = ~revenue,
                       type = "bar",
                       orientation = "h",
                       marker = list(
                         color = ~revenue,
                         colorscale = list(c(0, "#06b6d4"), c(1, "#6366f1")),
                         line = list(width = 0)
                       ),
                       hovertemplate = "%{y}<br>$%{x:,.0f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Revenue ($)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"), tickprefix = "$"),
          yaxis = list(title = "", tickfont = list(color = "#94a3b8", size = 11)),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 150, r = 20, t = 10, b = 40)
        )
    })

    # --- Expenses Chart ---
    output$expenses_chart <- plotly::renderPlotly({
      d <- data()
      if (nrow(d$expenses) == 0) return(plotly::plotly_empty())

      exp_cats <- calc_expenses_by_category(d$expenses)

      colors <- c("#ef4444", "#f59e0b", "#10b981", "#3b82f6", "#8b5cf6",
                  "#ec4899", "#06b6d4", "#f97316", "#64748b", "#14b8a6")

      plotly::plot_ly(exp_cats, labels = ~category, values = ~total,
                       type = "pie", hole = 0.5,
                       textinfo = "label+percent",
                       textposition = "outside",
                       marker = list(colors = colors[seq_len(nrow(exp_cats))]),
                       hovertemplate = "%{label}<br>$%{value:,.0f}<extra></extra>") %>%
        plotly::layout(
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9", size = 10),
          margin = list(l = 10, r = 10, t = 10, b = 10)
        )
    })

    # --- AI Insights ---
    output$insights_panel <- shiny::renderUI({
      d <- data()
      k <- kpis()
      insights <- generate_all_insights(k, d$sales, d$inventory)

      if (length(insights) == 0) {
        return(shiny::p(class = "text-muted", "No insights available yet."))
      }

      shiny::tagList(insights)
    })

    # --- Recent Orders Table ---
    output$recent_orders <- DT::renderDT({
      d <- data()
      if (nrow(d$sales) == 0) return(DT::datatable(data.frame()))

      recent <- d$sales %>%
        dplyr::arrange(dplyr::desc(date)) %>%
        dplyr::slice_head(n = 50) %>%
        dplyr::select(
          Order = order_id,
          Date = date,
          Product = product,
          Category = category,
          Qty = quantity,
          Price = unit_price,
          Total = total,
          Region = region,
          Channel = channel
        )

      DT::datatable(
        recent,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "frtip",
          language = list(
            search = "",
            searchPlaceholder = "Search orders..."
          )
        ),
        class = "display compact",
        rownames = FALSE
      ) %>%
        DT::formatCurrency(c("Price", "Total")) %>%
        DT::formatDate("Date", method = "toLocaleDateString")
    })
  })
}
