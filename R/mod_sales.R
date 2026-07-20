# ============================================================================
# InsightFlow — Sales Analysis Module
# Detailed sales drill-down with filters, trends, and product performance.
# ============================================================================

mod_sales_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      title = "Sales Filters",
      width = 280,
      shiny::dateRangeInput(
        ns("date_range"), "Date Range",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),
      shiny::selectInput(
        ns("category_filter"), "Category",
        choices = c("All" = "all"),
        multiple = TRUE,
        selected = "all"
      ),
      shiny::selectInput(
        ns("region_filter"), "Region",
        choices = c("All" = "all"),
        multiple = TRUE,
        selected = "all"
      ),
      shiny::selectInput(
        ns("channel_filter"), "Channel",
        choices = c("All" = "all"),
        multiple = TRUE,
        selected = "all"
      ),
      shiny::radioButtons(
        ns("time_granularity"), "Time Granularity",
        choices = c("Daily" = "day", "Weekly" = "week",
                    "Monthly" = "month", "Quarterly" = "quarter"),
        selected = "month",
        inline = TRUE
      )
    ),

    # Main content
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      shiny::uiOutput(ns("sales_kpi_revenue")),
      shiny::uiOutput(ns("sales_kpi_orders")),
      shiny::uiOutput(ns("sales_kpi_aov")),
      shiny::uiOutput(ns("sales_kpi_units"))
    ),

    bslib::card(
      bslib::card_header("Sales Trend"),
      bslib::card_body(
        plotly::plotlyOutput(ns("sales_trend"), height = "350px")
      )
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Sales by Channel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("channel_chart"), height = "300px")
        )
      ),
      bslib::card(
        bslib::card_header("Sales by Region"),
        bslib::card_body(
          plotly::plotlyOutput(ns("region_chart"), height = "300px")
        )
      )
    ),

    bslib::card(
      bslib::card_header("Product Performance"),
      bslib::card_body(
        DT::DTOutput(ns("product_table"))
      )
    )
  )
}

mod_sales_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Raw data ---
    raw_sales <- shiny::reactive({
      app_data$last_refresh
      read_table(db_con(), "sales")
    })

    # --- Update filter choices ---
    shiny::observe({
      d <- raw_sales()
      if (nrow(d) == 0) return()

      if ("category" %in% colnames(d)) {
        cats <- sort(unique(d$category))
        shiny::updateSelectInput(session, "category_filter",
                                  choices = c("All" = "all", setNames(cats, cats)),
                                  selected = "all")
      }
      if ("region" %in% colnames(d)) {
        regs <- sort(unique(d$region))
        shiny::updateSelectInput(session, "region_filter",
                                  choices = c("All" = "all", setNames(regs, regs)),
                                  selected = "all")
      }
      if ("channel" %in% colnames(d)) {
        chans <- sort(unique(d$channel))
        shiny::updateSelectInput(session, "channel_filter",
                                  choices = c("All" = "all", setNames(chans, chans)),
                                  selected = "all")
      }
      if ("date" %in% colnames(d)) {
        shiny::updateDateRangeInput(session, "date_range",
                                     start = min(d$date, na.rm = TRUE),
                                     end = max(d$date, na.rm = TRUE))
      }
    })

    # --- Filtered data ---
    filtered <- shiny::reactive({
      d <- raw_sales()
      if (nrow(d) == 0) return(d)

      # Date filter
      if ("date" %in% colnames(d)) {
        d <- d %>% dplyr::filter(
          date >= input$date_range[1],
          date <= input$date_range[2]
        )
      }

      # Category filter
      if (!"all" %in% input$category_filter && "category" %in% colnames(d)) {
        d <- d %>% dplyr::filter(category %in% input$category_filter)
      }

      # Region filter
      if (!"all" %in% input$region_filter && "region" %in% colnames(d)) {
        d <- d %>% dplyr::filter(region %in% input$region_filter)
      }

      # Channel filter
      if (!"all" %in% input$channel_filter && "channel" %in% colnames(d)) {
        d <- d %>% dplyr::filter(channel %in% input$channel_filter)
      }

      d
    })

    # --- KPI boxes ---
    output$sales_kpi_revenue <- shiny::renderUI({
      d <- filtered()
      bslib::value_box(
        title = "Revenue", value = format_currency(sum(d$total, na.rm = TRUE)),
        showcase = shiny::icon("dollar-sign"),
        theme = bslib::value_box_theme(bg = "#4f46e5")
      )
    })

    output$sales_kpi_orders <- shiny::renderUI({
      d <- filtered()
      bslib::value_box(
        title = "Orders", value = format(nrow(d), big.mark = ","),
        showcase = shiny::icon("shopping-cart"),
        theme = bslib::value_box_theme(bg = "#0891b2")
      )
    })

    output$sales_kpi_aov <- shiny::renderUI({
      d <- filtered()
      aov <- if (nrow(d) > 0) sum(d$total, na.rm = TRUE) / nrow(d) else 0
      bslib::value_box(
        title = "Avg Order Value", value = sprintf("$%.2f", aov),
        showcase = shiny::icon("receipt"),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    output$sales_kpi_units <- shiny::renderUI({
      d <- filtered()
      bslib::value_box(
        title = "Units Sold",
        value = format(sum(d$quantity, na.rm = TRUE), big.mark = ","),
        showcase = shiny::icon("box"),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    # --- Sales Trend ---
    output$sales_trend <- plotly::renderPlotly({
      d <- filtered()
      if (nrow(d) == 0) return(plotly::plotly_empty())

      trend <- calc_revenue_by_period(d, input$time_granularity)

      plotly::plot_ly(trend, x = ~period, y = ~revenue,
                       type = "scatter", mode = "lines+markers",
                       fill = "tozeroy",
                       fillcolor = "rgba(99, 102, 241, 0.1)",
                       line = list(color = "#6366f1", width = 2.5),
                       marker = list(color = "#818cf8", size = 5),
                       hovertemplate = "%{x}<br>$%{y:,.0f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8")),
          yaxis = list(title = "Revenue ($)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"), tickprefix = "$"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 60, r = 20, t = 10, b = 40)
        )
    })

    # --- Channel Chart ---
    output$channel_chart <- plotly::renderPlotly({
      d <- filtered()
      if (nrow(d) == 0 || !"channel" %in% colnames(d)) return(plotly::plotly_empty())

      ch <- d %>%
        dplyr::group_by(channel) %>%
        dplyr::summarise(revenue = sum(total, na.rm = TRUE), .groups = "drop")

      colors <- c("#6366f1", "#06b6d4", "#10b981", "#f59e0b")

      plotly::plot_ly(ch, x = ~channel, y = ~revenue, type = "bar",
                       marker = list(color = colors[seq_len(nrow(ch))]),
                       hovertemplate = "%{x}<br>$%{y:,.0f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "", tickfont = list(color = "#94a3b8")),
          yaxis = list(title = "Revenue ($)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"), tickprefix = "$"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 60, r = 20, t = 10, b = 40)
        )
    })

    # --- Region Chart ---
    output$region_chart <- plotly::renderPlotly({
      d <- filtered()
      if (nrow(d) == 0 || !"region" %in% colnames(d)) return(plotly::plotly_empty())

      reg <- calc_revenue_by_region(d)

      plotly::plot_ly(reg, x = ~region, y = ~revenue, type = "bar",
                       marker = list(color = "#06b6d4"),
                       hovertemplate = "%{x}<br>$%{y:,.0f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "", tickfont = list(color = "#94a3b8")),
          yaxis = list(title = "Revenue ($)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"), tickprefix = "$"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 60, r = 20, t = 10, b = 40)
        )
    })

    # --- Product Performance Table ---
    output$product_table <- DT::renderDT({
      d <- filtered()
      if (nrow(d) == 0 || !"product" %in% colnames(d)) {
        return(DT::datatable(data.frame()))
      }

      products <- d %>%
        dplyr::group_by(Product = product, Category = category) %>%
        dplyr::summarise(
          Revenue = sum(total, na.rm = TRUE),
          Orders = dplyr::n(),
          Units = sum(quantity, na.rm = TRUE),
          `Avg Price` = mean(unit_price, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(Revenue))

      DT::datatable(
        products,
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        class = "display compact",
        rownames = FALSE
      ) %>%
        DT::formatCurrency(c("Revenue", "Avg Price")) %>%
        DT::formatRound("Units", 0)
    })
  })
}
