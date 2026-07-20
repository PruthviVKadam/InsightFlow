# ============================================================================
# InsightFlow — Inventory Analysis Module
# Stock overview, low-stock alerts, turnover metrics, restock recommendations.
# ============================================================================

mod_inventory_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # KPI row
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      shiny::uiOutput(ns("inv_kpi_total")),
      shiny::uiOutput(ns("inv_kpi_low")),
      shiny::uiOutput(ns("inv_kpi_turnover")),
      shiny::uiOutput(ns("inv_kpi_value"))
    ),

    # Charts
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Stock Levels by Product"),
        bslib::card_body(
          plotly::plotlyOutput(ns("stock_chart"), height = "400px")
        )
      ),
      bslib::card(
        bslib::card_header("Stock Distribution by Category"),
        bslib::card_body(
          plotly::plotlyOutput(ns("stock_by_category"), height = "400px")
        )
      )
    ),

    # Low stock alerts
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::tags$i(class = "fa-solid fa-triangle-exclamation",
                         style = "color: var(--if-warning);"),
          "Low Stock Alerts & Restock Recommendations"
        )
      ),
      bslib::card_body(
        DT::DTOutput(ns("low_stock_table"))
      )
    ),

    # Full inventory table
    bslib::card(
      bslib::card_header("Full Inventory"),
      bslib::card_body(
        DT::DTOutput(ns("inventory_table"))
      )
    )
  )
}

mod_inventory_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {

    inv_data <- shiny::reactive({
      app_data$last_refresh
      read_table(db_con(), "inventory")
    })

    sales_data <- shiny::reactive({
      app_data$last_refresh
      read_table(db_con(), "sales")
    })

    # --- KPIs ---
    output$inv_kpi_total <- shiny::renderUI({
      d <- inv_data()
      bslib::value_box(
        title = "Total Products",
        value = nrow(d),
        showcase = shiny::icon("boxes-stacked"),
        theme = bslib::value_box_theme(bg = "#4f46e5")
      )
    })

    output$inv_kpi_low <- shiny::renderUI({
      d <- inv_data()
      n_low <- calc_low_stock_count(d)
      bg <- if (n_low > 0) "#dc2626" else "#059669"
      bslib::value_box(
        title = "Low Stock Items",
        value = n_low,
        showcase = shiny::icon("triangle-exclamation"),
        theme = bslib::value_box_theme(bg = bg)
      )
    })

    output$inv_kpi_turnover <- shiny::renderUI({
      s <- sales_data()
      d <- inv_data()
      turnover <- calc_inventory_turnover(s, d)
      bslib::value_box(
        title = "Inventory Turnover",
        value = sprintf("%.1fx", turnover),
        showcase = shiny::icon("rotate"),
        theme = bslib::value_box_theme(bg = "#0891b2")
      )
    })

    output$inv_kpi_value <- shiny::renderUI({
      d <- inv_data()
      if (nrow(d) == 0 || !all(c("stock_quantity", "unit_cost") %in% colnames(d))) {
        total_val <- 0
      } else {
        total_val <- sum(d$stock_quantity * d$unit_cost, na.rm = TRUE)
      }
      bslib::value_box(
        title = "Inventory Value",
        value = format_currency(total_val),
        showcase = shiny::icon("warehouse"),
        theme = bslib::value_box_theme(bg = "#334155")
      )
    })

    # --- Stock Level Chart ---
    output$stock_chart <- plotly::renderPlotly({
      d <- inv_data()
      if (nrow(d) == 0) return(plotly::plotly_empty())

      # Show top 20 by stock quantity
      d <- d %>% dplyr::arrange(dplyr::desc(stock_quantity)) %>% dplyr::slice_head(n = 20)
      d <- d %>% dplyr::arrange(stock_quantity)

      colors <- ifelse(d$stock_quantity <= d$reorder_level, "#ef4444", "#6366f1")

      plotly::plot_ly(d, y = ~reorder(product, stock_quantity),
                       x = ~stock_quantity, type = "bar", orientation = "h",
                       marker = list(color = colors),
                       hovertemplate = "%{y}<br>Stock: %{x}<extra></extra>") %>%
        plotly::add_trace(y = ~reorder(product, stock_quantity),
                           x = ~reorder_level, type = "scatter",
                           mode = "markers",
                           marker = list(color = "#f59e0b", size = 8, symbol = "diamond"),
                           name = "Reorder Level",
                           hovertemplate = "Reorder: %{x}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Units", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8")),
          yaxis = list(title = "", tickfont = list(color = "#94a3b8", size = 10)),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 160, r = 20, t = 10, b = 40),
          showlegend = TRUE,
          legend = list(x = 0.7, y = 0.1, font = list(color = "#94a3b8"))
        )
    })

    # --- Stock by Category ---
    output$stock_by_category <- plotly::renderPlotly({
      d <- inv_data()
      if (nrow(d) == 0 || !"category" %in% colnames(d)) return(plotly::plotly_empty())

      cat_summary <- d %>%
        dplyr::group_by(category) %>%
        dplyr::summarise(
          total_stock = sum(stock_quantity, na.rm = TRUE),
          products = dplyr::n(),
          .groups = "drop"
        )

      colors <- c("#6366f1", "#06b6d4", "#10b981", "#f59e0b", "#ef4444",
                  "#8b5cf6", "#ec4899", "#14b8a6", "#f97316", "#64748b")

      plotly::plot_ly(cat_summary, labels = ~category, values = ~total_stock,
                       type = "pie", hole = 0.4,
                       marker = list(colors = colors[seq_len(nrow(cat_summary))]),
                       textinfo = "label+percent",
                       hovertemplate = "%{label}<br>%{value} units<br>%{percent}<extra></extra>") %>%
        plotly::layout(
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 10, r = 10, t = 10, b = 10)
        )
    })

    # --- Low Stock Table ---
    output$low_stock_table <- DT::renderDT({
      d <- inv_data()
      s <- sales_data()
      if (nrow(d) == 0 || !all(c("stock_quantity", "reorder_level") %in% colnames(d))) {
        return(DT::datatable(data.frame(Message = "No inventory data loaded")))
      }

      low <- d %>% dplyr::filter(stock_quantity <= reorder_level)

      if (nrow(low) == 0) {
        return(DT::datatable(data.frame(Message = "All stock levels are healthy!")))
      }

      # Add days-remaining estimate
      if (nrow(s) > 0 && "product" %in% colnames(s)) {
        daily_demand <- s %>%
          dplyr::group_by(product) %>%
          dplyr::summarise(
            total_sold = sum(quantity, na.rm = TRUE),
            days_span = max(1, as.numeric(
              difftime(max(date, na.rm = TRUE), min(date, na.rm = TRUE), units = "days")
            )),
            .groups = "drop"
          ) %>%
          dplyr::mutate(daily_rate = total_sold / days_span)

        low <- low %>%
          dplyr::left_join(daily_demand %>% dplyr::select(product, daily_rate),
                           by = "product") %>%
          dplyr::mutate(
            days_remaining = dplyr::if_else(
              !is.na(daily_rate) & daily_rate > 0,
              round(stock_quantity / daily_rate, 0),
              NA_real_
            ),
            suggested_reorder = dplyr::if_else(
              !is.na(daily_rate),
              ceiling(daily_rate * 30),  # 30-day supply
              reorder_level * 2
            )
          )
      } else {
        low$days_remaining <- NA_real_
        low$suggested_reorder <- low$reorder_level * 2
      }

      display <- low %>%
        dplyr::select(
          Product = product,
          Category = category,
          Stock = stock_quantity,
          `Reorder Level` = reorder_level,
          `Days Remaining` = days_remaining,
          `Suggested Order` = suggested_reorder,
          Warehouse = warehouse
        )

      DT::datatable(
        display,
        options = list(pageLength = 10, dom = "frtip", scrollX = TRUE),
        class = "display compact",
        rownames = FALSE
      ) %>%
        DT::formatRound(c("Days Remaining", "Suggested Order"), 0)
    })

    # --- Full Inventory Table ---
    output$inventory_table <- DT::renderDT({
      d <- inv_data()
      if (nrow(d) == 0) return(DT::datatable(data.frame()))

      display <- d %>%
        dplyr::mutate(
          Status = dplyr::case_when(
            stock_quantity <= reorder_level * 0.5 ~ "Critical",
            stock_quantity <= reorder_level ~ "Low",
            stock_quantity <= reorder_level * 2 ~ "Normal",
            TRUE ~ "Overstocked"
          ),
          `Inventory Value` = round(stock_quantity * unit_cost, 2)
        ) %>%
        dplyr::select(
          ID = product_id,
          Product = product,
          Category = category,
          Stock = stock_quantity,
          `Reorder Lvl` = reorder_level,
          `Unit Cost` = unit_cost,
          Status,
          `Inventory Value`,
          Warehouse = warehouse
        )

      DT::datatable(
        display,
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        class = "display compact",
        rownames = FALSE
      ) %>%
        DT::formatCurrency(c("Unit Cost", "Inventory Value"))
    })
  })
}
