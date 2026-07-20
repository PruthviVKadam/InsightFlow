# ============================================================================
# InsightFlow — Customer Segmentation Module
# RFM analysis, K-Means clustering, and customer segment visualization.
# ============================================================================

mod_customers_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      title = "Segmentation Settings",
      width = 280,
      shiny::sliderInput(
        ns("n_clusters"), "Number of Clusters",
        min = 2, max = 8, value = 4, step = 1
      ),
      shiny::checkboxInput(
        ns("auto_k"), "Auto-detect optimal k", value = TRUE
      ),
      shiny::hr(),
      shiny::actionButton(
        ns("run_segmentation"),
        label = shiny::span(shiny::icon("object-group"), " Run Segmentation"),
        class = "btn-primary w-100"
      )
    ),

    # Results
    shiny::conditionalPanel(
      condition = "output.has_segments",
      ns = ns,

      # Segment summary cards
      shiny::uiOutput(ns("segment_summary_cards")),

      # Charts
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Customer Segments (2D View)"),
          bslib::card_body(
            plotly::plotlyOutput(ns("cluster_scatter"), height = "400px")
          )
        ),
        bslib::card(
          bslib::card_header("Segment Distribution"),
          bslib::card_body(
            plotly::plotlyOutput(ns("segment_pie"), height = "400px")
          )
        )
      ),

      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::card(
          bslib::card_header("Recency Distribution"),
          bslib::card_body(
            plotly::plotlyOutput(ns("rfm_recency"), height = "250px")
          )
        ),
        bslib::card(
          bslib::card_header("Frequency Distribution"),
          bslib::card_body(
            plotly::plotlyOutput(ns("rfm_frequency"), height = "250px")
          )
        ),
        bslib::card(
          bslib::card_header("Monetary Distribution"),
          bslib::card_body(
            plotly::plotlyOutput(ns("rfm_monetary"), height = "250px")
          )
        )
      ),

      # Elbow plot
      bslib::card(
        bslib::card_header("Elbow Plot (Optimal K Selection)"),
        bslib::card_body(
          plotly::plotlyOutput(ns("elbow_plot"), height = "250px")
        )
      ),

      # Customer table
      bslib::card(
        bslib::card_header("Customer Details"),
        bslib::card_body(
          DT::DTOutput(ns("customer_table"))
        )
      )
    ),

    # Placeholder
    shiny::conditionalPanel(
      condition = "!output.has_segments",
      ns = ns,
      shiny::div(
        class = "welcome-hero",
        shiny::h2("Customer Segmentation"),
        shiny::p(
          "Discover your customer segments using RFM analysis and K-Means clustering.",
          shiny::br(),
          "Click ", shiny::strong("Run Segmentation"), " to begin."
        ),
        shiny::tags$i(class = "fa-solid fa-users fa-3x",
                       style = "color: var(--if-primary); opacity: 0.5;")
      )
    )
  )
}

mod_customers_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {

    seg_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_segmentation, {
      sales <- read_table(db_con(), "sales")

      if (nrow(sales) == 0 || !"customer_id" %in% colnames(sales)) {
        shiny::showNotification("Need sales data with customer_id.", type = "warning")
        return()
      }

      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_fading_circles(),
          shiny::h4("Segmenting customers...", style = "color: white; margin-top: 1rem;")
        ),
        color = "rgba(0, 0, 0, 0.7)"
      )

      tryCatch({
        rfm <- calc_rfm(sales)
        k <- if (input$auto_k) NULL else input$n_clusters
        result <- run_kmeans(rfm, k = k)
        seg_result(result)
      }, error = function(e) {
        shiny::showNotification(paste("Segmentation error:", e$message),
                                 type = "error")
      })

      waiter::waiter_hide()
    })

    output$has_segments <- shiny::reactive({
      !is.null(seg_result()) && is.null(seg_result()$error)
    })
    shiny::outputOptions(output, "has_segments", suspendWhenHidden = FALSE)

    # --- Segment Summary Cards ---
    output$segment_summary_cards <- shiny::renderUI({
      req(seg_result())
      res <- seg_result()
      summary_df <- summarize_segments(res$clustered_data)

      colors <- c("#6366f1", "#06b6d4", "#10b981", "#f59e0b",
                  "#ef4444", "#8b5cf6", "#ec4899", "#14b8a6")

      cards <- lapply(seq_len(nrow(summary_df)), function(i) {
        row <- summary_df[i, ]
        bslib::value_box(
          title = row$segment,
          value = row$Customers,
          showcase = shiny::icon("users"),
          theme = bslib::value_box_theme(bg = colors[i]),
          p(sprintf("Avg Revenue: %s", format_currency(row$`Avg Monetary ($)`)))
        )
      })

      bslib::layout_columns(
        col_widths = rep(12 %/% min(nrow(summary_df), 4), min(nrow(summary_df), 4)),
        fill = FALSE,
        !!!cards[1:min(length(cards), 4)]
      )
    })

    # --- 2D Cluster Scatter ---
    output$cluster_scatter <- plotly::renderPlotly({
      req(seg_result())
      d <- seg_result()$clustered_data

      colors <- c("#6366f1", "#06b6d4", "#10b981", "#f59e0b",
                  "#ef4444", "#8b5cf6", "#ec4899", "#14b8a6")

      plotly::plot_ly(d, x = ~recency, y = ~monetary, color = ~segment,
                       colors = colors[1:length(unique(d$segment))],
                       size = ~frequency, sizes = c(5, 25),
                       type = "scatter", mode = "markers",
                       marker = list(opacity = 0.7),
                       hovertemplate = paste(
                         "Customer: %{customdata}<br>",
                         "Recency: %{x} days<br>",
                         "Monetary: $%{y:,.0f}<br>",
                         "<extra>%{fullData.name}</extra>"
                       ),
                       customdata = ~customer_id) %>%
        plotly::layout(
          xaxis = list(title = "Recency (days since last purchase)",
                       gridcolor = "#334155", tickfont = list(color = "#94a3b8")),
          yaxis = list(title = "Total Monetary ($)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"), tickprefix = "$"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          legend = list(font = list(color = "#94a3b8"))
        )
    })

    # --- Segment Pie ---
    output$segment_pie <- plotly::renderPlotly({
      req(seg_result())
      d <- seg_result()$clustered_data

      seg_counts <- d %>%
        dplyr::count(segment) %>%
        dplyr::arrange(dplyr::desc(n))

      colors <- c("#6366f1", "#06b6d4", "#10b981", "#f59e0b",
                  "#ef4444", "#8b5cf6", "#ec4899", "#14b8a6")

      plotly::plot_ly(seg_counts, labels = ~segment, values = ~n,
                       type = "pie", hole = 0.4,
                       marker = list(colors = colors[seq_len(nrow(seg_counts))]),
                       textinfo = "label+percent",
                       hovertemplate = "%{label}<br>%{value} customers<br>%{percent}<extra></extra>") %>%
        plotly::layout(
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9")
        )
    })

    # --- RFM Distribution Charts ---
    rfm_histogram <- function(d, col, title, color) {
      plotly::plot_ly(d, x = as.formula(paste0("~", col)), type = "histogram",
                       marker = list(color = color, line = list(color = "#0f172a", width = 1)),
                       hovertemplate = "%{x}<br>Count: %{y}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = title, gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8")),
          yaxis = list(title = "Count", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8")),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          margin = list(l = 40, r = 10, t = 10, b = 40)
        )
    }

    output$rfm_recency <- plotly::renderPlotly({
      req(seg_result())
      rfm_histogram(seg_result()$clustered_data, "recency", "Days", "#6366f1")
    })

    output$rfm_frequency <- plotly::renderPlotly({
      req(seg_result())
      rfm_histogram(seg_result()$clustered_data, "frequency", "Orders", "#06b6d4")
    })

    output$rfm_monetary <- plotly::renderPlotly({
      req(seg_result())
      rfm_histogram(seg_result()$clustered_data, "monetary", "Revenue ($)", "#10b981")
    })

    # --- Elbow Plot ---
    output$elbow_plot <- plotly::renderPlotly({
      req(seg_result())
      elbow <- seg_result()$elbow_data

      plotly::plot_ly(elbow, x = ~k, y = ~wss, type = "scatter",
                       mode = "lines+markers",
                       line = list(color = "#6366f1", width = 2.5),
                       marker = list(color = "#818cf8", size = 8),
                       hovertemplate = "k = %{x}<br>WSS = %{y:,.0f}<extra></extra>") %>%
        # Highlight selected k
        plotly::add_trace(
          x = seg_result()$k,
          y = elbow$wss[elbow$k == seg_result()$k],
          type = "scatter", mode = "markers",
          marker = list(color = "#ef4444", size = 14, symbol = "star"),
          name = paste("Selected k =", seg_result()$k),
          hovertemplate = "Selected: k = %{x}<extra></extra>"
        ) %>%
        plotly::layout(
          xaxis = list(title = "Number of Clusters (k)", gridcolor = "#334155",
                       tickfont = list(color = "#94a3b8"), dtick = 1),
          yaxis = list(title = "Within-Cluster Sum of Squares",
                       gridcolor = "#334155", tickfont = list(color = "#94a3b8")),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Inter", color = "#f1f5f9"),
          showlegend = TRUE, legend = list(font = list(color = "#94a3b8"))
        )
    })

    # --- Customer Table ---
    output$customer_table <- DT::renderDT({
      req(seg_result())
      d <- seg_result()$clustered_data %>%
        dplyr::select(
          `Customer ID` = customer_id,
          Segment = segment,
          `Recency (days)` = recency,
          Frequency = frequency,
          `Monetary ($)` = monetary,
          `R Score` = R_score,
          `F Score` = F_score,
          `M Score` = M_score
        )

      DT::datatable(
        d,
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        class = "display compact",
        rownames = FALSE,
        filter = "top"
      ) %>%
        DT::formatCurrency("Monetary ($)") %>%
        DT::formatRound("Recency (days)", 0)
    })
  })
}
