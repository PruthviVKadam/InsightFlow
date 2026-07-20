# ============================================================================
# InsightFlow — Automated Insight Generation
# Produces human-readable text insights from KPI data.
# ============================================================================

#' Generate all insights as a list of HTML blocks
#'
#' @param kpis Named list from generate_kpi_summary()
#' @param sales Sales tibble
#' @param inventory Inventory tibble
#' @return List of HTML tag objects
generate_all_insights <- function(kpis, sales, inventory) {
  insights <- list()

  # Revenue insight
  insights <- c(insights, list(generate_revenue_insight(kpis)))

  # Growth insight
  insights <- c(insights, list(generate_growth_insight(kpis)))

  # Top performer
  if (!is.null(sales) && nrow(sales) > 0) {
    insights <- c(insights, list(generate_top_performer(sales)))
  }

  # Inventory alerts
  if (!is.null(inventory) && nrow(inventory) > 0) {
    inv_insights <- generate_inventory_alerts(inventory, sales)
    insights <- c(insights, inv_insights)
  }

  # Profit insight
  insights <- c(insights, list(generate_profit_insight(kpis)))

  # Filter out NULLs

  Filter(Negate(is.null), insights)
}

#' Revenue comparison insight
generate_revenue_insight <- function(kpis) {
  delta <- kpis$revenue_delta
  if (is.null(delta)) return(NULL)

  pct <- delta$pct
  direction <- if (pct > 0) "increased" else if (pct < 0) "decreased" else "remained flat"
  css_class <- if (pct > 0) "success" else if (pct < 0) "danger" else ""
  icon <- if (pct > 0) "arrow-trend-up" else if (pct < 0) "arrow-trend-down" else "minus"

  create_insight_block(
    icon = icon,
    text = sprintf(
      "Revenue %s by <strong>%.1f%%</strong> compared to the previous month (%s).",
      direction,
      abs(pct),
      format_currency(abs(delta$value))
    ),
    css_class = css_class
  )
}

#' Growth trend insight
generate_growth_insight <- function(kpis) {
  growth <- kpis$monthly_growth
  if (is.null(growth) || growth == 0) return(NULL)

  trend <- if (growth > 5) {
    "strong upward"
  } else if (growth > 0) {
    "slight upward"
  } else if (growth > -5) {
    "slight downward"
  } else {
    "significant downward"
  }

  css_class <- if (growth > 0) "success" else "warning"

  create_insight_block(
    icon = "chart-line",
    text = sprintf(
      "The business is showing a <strong>%s trend</strong> with %.1f%% month-over-month growth.",
      trend, growth
    ),
    css_class = css_class
  )
}

#' Top performing product insight
generate_top_performer <- function(sales) {
  if (is.null(sales) || nrow(sales) == 0) return(NULL)
  if (!"product" %in% colnames(sales)) return(NULL)

  top <- sales %>%
    dplyr::group_by(product) %>%
    dplyr::summarise(revenue = sum(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(revenue)) %>%
    dplyr::slice_head(n = 1)

  if (nrow(top) == 0) return(NULL)

  create_insight_block(
    icon = "trophy",
    text = sprintf(
      "<strong>%s</strong> is the top-performing product with %s in total revenue.",
      top$product, format_currency(top$revenue)
    ),
    css_class = "success"
  )
}

#' Inventory alerts for low stock and stockout predictions
generate_inventory_alerts <- function(inventory, sales = NULL) {
  alerts <- list()

  if (!all(c("stock_quantity", "reorder_level") %in% colnames(inventory))) {
    return(alerts)
  }

  # Low stock items
  low_stock <- inventory %>%
    dplyr::filter(stock_quantity <= reorder_level)

  if (nrow(low_stock) > 0) {
    items <- paste(head(low_stock$product, 3), collapse = ", ")
    more <- if (nrow(low_stock) > 3) sprintf(" and %d more", nrow(low_stock) - 3) else ""

    alerts <- c(alerts, list(create_insight_block(
      icon = "box-open",
      text = sprintf(
        "<strong>%d items</strong> are at or below reorder level: %s%s.",
        nrow(low_stock), items, more
      ),
      css_class = "warning"
    )))
  }

  # Estimate days until stockout for lowest items
  if (!is.null(sales) && nrow(sales) > 0 && "product" %in% colnames(sales)) {
    daily_demand <- sales %>%
      dplyr::group_by(product) %>%
      dplyr::summarise(
        total_sold = sum(quantity, na.rm = TRUE),
        days_span = as.numeric(
          difftime(max(date, na.rm = TRUE), min(date, na.rm = TRUE), units = "days")
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        daily_rate = dplyr::if_else(days_span > 0, total_sold / days_span, 0)
      )

    critical <- inventory %>%
      dplyr::inner_join(daily_demand, by = "product") %>%
      dplyr::mutate(
        days_remaining = dplyr::if_else(
          daily_rate > 0,
          round(stock_quantity / daily_rate, 0),
          Inf
        )
      ) %>%
      dplyr::filter(days_remaining < 30 & days_remaining > 0) %>%
      dplyr::arrange(days_remaining) %>%
      dplyr::slice_head(n = 3)

    for (i in seq_len(nrow(critical))) {
      row <- critical[i, ]
      alerts <- c(alerts, list(create_insight_block(
        icon = "clock",
        text = sprintf(
          "Inventory for <strong>%s</strong> will run out in approximately <strong>%d days</strong> at current demand rate.",
          row$product, row$days_remaining
        ),
        css_class = "danger"
      )))
    }
  }

  alerts
}

#' Profit insight
generate_profit_insight <- function(kpis) {
  margin <- kpis$gross_margin
  if (is.null(margin)) return(NULL)

  health <- if (margin > 40) {
    list(text = "healthy", class = "success")
  } else if (margin > 20) {
    list(text = "moderate", class = "")
  } else if (margin > 0) {
    list(text = "thin", class = "warning")
  } else {
    list(text = "negative — the business is operating at a loss", class = "danger")
  }

  create_insight_block(
    icon = "coins",
    text = sprintf(
      "Gross margin is <strong>%.1f%%</strong> — margins are %s. Gross profit: %s.",
      margin, health$text, format_currency(kpis$gross_profit)
    ),
    css_class = health$class
  )
}

# --- Helper: Create insight HTML block ---
create_insight_block <- function(icon, text, css_class = "") {
  shiny::div(
    class = paste("insight-block", css_class),
    shiny::tags$i(class = paste("fa-solid", paste0("fa-", icon), "insight-icon")),
    shiny::HTML(text)
  )
}

# --- Helper: Format currency ---
format_currency <- function(x) {
  if (is.na(x) || is.infinite(x)) return("$0")
  if (abs(x) >= 1e6) {
    sprintf("$%.1fM", x / 1e6)
  } else if (abs(x) >= 1e3) {
    sprintf("$%s", format(round(x), big.mark = ","))
  } else {
    sprintf("$%.2f", x)
  }
}

# --- Helper: Format percentage with arrow ---
format_delta <- function(pct) {
  if (is.null(pct) || is.na(pct)) return("")
  arrow <- if (pct > 0) "\u25B2" else if (pct < 0) "\u25BC" else "\u25CF"
  cls <- if (pct > 0) "positive" else if (pct < 0) "negative" else ""
  shiny::span(
    class = paste("kpi-delta", cls),
    sprintf("%s %.1f%%", arrow, abs(pct))
  )
}
