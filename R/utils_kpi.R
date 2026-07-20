# ============================================================================
# InsightFlow — KPI Calculation Utilities
# Functions for computing business key performance indicators.
# ============================================================================

#' Calculate all KPIs from database data
#'
#' @param sales Tibble of sales data
#' @param expenses Tibble of expenses data
#' @param customers Tibble of customers data
#' @param inventory Tibble of inventory data
#' @return Named list of KPI values and metadata
generate_kpi_summary <- function(sales, expenses, customers, inventory) {
  kpis <- list()

  # Revenue
  kpis$total_revenue <- calc_revenue(sales)
  kpis$revenue_by_month <- calc_revenue_by_period(sales, "month")

  # Profit
  kpis$total_expenses <- calc_total_expenses(expenses)
  kpis$gross_profit <- kpis$total_revenue - kpis$total_expenses

  # Margins
  kpis$gross_margin <- calc_gross_margin(kpis$total_revenue, kpis$total_expenses)

  # Orders
  kpis$total_orders <- calc_total_orders(sales)
  kpis$avg_order_value <- calc_avg_order_value(sales)

  # Growth
  kpis$monthly_growth <- calc_monthly_growth(sales)

  # Customer metrics
  kpis$total_customers <- calc_total_customers(customers)
  kpis$avg_clv <- calc_clv(sales, customers)

  # Inventory
  kpis$inventory_turnover <- calc_inventory_turnover(sales, inventory)
  kpis$low_stock_count <- calc_low_stock_count(inventory)

  # Burn rate
  kpis$burn_rate <- calc_burn_rate(expenses)

  # Period comparisons (current month vs previous)
  kpis$revenue_delta <- calc_period_delta(sales, "total", "month")
  kpis$orders_delta <- calc_period_delta_count(sales, "month")

  kpis
}

#' Total revenue
calc_revenue <- function(sales) {
  if (is.null(sales) || nrow(sales) == 0) return(0)
  sum(sales$total, na.rm = TRUE)
}

#' Revenue grouped by time period
calc_revenue_by_period <- function(sales, period = "month") {
  if (is.null(sales) || nrow(sales) == 0) return(dplyr::tibble())

  sales %>%
    dplyr::mutate(
      period = switch(period,
        "day"   = as.Date(date),
        "week"  = lubridate::floor_date(date, "week"),
        "month" = lubridate::floor_date(date, "month"),
        "quarter" = lubridate::floor_date(date, "quarter"),
        "year"  = lubridate::floor_date(date, "year")
      )
    ) %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      revenue = sum(total, na.rm = TRUE),
      orders  = dplyr::n(),
      units   = sum(quantity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(period)
}

#' Total expenses
calc_total_expenses <- function(expenses) {
  if (is.null(expenses) || nrow(expenses) == 0) return(0)
  sum(expenses$amount, na.rm = TRUE)
}

#' Gross margin percentage
calc_gross_margin <- function(revenue, expenses) {
  if (revenue == 0) return(0)
  ((revenue - expenses) / revenue) * 100
}

#' Total number of orders
calc_total_orders <- function(sales) {
  if (is.null(sales) || nrow(sales) == 0) return(0)
  if ("order_id" %in% colnames(sales)) {
    dplyr::n_distinct(sales$order_id)
  } else {
    nrow(sales)
  }
}

#' Average order value
calc_avg_order_value <- function(sales) {
  if (is.null(sales) || nrow(sales) == 0) return(0)
  total_rev <- sum(sales$total, na.rm = TRUE)
  n_orders <- if ("order_id" %in% colnames(sales)) {
    dplyr::n_distinct(sales$order_id)
  } else {
    nrow(sales)
  }
  if (n_orders == 0) return(0)
  total_rev / n_orders
}

#' Month-over-month growth rate (latest month vs previous)
calc_monthly_growth <- function(sales) {
  if (is.null(sales) || nrow(sales) == 0) return(0)

  monthly <- sales %>%
    dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(revenue = sum(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(month)

  if (nrow(monthly) < 2) return(0)

  current <- monthly$revenue[nrow(monthly)]
  previous <- monthly$revenue[nrow(monthly) - 1]

  if (previous == 0) return(0)
  ((current - previous) / previous) * 100
}

#' Total unique customers
calc_total_customers <- function(customers) {
  if (is.null(customers) || nrow(customers) == 0) return(0)
  nrow(customers)
}

#' Average Customer Lifetime Value
#' CLV = Average Purchase Value × Average Purchase Frequency × Average Lifespan
calc_clv <- function(sales, customers) {
  if (is.null(sales) || nrow(sales) == 0) return(0)
  if (is.null(customers) || nrow(customers) == 0) return(0)

  # Average purchase value
  avg_purchase <- mean(sales$total, na.rm = TRUE)

  # Average purchase frequency (orders per customer per year)
  customer_orders <- sales %>%
    dplyr::group_by(customer_id) %>%
    dplyr::summarise(
      n_orders = dplyr::n(),
      first_order = min(date, na.rm = TRUE),
      last_order = max(date, na.rm = TRUE),
      .groups = "drop"
    )

  avg_frequency <- mean(customer_orders$n_orders, na.rm = TRUE)

  # Average customer lifespan in years
  if ("join_date" %in% colnames(customers) &&
      "last_purchase_date" %in% colnames(customers)) {
    lifespans <- as.numeric(
      difftime(customers$last_purchase_date, customers$join_date, units = "days")
    ) / 365.25
    avg_lifespan <- mean(pmax(lifespans, 0.1), na.rm = TRUE)
  } else {
    avg_lifespan <- 1
  }

  round(avg_purchase * avg_frequency * avg_lifespan, 2)
}

#' Inventory turnover ratio
#' Turnover = Total Units Sold / Average Inventory
calc_inventory_turnover <- function(sales, inventory) {
  if (is.null(sales) || nrow(sales) == 0) return(0)
  if (is.null(inventory) || nrow(inventory) == 0) return(0)

  total_sold <- sum(sales$quantity, na.rm = TRUE)
  avg_stock <- mean(inventory$stock_quantity, na.rm = TRUE)

  if (avg_stock == 0) return(0)
  round(total_sold / avg_stock, 2)
}

#' Count of low-stock items
calc_low_stock_count <- function(inventory) {
  if (is.null(inventory) || nrow(inventory) == 0) return(0)
  if (!all(c("stock_quantity", "reorder_level") %in% colnames(inventory))) return(0)
  sum(inventory$stock_quantity <= inventory$reorder_level, na.rm = TRUE)
}

#' Monthly burn rate (average monthly expenses)
calc_burn_rate <- function(expenses) {
  if (is.null(expenses) || nrow(expenses) == 0) return(0)

  monthly <- expenses %>%
    dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")

  mean(monthly$total, na.rm = TRUE)
}

#' Period-over-period delta for a numeric column
calc_period_delta <- function(sales, col = "total", period = "month") {
  if (is.null(sales) || nrow(sales) == 0) return(list(value = 0, pct = 0))

  grouped <- sales %>%
    dplyr::mutate(p = lubridate::floor_date(date, period)) %>%
    dplyr::group_by(p) %>%
    dplyr::summarise(val = sum(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(p)

  if (nrow(grouped) < 2) return(list(value = 0, pct = 0))

  current <- grouped$val[nrow(grouped)]
  previous <- grouped$val[nrow(grouped) - 1]
  change <- current - previous
  pct <- if (previous != 0) (change / previous) * 100 else 0

  list(value = change, pct = round(pct, 1))
}

#' Period-over-period count delta
calc_period_delta_count <- function(sales, period = "month") {
  if (is.null(sales) || nrow(sales) == 0) return(list(value = 0, pct = 0))

  grouped <- sales %>%
    dplyr::mutate(p = lubridate::floor_date(date, period)) %>%
    dplyr::group_by(p) %>%
    dplyr::summarise(val = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(p)

  if (nrow(grouped) < 2) return(list(value = 0, pct = 0))

  current <- grouped$val[nrow(grouped)]
  previous <- grouped$val[nrow(grouped) - 1]
  change <- current - previous
  pct <- if (previous != 0) (change / previous) * 100 else 0

  list(value = change, pct = round(pct, 1))
}

#' Revenue by category
calc_revenue_by_category <- function(sales) {
  if (is.null(sales) || nrow(sales) == 0) return(dplyr::tibble())
  if (!"category" %in% colnames(sales)) return(dplyr::tibble())

  sales %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      revenue = sum(total, na.rm = TRUE),
      orders = dplyr::n(),
      units = sum(quantity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(revenue))
}

#' Top N products by revenue
calc_top_products <- function(sales, n = 10) {
  if (is.null(sales) || nrow(sales) == 0) return(dplyr::tibble())
  if (!"product" %in% colnames(sales)) return(dplyr::tibble())

  sales %>%
    dplyr::group_by(product) %>%
    dplyr::summarise(
      revenue = sum(total, na.rm = TRUE),
      orders = dplyr::n(),
      units = sum(quantity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(revenue)) %>%
    dplyr::slice_head(n = n)
}

#' Expenses by category
calc_expenses_by_category <- function(expenses) {
  if (is.null(expenses) || nrow(expenses) == 0) return(dplyr::tibble())

  expenses %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      total = sum(amount, na.rm = TRUE),
      count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(total))
}

#' Revenue by region
calc_revenue_by_region <- function(sales) {
  if (is.null(sales) || nrow(sales) == 0) return(dplyr::tibble())
  if (!"region" %in% colnames(sales)) return(dplyr::tibble())

  sales %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      revenue = sum(total, na.rm = TRUE),
      orders = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(revenue))
}
