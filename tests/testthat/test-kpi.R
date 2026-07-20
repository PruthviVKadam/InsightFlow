# ============================================================================
# Tests for KPI calculation utilities
# ============================================================================
library(testthat)

source(file.path("..", "..", "R", "utils_kpi.R"))

# --- Test fixtures ---
make_sales <- function() {
  data.frame(
    order_id = paste0("ORD-", 1:10),
    date = as.Date("2024-01-01") + (0:9) * 30,
    product = rep(c("A", "B"), 5),
    category = rep(c("Cat1", "Cat2"), 5),
    quantity = c(2, 3, 1, 4, 2, 3, 1, 5, 2, 3),
    unit_price = c(10, 20, 10, 20, 10, 20, 10, 20, 10, 20),
    total = c(20, 60, 10, 80, 20, 60, 10, 100, 20, 60),
    customer_id = paste0("C", 1:10),
    region = rep(c("East", "West"), 5),
    channel = rep("Online", 10),
    stringsAsFactors = FALSE
  )
}

make_expenses <- function() {
  data.frame(
    date = as.Date("2024-01-01") + (0:4) * 60,
    category = c("Marketing", "Salaries", "Rent", "Software", "Operations"),
    amount = c(1000, 5000, 2000, 500, 1500),
    department = rep("Ops", 5),
    stringsAsFactors = FALSE
  )
}

make_customers <- function() {
  data.frame(
    customer_id = paste0("C", 1:10),
    name = paste("Customer", 1:10),
    join_date = as.Date("2023-01-01") + (0:9) * 30,
    last_purchase_date = as.Date("2024-06-01") + (0:9) * 10,
    total_purchases = c(100, 200, 50, 300, 150, 80, 400, 60, 250, 120),
    stringsAsFactors = FALSE
  )
}

make_inventory <- function() {
  data.frame(
    product_id = c("P1", "P2", "P3"),
    product = c("A", "B", "C"),
    category = c("Cat1", "Cat2", "Cat1"),
    stock_quantity = c(100, 5, 50),
    reorder_level = c(20, 30, 10),
    unit_cost = c(5, 10, 8),
    stringsAsFactors = FALSE
  )
}

# --- Tests ---
test_that("calc_revenue returns correct total", {
  sales <- make_sales()
  expect_equal(calc_revenue(sales), sum(sales$total))
})

test_that("calc_revenue handles empty data", {
  expect_equal(calc_revenue(data.frame()), 0)
  expect_equal(calc_revenue(NULL), 0)
})

test_that("calc_total_orders counts distinct order_ids", {
  sales <- make_sales()
  expect_equal(calc_total_orders(sales), 10)
})

test_that("calc_avg_order_value is correct", {
  sales <- make_sales()
  expected <- sum(sales$total) / length(unique(sales$order_id))
  expect_equal(calc_avg_order_value(sales), expected)
})

test_that("calc_gross_margin returns percentage", {
  expect_equal(calc_gross_margin(100, 40), 60)
  expect_equal(calc_gross_margin(100, 100), 0)
  expect_equal(calc_gross_margin(0, 0), 0)
})

test_that("calc_low_stock_count identifies low stock items", {
  inv <- make_inventory()
  # Product B: stock=5, reorder=30 -> low
  expect_equal(calc_low_stock_count(inv), 1)
})

test_that("calc_total_expenses sums amounts", {
  expenses <- make_expenses()
  expect_equal(calc_total_expenses(expenses), sum(expenses$amount))
})

test_that("calc_revenue_by_category groups correctly", {
  sales <- make_sales()
  result <- calc_revenue_by_category(sales)
  expect_true("category" %in% colnames(result))
  expect_true("revenue" %in% colnames(result))
  expect_equal(sum(result$revenue), sum(sales$total))
})

test_that("calc_top_products returns at most n products", {
  sales <- make_sales()
  result <- calc_top_products(sales, 1)
  expect_equal(nrow(result), 1)
})

test_that("calc_revenue_by_period groups by month", {
  sales <- make_sales()
  result <- calc_revenue_by_period(sales, "month")
  expect_true("period" %in% colnames(result))
  expect_true("revenue" %in% colnames(result))
  expect_equal(sum(result$revenue), sum(sales$total))
})

test_that("generate_kpi_summary returns all expected KPIs", {
  sales <- make_sales()
  expenses <- make_expenses()
  customers <- make_customers()
  inventory <- make_inventory()

  kpis <- generate_kpi_summary(sales, expenses, customers, inventory)

  expect_true("total_revenue" %in% names(kpis))
  expect_true("gross_profit" %in% names(kpis))
  expect_true("gross_margin" %in% names(kpis))
  expect_true("total_orders" %in% names(kpis))
  expect_true("avg_order_value" %in% names(kpis))
  expect_true("monthly_growth" %in% names(kpis))
  expect_true("burn_rate" %in% names(kpis))
})
