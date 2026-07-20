# ============================================================================
# Tests for data cleaning utilities
# ============================================================================
library(testthat)

# Source the utilities
source(file.path("..", "..", "R", "utils_cleaning.R"))

test_that("detect_data_type identifies sales from filename", {
  expect_equal(detect_data_type(c("id", "date", "total"), "Sales.xlsx"), "sales")
  expect_equal(detect_data_type(c("id", "date", "total"), "Monthly_Sales_2024.xlsx"),
               "sales")
})

test_that("detect_data_type identifies inventory from filename", {
  expect_equal(detect_data_type(c("id", "stock"), "Inventory.xlsx"), "inventory")
})

test_that("detect_data_type identifies by columns when filename is ambiguous", {
  expect_equal(
    detect_data_type(c("order_id", "quantity", "unit_price"), "data.xlsx"),
    "sales"
  )
  expect_equal(
    detect_data_type(c("stock_quantity", "reorder_level"), "data.xlsx"),
    "inventory"
  )
  expect_equal(
    detect_data_type(c("amount", "department", "category"), "data.xlsx"),
    "expenses"
  )
  expect_equal(
    detect_data_type(c("customer_id", "email", "name"), "data.xlsx"),
    "customers"
  )
})

test_that("clean_sales_data removes duplicates", {
  df <- data.frame(
    order_id = c("A", "A", "B"),
    date = c("2024-01-01", "2024-01-01", "2024-01-02"),
    product = c("Widget", "Widget", "Gadget"),
    quantity = c(2, 2, 3),
    unit_price = c(10, 10, 20),
    total = c(20, 20, 60),
    stringsAsFactors = FALSE
  )

  result <- clean_sales_data(df)
  expect_true(nrow(result$data) < nrow(df))
  expect_true(any(grepl("duplicate", result$issues, ignore.case = TRUE)))
})

test_that("clean_sales_data recalculates totals", {
  df <- data.frame(
    order_id = "A",
    date = "2024-01-01",
    product = "Widget",
    quantity = 5,
    unit_price = 10,
    total = 999,  # incorrect
    stringsAsFactors = FALSE
  )

  result <- clean_sales_data(df)
  expect_equal(result$data$total[1], 50)
})

test_that("clean_sales_data handles missing dates", {
  df <- data.frame(
    order_id = c("A", "B"),
    date = c("2024-01-01", NA),
    quantity = c(1, 2),
    unit_price = c(10, 20),
    total = c(10, 40),
    stringsAsFactors = FALSE
  )

  result <- clean_sales_data(df)
  expect_equal(nrow(result$data), 1)
  expect_true(any(grepl("unparseable dates", result$issues)))
})

test_that("parse_flexible_date handles multiple formats", {
  expect_equal(parse_flexible_date("2024-01-15"), as.Date("2024-01-15"))
  expect_equal(parse_flexible_date("01/15/2024"), as.Date("2024-01-15"))
  expect_equal(parse_flexible_date("15-01-2024"), as.Date("2024-01-15"))
})

test_that("trim_character_cols removes whitespace", {
  df <- data.frame(name = c("  hello  ", "world  "), stringsAsFactors = FALSE)
  result <- trim_character_cols(df)
  expect_equal(result$name, c("hello", "world"))
})

test_that("clean_customers_data deduplicates by customer_id", {
  df <- data.frame(
    customer_id = c("C001", "C001", "C002"),
    name = c("Alice", "Alice Dup", "Bob"),
    email = c("a@test.com", "a2@test.com", "b@test.com"),
    total_purchases = c(100, 200, 300),
    stringsAsFactors = FALSE
  )

  result <- clean_customers_data(df)
  expect_equal(nrow(result$data), 2)
})

test_that("clean_expenses_data makes negative amounts positive", {
  df <- data.frame(
    date = "2024-01-01",
    category = "marketing",
    amount = -500,
    description = "Ad spend",
    department = "Marketing",
    stringsAsFactors = FALSE
  )

  result <- clean_expenses_data(df)
  expect_equal(result$data$amount[1], 500)
})
