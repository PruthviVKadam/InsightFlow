# ============================================================================
# Tests for forecasting utilities
# ============================================================================
library(testthat)

source(file.path("..", "..", "R", "utils_forecast.R"))

# --- Test fixtures ---
make_forecast_sales <- function() {
  # 24 months of data with trend + seasonality
  dates <- seq(as.Date("2023-01-01"), by = "day", length.out = 730)
  n <- length(dates)

  data.frame(
    order_id = paste0("ORD-", seq_len(n)),
    date = dates,
    product = sample(c("A", "B", "C"), n, replace = TRUE),
    category = sample(c("Cat1", "Cat2"), n, replace = TRUE),
    quantity = rpois(n, 3),
    unit_price = 25,
    total = rpois(n, 3) * 25,
    customer_id = paste0("C", sample(1:100, n, replace = TRUE)),
    region = sample(c("East", "West"), n, replace = TRUE),
    channel = rep("Online", n),
    stringsAsFactors = FALSE
  )
}

test_that("create_lag_features creates correct columns", {
  y <- 1:20
  result <- create_lag_features(y, max_lag = 3)

  expect_true("y" %in% colnames(result))
  expect_true("lag_1" %in% colnames(result))
  expect_true("lag_2" %in% colnames(result))
  expect_true("lag_3" %in% colnames(result))
  expect_true("trend" %in% colnames(result))
  expect_true("month_sin" %in% colnames(result))
  expect_true("month_cos" %in% colnames(result))
  expect_equal(nrow(result), 20)
})

test_that("create_lag_features lag values are correct", {
  y <- c(10, 20, 30, 40, 50)
  result <- create_lag_features(y, max_lag = 2)

  expect_equal(result$lag_1[3], 20)  # lag of 30 is 20
  expect_equal(result$lag_2[3], 10)  # 2-lag of 30 is 10
  expect_true(is.na(result$lag_1[1]))
})

test_that("run_forecast_pipeline returns expected structure", {
  sales <- make_forecast_sales()
  result <- run_forecast_pipeline(sales, target = "revenue", horizon = 3)

  expect_null(result$error)
  expect_true("forecasts" %in% names(result))
  expect_true("accuracy" %in% names(result))
  expect_true("best_model" %in% names(result))

  # Forecast tibble has expected columns
  expect_true("date" %in% colnames(result$forecasts))
  expect_true("actual" %in% colnames(result$forecasts))
  expect_true("predicted" %in% colnames(result$forecasts))
  expect_true("type" %in% colnames(result$forecasts))

  # Accuracy has MAPE, RMSE, MAE
  expect_true("MAPE" %in% colnames(result$accuracy))
  expect_true("RMSE" %in% colnames(result$accuracy))
  expect_true("MAE" %in% colnames(result$accuracy))
})

test_that("run_forecast_pipeline handles insufficient data", {
  # Only 3 months — too short
  sales <- data.frame(
    date = as.Date("2024-01-01") + c(0, 30, 60),
    total = c(100, 200, 150),
    quantity = c(10, 20, 15),
    stringsAsFactors = FALSE
  )

  result <- run_forecast_pipeline(sales, target = "revenue", horizon = 3)
  expect_false(is.null(result$error))
})

test_that("accuracy metrics are numeric and non-negative", {
  sales <- make_forecast_sales()
  result <- run_forecast_pipeline(sales, target = "revenue", horizon = 3)

  if (is.null(result$error)) {
    expect_true(all(result$accuracy$MAPE >= 0))
    expect_true(all(result$accuracy$RMSE >= 0))
    expect_true(all(result$accuracy$MAE >= 0))
    expect_true(all(is.numeric(result$accuracy$MAPE)))
  }
})

test_that("best_model is one of the fitted models", {
  sales <- make_forecast_sales()
  result <- run_forecast_pipeline(sales, target = "revenue", horizon = 3)

  if (is.null(result$error)) {
    expect_true(result$best_model %in% result$accuracy$Model)
  }
})
