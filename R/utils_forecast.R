# ============================================================================
# InsightFlow — Forecasting Utilities
# Time series forecasting with ARIMA, ETS, and XGBoost.
# Includes model comparison and automatic selection.
# ============================================================================

#' Run the full forecasting pipeline
#'
#' @param sales Sales tibble
#' @param target Column to forecast ("revenue", "units", "orders")
#' @param horizon Number of periods ahead to forecast
#' @param frequency Time series frequency (12 = monthly)
#' @param confidence Confidence level for prediction intervals
#' @return List with: forecasts, accuracy, best_model, comparison
run_forecast_pipeline <- function(sales, target = "revenue", horizon = 6,
                                   frequency = 12, confidence = 0.95) {
  # Aggregate to monthly time series
  monthly <- sales %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::mutate(month = lubridate::floor_date(as.Date(date), "month")) %>%
    dplyr::filter(!is.na(month)) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      revenue = sum(total, na.rm = TRUE),
      units   = sum(quantity, na.rm = TRUE),
      orders  = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(month)

  if (nrow(monthly) < 6) {
    return(list(
      error = "Need at least 6 months of data for forecasting.",
      forecasts = NULL,
      accuracy = NULL
    ))
  }

  # Extract target series
  y_values <- monthly[[target]]
  dates <- monthly$month

  # Create ts object
  start_year <- lubridate::year(min(dates, na.rm = TRUE))
  start_month <- lubridate::month(min(dates, na.rm = TRUE))
  ts_data <- ts(y_values, start = c(start_year, start_month), frequency = frequency)

  # Train/test split (80/20)
  n <- length(ts_data)
  n_test <- max(2, round(n * 0.2))
  n_train <- n - n_test
  train_ts <- ts(y_values[1:n_train],
                  start = c(start_year, start_month),
                  frequency = frequency)
  test_values <- y_values[(n_train + 1):n]

  # --- Fit models ---
  results <- list()

  # 1. ARIMA
  tryCatch({
    arima_fit <- forecast::auto.arima(train_ts, stepwise = TRUE, approximation = TRUE)
    arima_fc <- forecast::forecast(arima_fit, h = n_test, level = confidence * 100)
    results$ARIMA <- list(
      model = arima_fit,
      forecast = arima_fc,
      predicted = as.numeric(arima_fc$mean),
      lower = as.numeric(arima_fc$lower),
      upper = as.numeric(arima_fc$upper),
      actual = test_values
    )
  }, error = function(e) {
    message("ARIMA failed: ", e$message)
  })

  # 2. ETS
  tryCatch({
    ets_fit <- forecast::ets(train_ts)
    ets_fc <- forecast::forecast(ets_fit, h = n_test, level = confidence * 100)
    results$ETS <- list(
      model = ets_fit,
      forecast = ets_fc,
      predicted = as.numeric(ets_fc$mean),
      lower = as.numeric(ets_fc$lower),
      upper = as.numeric(ets_fc$upper),
      actual = test_values
    )
  }, error = function(e) {
    message("ETS failed: ", e$message)
  })

  # 3. XGBoost
  tryCatch({
    xgb_result <- fit_xgboost_ts(y_values, n_train, n_test, frequency)
    results$XGBoost <- xgb_result
  }, error = function(e) {
    message("XGBoost failed: ", e$message)
  })

  if (length(results) == 0) {
    return(list(
      error = "All models failed to fit.",
      forecasts = NULL,
      accuracy = NULL
    ))
  }

  # --- Calculate accuracy ---
  accuracy_df <- dplyr::bind_rows(lapply(names(results), function(name) {
    r <- results[[name]]
    pred <- r$predicted
    act <- r$actual
    n_compare <- min(length(pred), length(act))
    pred <- pred[1:n_compare]
    act <- act[1:n_compare]

    mape_vals <- abs((act - pred) / ifelse(act == 0, NA, act))
    mape_vals <- mape_vals[is.finite(mape_vals)]
    mape <- if (length(mape_vals) > 0) mean(mape_vals) * 100 else 999

    rmse_val <- sqrt(mean((act - pred)^2, na.rm = TRUE))
    mae_val  <- mean(abs(act - pred), na.rm = TRUE)

    dplyr::tibble(
      Model = name,
      MAPE  = if (is.finite(mape)) mape else 999,
      RMSE  = if (is.finite(rmse_val)) rmse_val else 999999,
      MAE   = if (is.finite(mae_val)) mae_val else 999999
    )
  }))

  # --- Select best model ---
  best_model_name <- if (nrow(accuracy_df) > 0) {
    accuracy_df %>%
      dplyr::arrange(MAPE, RMSE) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(Model)
  } else {
    names(results)[1]
  }

  if (is.null(best_model_name) || length(best_model_name) == 0 || is.na(best_model_name)) {
    best_model_name <- names(results)[1]
  }

  # --- Generate future forecast using best model on FULL data ---
  future_fc <- generate_future_forecast(ts_data, y_values, best_model_name,
                                         horizon, frequency, confidence)

  # --- Build forecast tibble ---
  last_date <- max(dates, na.rm = TRUE)
  future_dates <- seq(last_date + months(1), by = "month", length.out = horizon)

  forecast_df <- dplyr::tibble(
    date = c(dates, future_dates),
    actual = c(y_values, rep(NA, horizon)),
    predicted = c(rep(NA, n), future_fc$mean),
    lower = c(rep(NA, n), future_fc$lower),
    upper = c(rep(NA, n), future_fc$upper),
    type = c(rep("Historical", n), rep("Forecast", horizon))
  )

  list(
    forecasts   = forecast_df,
    accuracy    = accuracy_df,
    best_model  = best_model_name,
    comparison  = results,
    monthly     = monthly,
    error       = NULL
  )
}

#' Fit XGBoost for time series (supervised approach with lag features)
#'
#' @param y_values Numeric vector of the full series
#' @param n_train Number of training observations
#' @param n_test Number of test observations
#' @param frequency Seasonal frequency
#' @return List with predicted, actual, lower, upper
fit_xgboost_ts <- function(y_values, n_train, n_test, frequency = 12) {
  # Create lag features
  max_lag <- min(frequency, n_train - 1, 6)
  df <- create_lag_features(y_values, max_lag)

  # Split
  train_df <- df[1:n_train, ]
  test_df  <- df[(n_train + 1):(n_train + n_test), ]

  # Remove rows with NAs from training
  train_df <- train_df[complete.cases(train_df), ]

  if (nrow(train_df) < 5) {
    stop("Not enough training data for XGBoost after creating lag features")
  }

  feature_cols <- setdiff(colnames(train_df), "y")

  # Fit
  dtrain <- xgboost::xgb.DMatrix(
    data = as.matrix(train_df[, feature_cols]),
    label = train_df$y
  )

  params <- list(
    objective = "reg:squarederror",
    max_depth = 4,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 3
  )

  xgb_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )

  # Predict on test
  dtest <- xgboost::xgb.DMatrix(
    data = as.matrix(test_df[, feature_cols])
  )
  predicted <- predict(xgb_model, dtest)

  # Simple prediction intervals (bootstrap-inspired heuristic)
  residuals <- train_df$y - predict(xgb_model, dtrain)
  se <- sd(residuals, na.rm = TRUE)

  list(
    model     = xgb_model,
    predicted = predicted,
    actual    = y_values[(n_train + 1):(n_train + n_test)],
    lower     = predicted - 1.96 * se,
    upper     = predicted + 1.96 * se,
    features  = feature_cols
  )
}

#' Create lag and calendar features for time series
#'
#' @param y Numeric vector
#' @param max_lag Maximum number of lag features
#' @return data.frame with y and features
create_lag_features <- function(y, max_lag = 6) {
  n <- length(y)
  df <- data.frame(y = y)

  # Lag features
  for (lag in 1:max_lag) {
    df[[paste0("lag_", lag)]] <- dplyr::lag(y, lag)
  }

  # Rolling statistics
  if (n >= 3) {
    df$roll_mean_3 <- zoo::rollmean(y, k = 3, fill = NA, align = "right")
  }
  if (n >= 6) {
    df$roll_mean_6 <- zoo::rollmean(y, k = 6, fill = NA, align = "right")
  }

  # Trend
  df$trend <- seq_len(n)

  # Month indicator (cyclic)
  df$month_sin <- sin(2 * pi * (seq_len(n) %% 12) / 12)
  df$month_cos <- cos(2 * pi * (seq_len(n) %% 12) / 12)

  df
}

#' Generate future forecast using the best model on full data
#'
#' @param ts_data Full ts object
#' @param y_values Full numeric vector
#' @param model_name "ARIMA", "ETS", or "XGBoost"
#' @param horizon Forecast horizon
#' @param frequency Seasonal frequency
#' @param confidence Confidence level
#' @return List with mean, lower, upper
generate_future_forecast <- function(ts_data, y_values, model_name,
                                      horizon, frequency, confidence) {
  if (model_name == "ARIMA") {
    fit <- forecast::auto.arima(ts_data, stepwise = TRUE, approximation = TRUE)
    fc <- forecast::forecast(fit, h = horizon, level = confidence * 100)
    list(
      mean  = as.numeric(fc$mean),
      lower = as.numeric(fc$lower),
      upper = as.numeric(fc$upper)
    )
  } else if (model_name == "ETS") {
    fit <- forecast::ets(ts_data)
    fc <- forecast::forecast(fit, h = horizon, level = confidence * 100)
    list(
      mean  = as.numeric(fc$mean),
      lower = as.numeric(fc$lower),
      upper = as.numeric(fc$upper)
    )
  } else if (model_name == "XGBoost") {
    # Iterative forecasting
    max_lag <- min(frequency, length(y_values) - 1, 6)
    df <- create_lag_features(y_values, max_lag)
    df_complete <- df[complete.cases(df), ]
    feature_cols <- setdiff(colnames(df_complete), "y")

    dtrain <- xgboost::xgb.DMatrix(
      data = as.matrix(df_complete[, feature_cols]),
      label = df_complete$y
    )

    params <- list(
      objective = "reg:squarederror",
      max_depth = 4,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8
    )

    xgb_model <- xgboost::xgb.train(
      params = params, data = dtrain, nrounds = 100, verbose = 0
    )

    # Residual SE for intervals
    residuals <- df_complete$y - predict(xgb_model, dtrain)
    se <- sd(residuals, na.rm = TRUE)

    # Iterative multi-step forecast
    current_series <- y_values
    predictions <- numeric(horizon)

    for (h in seq_len(horizon)) {
      new_features <- create_lag_features(current_series, max_lag)
      last_row <- new_features[nrow(new_features), feature_cols, drop = FALSE]
      last_row$trend <- length(current_series) + 1
      last_row$month_sin <- sin(2 * pi * ((length(current_series) + 1) %% 12) / 12)
      last_row$month_cos <- cos(2 * pi * ((length(current_series) + 1) %% 12) / 12)

      dnew <- xgboost::xgb.DMatrix(data = as.matrix(last_row))
      pred <- predict(xgb_model, dnew)
      predictions[h] <- pred
      current_series <- c(current_series, pred)
    }

    list(
      mean  = predictions,
      lower = predictions - 1.96 * se,
      upper = predictions + 1.96 * se
    )
  } else {
    list(
      mean  = rep(mean(y_values), horizon),
      lower = rep(mean(y_values) - 2 * sd(y_values), horizon),
      upper = rep(mean(y_values) + 2 * sd(y_values), horizon)
    )
  }
}
