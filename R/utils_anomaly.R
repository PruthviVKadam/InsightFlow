# ============================================================================
# InsightFlow — Anomaly Detection Utilities
# Z-Score, STL decomposition, and Isolation Forest methods.
# ============================================================================

#' Run anomaly detection using the specified method
#'
#' @param data Tibble with date and value columns
#' @param value_col Name of the value column
#' @param method One of "zscore", "stl", "isolation_forest"
#' @param threshold Sensitivity threshold
#' @return Tibble with anomaly flags and scores
detect_anomalies <- function(data, value_col = "total", method = "zscore",
                              threshold = 3.0) {
  if (is.null(data) || nrow(data) == 0) return(dplyr::tibble())

  result <- switch(method,
    "zscore"           = detect_zscore(data, value_col, threshold),
    "stl"              = detect_stl(data, value_col, threshold),
    "isolation_forest" = detect_isolation_forest(data, value_col, threshold),
    detect_zscore(data, value_col, threshold)  # default
  )

  result
}

#' Z-Score based anomaly detection
#'
#' @param data Tibble with a value column
#' @param value_col Column to analyze
#' @param threshold Z-score threshold (default 3.0)
#' @return Tibble with anomaly flag, z_score, and severity
detect_zscore <- function(data, value_col = "total", threshold = 3.0) {
  values <- data[[value_col]]
  mu <- mean(values, na.rm = TRUE)
  sigma <- sd(values, na.rm = TRUE)

  if (sigma == 0 || is.na(sigma)) {
    data$z_score <- 0
    data$is_anomaly <- FALSE
    data$severity <- "Normal"
    data$method <- "Z-Score"
    return(data)
  }

  data %>%
    dplyr::mutate(
      z_score = (!!rlang::sym(value_col) - mu) / sigma,
      is_anomaly = abs(z_score) > threshold,
      severity = dplyr::case_when(
        abs(z_score) > threshold * 1.5 ~ "Critical",
        abs(z_score) > threshold ~ "Warning",
        TRUE ~ "Normal"
      ),
      anomaly_type = dplyr::case_when(
        z_score > threshold ~ "Spike",
        z_score < -threshold ~ "Drop",
        TRUE ~ NA_character_
      ),
      method = "Z-Score"
    )
}

#' STL decomposition residual-based anomaly detection
#'
#' @param data Tibble with date and value columns
#' @param value_col Column to analyze
#' @param threshold Multiplier for residual IQR
#' @return Tibble with anomaly flags
detect_stl <- function(data, value_col = "total", threshold = 2.0) {
  # Need at least 2 full periods for STL
  if (nrow(data) < 14) {
    # Fall back to z-score if not enough data
    return(detect_zscore(data, value_col, threshold))
  }

  # Aggregate to daily if date column exists
  if ("date" %in% colnames(data)) {
    daily <- data %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(
        value = sum(!!rlang::sym(value_col), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(date)
  } else {
    daily <- data %>%
      dplyr::mutate(value = !!rlang::sym(value_col)) %>%
      dplyr::slice_head(n = nrow(data))
  }

  # Create time series (weekly seasonality)
  freq <- min(7, nrow(daily) %/% 2)
  if (freq < 2) freq <- 2

  tryCatch({
    ts_data <- ts(daily$value, frequency = freq)
    decomp <- stl(ts_data, s.window = "periodic", robust = TRUE)
    residuals <- as.numeric(decomp$time.series[, "remainder"])

    # Flag outliers in residuals
    q1 <- quantile(residuals, 0.25, na.rm = TRUE)
    q3 <- quantile(residuals, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - threshold * iqr
    upper_bound <- q3 + threshold * iqr

    daily <- daily %>%
      dplyr::mutate(
        residual = residuals,
        is_anomaly = residual < lower_bound | residual > upper_bound,
        z_score = (residual - mean(residual, na.rm = TRUE)) /
                  sd(residual, na.rm = TRUE),
        severity = dplyr::case_when(
          abs(residual) > q3 + threshold * 1.5 * iqr ~ "Critical",
          is_anomaly ~ "Warning",
          TRUE ~ "Normal"
        ),
        anomaly_type = dplyr::case_when(
          residual > upper_bound ~ "Spike",
          residual < lower_bound ~ "Drop",
          TRUE ~ NA_character_
        ),
        method = "STL Decomposition"
      )

    daily
  }, error = function(e) {
    # Fallback to z-score
    detect_zscore(data, value_col, threshold)
  })
}

#' Isolation Forest anomaly detection
#'
#' @param data Tibble with value columns
#' @param value_col Primary column to analyze
#' @param threshold Anomaly score threshold (0-1, lower = more anomalous)
#' @return Tibble with anomaly flags
detect_isolation_forest <- function(data, value_col = "total", threshold = 0.6) {
  tryCatch({
    if (!requireNamespace("solitude", quietly = TRUE)) {
      return(detect_zscore(data, value_col, threshold))
    }

    # Prepare features
    numeric_cols <- sapply(data, is.numeric)
    feature_data <- data[, numeric_cols, drop = FALSE]

    if (ncol(feature_data) == 0) {
      return(detect_zscore(data, value_col, threshold))
    }

    # Remove columns with zero variance
    variances <- sapply(feature_data, var, na.rm = TRUE)
    feature_data <- feature_data[, variances > 0, drop = FALSE]

    if (ncol(feature_data) == 0) {
      return(detect_zscore(data, value_col, threshold))
    }

    # Handle NAs
    feature_data <- feature_data[complete.cases(feature_data), ]
    if (nrow(feature_data) < 10) {
      return(detect_zscore(data, value_col, threshold))
    }

    # Fit Isolation Forest
    iso_forest <- solitude::isolationForest$new(
      num_trees = 100,
      sample_size = min(256, nrow(feature_data))
    )
    iso_forest$fit(feature_data)

    # Predict anomaly scores
    scores <- iso_forest$predict(feature_data)

    data_subset <- data[complete.cases(data[, numeric_cols]), ]

    data_subset %>%
      dplyr::mutate(
        anomaly_score = scores$anomaly_score,
        is_anomaly = anomaly_score > threshold,
        z_score = anomaly_score,
        severity = dplyr::case_when(
          anomaly_score > 0.8 ~ "Critical",
          anomaly_score > threshold ~ "Warning",
          TRUE ~ "Normal"
        ),
        anomaly_type = dplyr::if_else(is_anomaly, "Outlier", NA_character_),
        method = "Isolation Forest"
      )
  }, error = function(e) {
    detect_zscore(data, value_col, threshold)
  })
}

#' Summarize detected anomalies with human-readable explanations
#'
#' @param anomaly_data Output from detect_anomalies()
#' @return Summary tibble
summarize_anomalies <- function(anomaly_data) {
  if (is.null(anomaly_data) || nrow(anomaly_data) == 0) return(dplyr::tibble())
  if (!"is_anomaly" %in% colnames(anomaly_data)) return(dplyr::tibble())

  anomalies <- anomaly_data %>% dplyr::filter(is_anomaly)

  if (nrow(anomalies) == 0) return(dplyr::tibble())

  summary_cols <- intersect(
    c("date", "value", "total", "amount", "z_score", "severity",
      "anomaly_type", "method"),
    colnames(anomalies)
  )

  anomalies %>%
    dplyr::select(dplyr::any_of(summary_cols)) %>%
    dplyr::arrange(dplyr::desc(abs(
      if ("z_score" %in% colnames(.)) z_score else 0
    )))
}
