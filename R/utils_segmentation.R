# ============================================================================
# InsightFlow — Customer Segmentation Utilities
# RFM analysis and K-Means clustering for customer segmentation.
# ============================================================================

#' Calculate RFM (Recency, Frequency, Monetary) scores
#'
#' @param sales Sales tibble (must have customer_id, date, total)
#' @param reference_date Date to calculate recency from (default: max date in data)
#' @return Tibble with customer_id, recency, frequency, monetary, and R/F/M scores
calc_rfm <- function(sales, reference_date = NULL) {
  if (is.null(sales) || nrow(sales) == 0) return(dplyr::tibble())
  required <- c("customer_id", "date", "total")
  if (!all(required %in% colnames(sales))) return(dplyr::tibble())

  if (is.null(reference_date)) {
    reference_date <- max(sales$date, na.rm = TRUE) + 1
  }

  rfm <- sales %>%
    dplyr::group_by(customer_id) %>%
    dplyr::summarise(
      recency   = as.numeric(difftime(reference_date,
                                       max(date, na.rm = TRUE), units = "days")),
      frequency = dplyr::n(),
      monetary  = sum(total, na.rm = TRUE),
      .groups   = "drop"
    )

  # Score each dimension (1-5, with 5 being best)
  # For recency, lower is better, so we reverse
  rfm <- rfm %>%
    dplyr::mutate(
      R_score = dplyr::ntile(dplyr::desc(recency), 5),
      F_score = dplyr::ntile(frequency, 5),
      M_score = dplyr::ntile(monetary, 5),
      RFM_score = R_score * 100 + F_score * 10 + M_score
    )

  rfm
}

#' Run K-Means clustering on RFM data
#'
#' @param rfm RFM tibble from calc_rfm()
#' @param k Number of clusters (NULL = auto-select)
#' @param max_k Maximum k to test for auto-selection
#' @return List with: clustered_data, centers, k, elbow_data
run_kmeans <- function(rfm, k = NULL, max_k = 8) {
  if (nrow(rfm) < 10) {
    return(list(error = "Need at least 10 customers for clustering"))
  }

  # Scale features
  features <- rfm %>%
    dplyr::select(recency, frequency, monetary)
  scaled <- scale(features)

  # Auto-select k using elbow method
  elbow_data <- dplyr::tibble(k = integer(), wss = numeric())

  max_k <- min(max_k, nrow(rfm) - 1)

  for (i in 2:max_k) {
    set.seed(42)
    km <- kmeans(scaled, centers = i, nstart = 25, iter.max = 100)
    elbow_data <- dplyr::bind_rows(
      elbow_data,
      dplyr::tibble(k = i, wss = km$tot.withinss)
    )
  }

  # Simple elbow detection: max second derivative
  if (is.null(k)) {
    if (nrow(elbow_data) >= 3) {
      diffs <- diff(elbow_data$wss)
      diffs2 <- diff(diffs)
      k <- elbow_data$k[which.max(abs(diffs2)) + 1]
      k <- max(2, min(k, max_k))
    } else {
      k <- 3
    }
  }

  # Final clustering
  set.seed(42)
  km_final <- kmeans(scaled, centers = k, nstart = 25, iter.max = 100)

  # Add cluster labels
  clustered <- rfm %>%
    dplyr::mutate(
      cluster = km_final$cluster,
      segment = label_segments(km_final, scaled, rfm)
    )

  list(
    clustered_data = clustered,
    centers        = km_final$centers,
    k              = k,
    elbow_data     = elbow_data,
    model          = km_final,
    error          = NULL
  )
}

#' Label clusters with business-meaningful segment names
#'
#' @param km_result kmeans object
#' @param scaled_data Scaled feature matrix
#' @param rfm Original RFM data
#' @return Character vector of segment labels
label_segments <- function(km_result, scaled_data, rfm) {
  centers <- km_result$centers
  clusters <- km_result$cluster

  # Analyze each cluster's characteristics
  # Centers columns: recency, frequency, monetary (scaled)
  # Lower recency = more recent = better
  # Higher frequency = better
  # Higher monetary = better

  cluster_labels <- character(nrow(centers))

  for (i in seq_len(nrow(centers))) {
    r <- centers[i, "recency"]    # lower = more recent
    f <- centers[i, "frequency"]  # higher = more frequent
    m <- centers[i, "monetary"]   # higher = more spend

    if (f > 0.5 && m > 0.5 && r < 0) {
      cluster_labels[i] <- "VIP"
    } else if (f > 0 && m > 0 && r < 0.3) {
      cluster_labels[i] <- "Loyal"
    } else if (r > 0.5 && f < 0 && m < 0) {
      cluster_labels[i] <- "Lost"
    } else if (r < -0.3 && f < 0 && m < 0) {
      cluster_labels[i] <- "New"
    } else if (r > 0 && f > -0.5) {
      cluster_labels[i] <- "At Risk"
    } else {
      cluster_labels[i] <- "Potential"
    }
  }

  # Handle duplicate labels by adding numbers
  dupes <- duplicated(cluster_labels)
  if (any(dupes)) {
    for (label in unique(cluster_labels[dupes])) {
      idx <- which(cluster_labels == label)
      if (length(idx) > 1) {
        # Differentiate by monetary value
        monetary_order <- order(centers[idx, "monetary"], decreasing = TRUE)
        suffixes <- c(" (High Value)", " (Mid Value)", " (Low Value)", "", "")
        for (j in seq_along(idx)) {
          cluster_labels[idx[monetary_order[j]]] <- paste0(
            label, suffixes[min(j, length(suffixes))]
          )
        }
      }
    }
  }

  cluster_labels[clusters]
}

#' Generate segment summary statistics
#'
#' @param clustered_data Tibble with segment column
#' @return Summary tibble
summarize_segments <- function(clustered_data) {
  clustered_data %>%
    dplyr::group_by(segment) %>%
    dplyr::summarise(
      Customers  = dplyr::n(),
      `Avg Recency (days)` = round(mean(recency), 0),
      `Avg Frequency` = round(mean(frequency), 1),
      `Avg Monetary ($)` = round(mean(monetary), 2),
      `Total Revenue ($)` = round(sum(monetary), 2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(`Total Revenue ($)`))
}
