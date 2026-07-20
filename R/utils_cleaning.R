# ============================================================================
# InsightFlow — Data Cleaning Utilities
# Functions for cleaning and validating uploaded Excel data.
# ============================================================================

#' Auto-detect and clean uploaded data based on column signatures
#'
#' @param df A data.frame read from an Excel file
#' @param filename The original filename (used for type detection)
#' @return A list with: data (cleaned tibble), type (character), issues (character vector)
auto_detect_and_clean <- function(df, filename = "") {
  # Standardize column names first
  df <- janitor::clean_names(df)
  cols <- colnames(df)

  # Detect type by column signatures
  type <- detect_data_type(cols, filename)

  result <- switch(type,
    "sales"     = clean_sales_data(df),
    "inventory" = clean_inventory_data(df),
    "expenses"  = clean_expenses_data(df),
    "customers" = clean_customers_data(df),
    list(data = df, type = "unknown",
         issues = c("Could not auto-detect data type. Loaded as-is."))
  )

  result$type <- type
  result
}

#' Detect the type of data based on column names
#'
#' @param cols Character vector of column names
#' @param filename Original filename
#' @return Character: "sales", "inventory", "expenses", "customers", or "unknown"
detect_data_type <- function(cols, filename = "") {
  fname <- tolower(filename)

  # Check filename first

  if (grepl("sale", fname))      return("sales")
  if (grepl("inventory", fname)) return("inventory")
  if (grepl("expens", fname))    return("expenses")
  if (grepl("customer", fname))  return("customers")

  # Then check column signatures
  if (all(c("order_id", "quantity", "unit_price") %in% cols)) return("sales")
  if (all(c("stock_quantity", "reorder_level") %in% cols))    return("inventory")
  if (all(c("amount", "department") %in% cols))               return("expenses")
  if (all(c("customer_id", "email") %in% cols))               return("customers")

  # Partial matches
  if (any(grepl("order", cols)) && any(grepl("price|total", cols))) return("sales")
  if (any(grepl("stock|inventory", cols)))                          return("inventory")
  if (any(grepl("expens|amount", cols)) && any(grepl("dept|department", cols)))
    return("expenses")
  if (any(grepl("customer|cust", cols)))                            return("customers")

  "unknown"
}

#' Clean sales data
#'
#' @param df Raw sales data.frame
#' @return List with cleaned data and issues log
clean_sales_data <- function(df) {
  issues <- character(0)

  # Ensure required columns exist
  required <- c("date", "quantity", "unit_price")
  missing_cols <- setdiff(required, colnames(df))
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Remove exact duplicates
  n_before <- nrow(df)
  df <- dplyr::distinct(df)
  n_dupes <- n_before - nrow(df)
  if (n_dupes > 0) issues <- c(issues, sprintf("Removed %d duplicate rows", n_dupes))

  # Parse dates
  if ("date" %in% colnames(df)) {
    df$date <- parse_flexible_date(df$date)
    na_dates <- sum(is.na(df$date))
    if (na_dates > 0) {
      issues <- c(issues, sprintf("Removed %d rows with unparseable dates", na_dates))
      df <- df[!is.na(df$date), ]
    }
  }

  # Fix numeric columns
  numeric_cols <- c("quantity", "unit_price", "total")
  for (col in intersect(numeric_cols, colnames(df))) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    n_na <- sum(is.na(df[[col]]))
    if (n_na > 0) {
      issues <- c(issues, sprintf("%d NA values in '%s'", n_na, col))
    }
  }

  # Recalculate total if quantity and unit_price exist
  if (all(c("quantity", "unit_price") %in% colnames(df))) {
    recalc <- df$quantity * df$unit_price
    if ("total" %in% colnames(df)) {
      mismatch <- !is.na(df$total) & !is.na(recalc) &
                  abs(df$total - recalc) > 0.01
      if (sum(mismatch, na.rm = TRUE) > 0) {
        issues <- c(issues,
          sprintf("Corrected %d mismatched totals", sum(mismatch, na.rm = TRUE)))
      }
    }
    df$total <- round(recalc, 2)
  }

  # Remove rows with negative quantities
  if ("quantity" %in% colnames(df)) {
    neg <- !is.na(df$quantity) & df$quantity < 0
    if (any(neg)) {
      issues <- c(issues, sprintf("Removed %d rows with negative quantity", sum(neg)))
      df <- df[!neg, ]
    }
  }

  # Trim whitespace from character columns
  df <- trim_character_cols(df)

  # Sort by date
  if ("date" %in% colnames(df)) {
    df <- df[order(df$date), ]
  }

  if (length(issues) == 0) issues <- "No issues found. Data is clean."

  list(data = dplyr::as_tibble(df), type = "sales", issues = issues)
}

#' Clean inventory data
#'
#' @param df Raw inventory data.frame
#' @return List with cleaned data and issues log
clean_inventory_data <- function(df) {
  issues <- character(0)

  # Remove duplicates
  n_before <- nrow(df)
  df <- dplyr::distinct(df)
  n_dupes <- n_before - nrow(df)
  if (n_dupes > 0) issues <- c(issues, sprintf("Removed %d duplicate rows", n_dupes))

  # Fix numeric columns
  numeric_cols <- c("stock_quantity", "reorder_level", "unit_cost")
  for (col in intersect(numeric_cols, colnames(df))) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    n_na <- sum(is.na(df[[col]]))
    if (n_na > 0) {
      issues <- c(issues, sprintf("%d NA values in '%s'", n_na, col))
    }
  }

  # Parse date columns
  if ("last_restocked" %in% colnames(df)) {
    df$last_restocked <- parse_flexible_date(df$last_restocked)
  }

  # Validate stock quantities aren't negative
  if ("stock_quantity" %in% colnames(df)) {
    neg <- !is.na(df$stock_quantity) & df$stock_quantity < 0
    if (any(neg)) {
      issues <- c(issues,
        sprintf("Set %d negative stock quantities to 0", sum(neg)))
      df$stock_quantity[neg] <- 0
    }
  }

  df <- trim_character_cols(df)

  if (length(issues) == 0) issues <- "No issues found. Data is clean."

  list(data = dplyr::as_tibble(df), type = "inventory", issues = issues)
}

#' Clean expenses data
#'
#' @param df Raw expenses data.frame
#' @return List with cleaned data and issues log
clean_expenses_data <- function(df) {
  issues <- character(0)

  # Remove duplicates
  n_before <- nrow(df)
  df <- dplyr::distinct(df)
  n_dupes <- n_before - nrow(df)
  if (n_dupes > 0) issues <- c(issues, sprintf("Removed %d duplicate rows", n_dupes))

  # Parse dates
  if ("date" %in% colnames(df)) {
    df$date <- parse_flexible_date(df$date)
    na_dates <- sum(is.na(df$date))
    if (na_dates > 0) {
      issues <- c(issues, sprintf("Removed %d rows with unparseable dates", na_dates))
      df <- df[!is.na(df$date), ]
    }
  }

  # Fix amount
  if ("amount" %in% colnames(df)) {
    df$amount <- suppressWarnings(as.numeric(df$amount))
    neg <- !is.na(df$amount) & df$amount < 0
    if (any(neg)) {
      issues <- c(issues, sprintf("Converted %d negative amounts to positive", sum(neg)))
      df$amount[neg] <- abs(df$amount[neg])
    }
  }

  # Standardize category names
  if ("category" %in% colnames(df)) {
    df$category <- stringr::str_to_title(trimws(df$category))
  }

  df <- trim_character_cols(df)

  if ("date" %in% colnames(df)) {
    df <- df[order(df$date), ]
  }

  if (length(issues) == 0) issues <- "No issues found. Data is clean."

  list(data = dplyr::as_tibble(df), type = "expenses", issues = issues)
}

#' Clean customers data
#'
#' @param df Raw customers data.frame
#' @return List with cleaned data and issues log
clean_customers_data <- function(df) {
  issues <- character(0)

  # Remove exact duplicates
  n_before <- nrow(df)
  df <- dplyr::distinct(df)
  n_dupes <- n_before - nrow(df)
  if (n_dupes > 0) issues <- c(issues, sprintf("Removed %d duplicate rows", n_dupes))

  # Deduplicate by customer_id
  if ("customer_id" %in% colnames(df)) {
    n_before2 <- nrow(df)
    df <- df[!duplicated(df$customer_id), ]
    n_id_dupes <- n_before2 - nrow(df)
    if (n_id_dupes > 0) {
      issues <- c(issues,
        sprintf("Removed %d duplicate customer IDs (kept first occurrence)", n_id_dupes))
    }
  }

  # Validate emails
  if ("email" %in% colnames(df)) {
    valid_email <- grepl("^[[:alnum:]._+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}$",
                         df$email, perl = TRUE)
    invalid_count <- sum(!valid_email & !is.na(df$email))
    if (invalid_count > 0) {
      issues <- c(issues, sprintf("%d invalid email addresses found", invalid_count))
    }
    na_email <- sum(is.na(df$email))
    if (na_email > 0) {
      issues <- c(issues, sprintf("%d missing email addresses", na_email))
    }
  }

  # Parse dates
  date_cols <- c("join_date", "last_purchase_date")
  for (col in intersect(date_cols, colnames(df))) {
    df[[col]] <- parse_flexible_date(df[[col]])
  }

  # Fix numeric columns
  if ("total_purchases" %in% colnames(df)) {
    df$total_purchases <- suppressWarnings(as.numeric(df$total_purchases))
  }

  df <- trim_character_cols(df)

  if (length(issues) == 0) issues <- "No issues found. Data is clean."

  list(data = dplyr::as_tibble(df), type = "customers", issues = issues)
}

# --- Helper Functions ---

#' Parse dates flexibly, handling multiple formats
#'
#' @param x A vector of date-like values
#' @return Date vector
parse_flexible_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))

  # Try common formats
  x <- as.character(x)
  result <- suppressWarnings(lubridate::ymd(x))

  # Fallback: try mdy
  na_idx <- is.na(result)
  if (any(na_idx)) {
    result[na_idx] <- suppressWarnings(lubridate::mdy(x[na_idx]))
  }

  # Fallback: try dmy
  na_idx <- is.na(result)
  if (any(na_idx)) {
    result[na_idx] <- suppressWarnings(lubridate::dmy(x[na_idx]))
  }

  # Fallback: try Excel serial date numbers
  na_idx <- is.na(result)
  if (any(na_idx)) {
    numeric_vals <- suppressWarnings(as.numeric(x[na_idx]))
    valid_nums <- !is.na(numeric_vals) & numeric_vals > 30000 & numeric_vals < 60000
    if (any(valid_nums)) {
      result[na_idx][valid_nums] <- as.Date(numeric_vals[valid_nums],
                                             origin = "1899-12-30")
    }
  }

  result
}

#' Trim whitespace from all character columns
#'
#' @param df A data.frame
#' @return data.frame with trimmed character columns
trim_character_cols <- function(df) {
  char_cols <- sapply(df, is.character)
  df[char_cols] <- lapply(df[char_cols], trimws)
  df
}
