# ============================================================================
# InsightFlow — SQLite Database Utilities
# Functions for managing the application's SQLite database.
# ============================================================================

#' Initialize the SQLite database and create tables if they don't exist
#'
#' @param db_path Path to the SQLite database file
#' @return DBI connection object
init_db <- function(db_path = "insightflow.db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Create tables
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sales (
      order_id TEXT,
      date TEXT,
      product TEXT,
      category TEXT,
      quantity REAL,
      unit_price REAL,
      total REAL,
      customer_id TEXT,
      region TEXT,
      channel TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS inventory (
      product_id TEXT,
      product TEXT,
      category TEXT,
      stock_quantity REAL,
      reorder_level REAL,
      unit_cost REAL,
      warehouse TEXT,
      last_restocked TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS expenses (
      date TEXT,
      category TEXT,
      amount REAL,
      description TEXT,
      department TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS customers (
      customer_id TEXT,
      name TEXT,
      email TEXT,
      join_date TEXT,
      region TEXT,
      segment TEXT,
      total_purchases REAL,
      last_purchase_date TEXT
    )
  ")

  # Create indices for common queries
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_sales_date ON sales(date)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_sales_customer ON sales(customer_id)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_expenses_date ON expenses(date)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_customers_id ON customers(customer_id)")

  con
}

#' Write cleaned data to the appropriate table
#'
#' @param con DBI connection
#' @param data A data.frame/tibble to write
#' @param table_name Name of the target table
#' @param overwrite If TRUE, replaces existing data. Default TRUE.
#' @return Number of rows written
write_clean_data <- function(con, data, table_name, overwrite = TRUE) {
  if (overwrite) {
    DBI::dbExecute(con, sprintf("DELETE FROM %s", table_name))
  }

  # Ensure date columns are stored as text in ISO format
  date_cols <- c("date", "join_date", "last_purchase_date", "last_restocked")
  for (col in intersect(date_cols, colnames(data))) {
    if (inherits(data[[col]], "Date") || inherits(data[[col]], "POSIXct")) {
      data[[col]] <- format(data[[col]], "%Y-%m-%d")
    }
  }

  DBI::dbWriteTable(con, table_name, as.data.frame(data), append = TRUE)
  nrow(data)
}

#' Read a table from the database with optional filters
#'
#' @param con DBI connection
#' @param table_name Name of the table to read
#' @param date_from Optional start date filter (character "YYYY-MM-DD")
#' @param date_to Optional end date filter (character "YYYY-MM-DD")
#' @param date_col Name of the date column to filter on
#' @return tibble
read_table <- function(con, table_name, date_from = NULL, date_to = NULL,
                       date_col = "date") {
  if (!DBI::dbExistsTable(con, table_name)) {
    return(dplyr::tibble())
  }

  query <- sprintf("SELECT * FROM %s", table_name)
  params <- list()
  conditions <- character(0)

  if (!is.null(date_from) && date_col %in%
      DBI::dbListFields(con, table_name)) {
    conditions <- c(conditions, sprintf("%s >= ?", date_col))
    params <- c(params, list(as.character(date_from)))
  }

  if (!is.null(date_to) && date_col %in%
      DBI::dbListFields(con, table_name)) {
    conditions <- c(conditions, sprintf("%s <= ?", date_col))
    params <- c(params, list(as.character(date_to)))
  }

  if (length(conditions) > 0) {
    query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
  }

  result <- if (length(params) > 0) {
    DBI::dbGetQuery(con, query, params = params)
  } else {
    DBI::dbGetQuery(con, query)
  }

  # Convert date columns back to Date type
  date_cols <- c("date", "join_date", "last_purchase_date", "last_restocked")
  for (col in intersect(date_cols, colnames(result))) {
    result[[col]] <- as.Date(result[[col]])
  }

  dplyr::as_tibble(result)
}

#' Get the date range available in a table
#'
#' @param con DBI connection
#' @param table_name Table name
#' @param date_col Date column name
#' @return Named list with 'min' and 'max' Date values
get_date_range <- function(con, table_name, date_col = "date") {
  if (!DBI::dbExistsTable(con, table_name)) {
    return(list(min = Sys.Date() - 365, max = Sys.Date()))
  }

  query <- sprintf("SELECT MIN(%s) as min_date, MAX(%s) as max_date FROM %s",
                    date_col, date_col, table_name)
  result <- DBI::dbGetQuery(con, query)

  list(
    min = as.Date(result$min_date %||% Sys.Date() - 365),
    max = as.Date(result$max_date %||% Sys.Date())
  )
}

#' Check if a table has data
#'
#' @param con DBI connection
#' @param table_name Table name
#' @return Logical
table_has_data <- function(con, table_name) {
  if (!DBI::dbExistsTable(con, table_name)) return(FALSE)
  result <- DBI::dbGetQuery(con,
    sprintf("SELECT COUNT(*) as n FROM %s", table_name))
  result$n > 0
}

#' Get row counts for all tables
#'
#' @param con DBI connection
#' @return Named numeric vector
get_table_counts <- function(con) {
  tables <- c("sales", "inventory", "expenses", "customers")
  counts <- sapply(tables, function(t) {
    if (DBI::dbExistsTable(con, t)) {
      DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM %s", t))$n
    } else {
      0
    }
  })
  names(counts) <- tables
  counts
}

#' Clear all data from the database
#'
#' @param con DBI connection
clear_database <- function(con) {
  tables <- c("sales", "inventory", "expenses", "customers")
  for (t in tables) {
    if (DBI::dbExistsTable(con, t)) {
      DBI::dbExecute(con, sprintf("DELETE FROM %s", t))
    }
  }
}

#' Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
