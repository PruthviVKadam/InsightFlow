# ============================================================================
# InsightFlow — Package Installer
# Run this script once to install all required dependencies.
# Usage: source("install_packages.R")
# ============================================================================

cat("
╔══════════════════════════════════════════════════════╗
║          InsightFlow — Installing Packages           ║
╚══════════════════════════════════════════════════════╝
\n")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  Installing %s...\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  } else {
    cat(sprintf("  ✓ %s already installed\n", pkg))
  }
}

# --- Core Data Wrangling ---
cat("\n── Core Data Wrangling ──\n")
core_data <- c("tidyverse", "readxl", "janitor", "data.table", "lubridate")
invisible(lapply(core_data, install_if_missing))

# --- Database ---
cat("\n── Database ──\n")
db_pkgs <- c("DBI", "RSQLite")
invisible(lapply(db_pkgs, install_if_missing))

# --- Dashboard & UI ---
cat("\n── Dashboard & UI ──\n")
dashboard_pkgs <- c("shiny", "bslib", "plotly", "DT", "shinyWidgets", "waiter",
                     "htmltools", "thematic")
invisible(lapply(dashboard_pkgs, install_if_missing))

# --- Visualization ---
cat("\n── Visualization ──\n")
viz_pkgs <- c("ggplot2", "scales", "RColorBrewer", "viridis")
invisible(lapply(viz_pkgs, install_if_missing))

# --- Forecasting ---
cat("\n── Forecasting ──\n")
forecast_pkgs <- c("forecast", "xgboost", "Metrics")
invisible(lapply(forecast_pkgs, install_if_missing))

# --- Machine Learning / Clustering ---
cat("\n── Machine Learning / Clustering ──\n")
ml_pkgs <- c("cluster", "factoextra", "solitude")
invisible(lapply(ml_pkgs, install_if_missing))

# --- Reporting ---
cat("\n── Reporting ──\n")
report_pkgs <- c("openxlsx2", "mschart", "quarto", "rmarkdown", "knitr",
                  "kableExtra")
invisible(lapply(report_pkgs, install_if_missing))

# --- Testing ---
cat("\n── Testing ──\n")
test_pkgs <- c("testthat")
invisible(lapply(test_pkgs, install_if_missing))

# --- Configuration ---
cat("\n── Configuration ──\n")
config_pkgs <- c("config", "yaml")
invisible(lapply(config_pkgs, install_if_missing))

cat("\n
╔══════════════════════════════════════════════════════╗
║            ✓ All packages installed!                 ║
║         Run: shiny::runApp() to start                ║
╚══════════════════════════════════════════════════════╝
\n")
