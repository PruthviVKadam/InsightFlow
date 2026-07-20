# ============================================================================
# InsightFlow — Main Application Entry Point
# AI-Powered Business Performance & Forecasting Platform
# ============================================================================

# --- Load packages ---
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(janitor)
library(stringr)
library(scales)
library(DBI)
library(RSQLite)
library(waiter)
library(htmltools)
library(shinyWidgets)

# --- Source all modules and utilities ---
# Shiny automatically sources files in R/ but we do it explicitly for clarity
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

# --- App Configuration ---
app_config <- yaml::read_yaml("config/config.yml")
cfg <- app_config$default

# --- Theme ---
insightflow_theme <- bslib::bs_theme(
  version    = 5,
  bootswatch = "darkly",
  primary    = "#6366f1",
  secondary  = "#334155",
  success    = "#10b981",
  info       = "#06b6d4",
  warning    = "#f59e0b",
  danger     = "#ef4444",
  base_font  = font_google("Inter"),
  heading_font = font_google("Inter"),
  "body-bg"  = "#0f172a",
  "body-color" = "#f1f5f9",
  "card-bg"  = "#1e293b"
)

# ============================================================================
# UI
# ============================================================================
ui <- bslib::page_navbar(
  title = tags$span(
    tags$i(class = "fas fa-chart-line me-2"),
    "InsightFlow"
  ),
  theme = insightflow_theme,
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"
      )
    ),
    waiter::useWaiter()
  ),
  fillable = TRUE,

  # --- Navigation Panels ---

  # 1. Dashboard
  bslib::nav_panel(
    title = "Dashboard",
    icon  = shiny::icon("gauge-high"),
    mod_dashboard_ui("dashboard")
  ),

  # 2. Sales
  bslib::nav_panel(
    title = "Sales",
    icon  = shiny::icon("chart-line"),
    mod_sales_ui("sales")
  ),

  # 3. Customers
  bslib::nav_panel(
    title = "Customers",
    icon  = shiny::icon("users"),
    mod_customers_ui("customers")
  ),

  # 4. Forecast
  bslib::nav_panel(
    title = "Forecast",
    icon  = shiny::icon("crystal-ball", lib = "glyphicon", class = "fa-solid fa-wand-magic-sparkles"),
    mod_forecast_ui("forecast")
  ),

  # 5. Inventory
  bslib::nav_panel(
    title = "Inventory",
    icon  = shiny::icon("boxes-stacked"),
    mod_inventory_ui("inventory")
  ),

  # 6. Anomalies
  bslib::nav_panel(
    title = "Anomalies",
    icon  = shiny::icon("triangle-exclamation"),
    mod_anomalies_ui("anomalies")
  ),

  # 7. Simulator
  bslib::nav_panel(
    title = "Simulator",
    icon  = shiny::icon("sliders"),
    mod_simulator_ui("simulator")
  ),

  # 8. Reports
  bslib::nav_panel(
    title = "Reports",
    icon  = shiny::icon("file-pdf"),
    mod_reports_ui("reports")
  ),

  # 9. Settings (in a menu)
  bslib::nav_spacer(),
  bslib::nav_menu(
    title = "More",
    icon  = shiny::icon("ellipsis"),
    bslib::nav_panel(
      title = "Data Upload",
      icon  = shiny::icon("cloud-upload-alt"),
      mod_upload_ui("upload")
    ),
    bslib::nav_panel(
      title = "Settings",
      icon  = shiny::icon("gear"),
      mod_settings_ui("settings")
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  # --- Initialize database ---
  db_path <- cfg$database$path
  con <- init_db(db_path)

  # Close connection when app stops

  shiny::onStop(function() {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  })

  # Reactive wrapper for connection
  db_con <- shiny::reactive(con)

  # --- Shared reactive state ---
  app_data <- shiny::reactiveValues(
    data_loaded  = FALSE,
    last_refresh = NULL
  )

  # Check if data already exists on startup
  shiny::observe({
    counts <- get_table_counts(con)
    if (any(counts > 0)) {
      app_data$data_loaded <- TRUE
      app_data$last_refresh <- Sys.time()
    }
  })

  # --- Module Servers ---
  mod_upload_server("upload", db_con, app_data)
  mod_dashboard_server("dashboard", db_con, app_data)
  mod_sales_server("sales", db_con, app_data)
  mod_customers_server("customers", db_con, app_data)
  mod_forecast_server("forecast", db_con, app_data)
  mod_inventory_server("inventory", db_con, app_data)
  mod_anomalies_server("anomalies", db_con, app_data)
  mod_simulator_server("simulator", db_con, app_data)
  mod_reports_server("reports", db_con, app_data)
  mod_settings_server("settings", db_con, app_data)
}

# ============================================================================
# Run Application
# ============================================================================
shiny::shinyApp(ui = ui, server = server)
