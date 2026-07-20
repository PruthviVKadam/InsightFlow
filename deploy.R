# ============================================================================
# InsightFlow — Deployment script (shinyapps.io)
# ----------------------------------------------------------------------------
# One-time account setup (do this once, credentials are NOT stored in this repo):
#
#   1. Create a free account at https://www.shinyapps.io
#   2. Dashboard -> Account -> Tokens -> Show -> copy the setAccountInfo call
#   3. Run it once in R:
#        rsconnect::setAccountInfo(name = "<account>",
#                                  token = "<token>",
#                                  secret = "<secret>")
#
# The credentials are saved to your user config (~/.config/rsconnect on Linux/mac,
# %APPDATA%/R/config/R/rsconnect on Windows) and picked up automatically below.
#
# Then deploy with:
#   Rscript deploy.R
# ============================================================================

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

# Files bundled to the server. The sample .xlsx are included so the live app's
# "Use Sample Data" button works; the local SQLite db, tests, raw/clean data,
# and IDE files are intentionally excluded.
app_files <- c(
  "app.R",
  "DESCRIPTION",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", full.names = TRUE),
  "config/config.yml",
  "reports/executive_report.qmd",
  list.files("data/sample", pattern = "\\.xlsx$", full.names = TRUE)
)

missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Missing files for deploy bundle:\n  ",
       paste(missing, collapse = "\n  "),
       "\nRun scripts/generate_sample_data.R first if the sample data is absent.")
}

accounts <- rsconnect::accounts()
if (is.null(accounts) || nrow(accounts) == 0) {
  stop("No shinyapps.io account configured. See the setAccountInfo steps at the ",
       "top of this file, then re-run.")
}

message("Deploying ", length(app_files), " files as account '",
        accounts$name[1], "' ...")

rsconnect::deployApp(
  appDir         = ".",
  appName        = "insightflow",
  appTitle       = "InsightFlow - Business Forecasting Platform",
  appFiles       = app_files,
  account        = accounts$name[1],
  forceUpdate    = TRUE,
  launch.browser = FALSE
)
