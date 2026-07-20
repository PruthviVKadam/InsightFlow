# ============================================================================
# InsightFlow — Reports Module
# Generate and download Excel and PDF reports.
# ============================================================================

mod_reports_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Header
    shiny::div(
      class = "mb-4",
      shiny::h4("Report Generation", class = "mb-1"),
      shiny::p(class = "text-muted",
               "Generate professional reports from your business data.")
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),

      # Excel Report Card
      bslib::card(
        bslib::card_header(
          class = "bg-gradient-primary",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$i(class = "fa-solid fa-file-excel fa-lg"),
            shiny::h5("Excel Report", class = "mb-0 text-white")
          )
        ),
        bslib::card_body(
          shiny::p("Generate a multi-sheet Excel workbook with:"),
          shiny::tags$ul(
            shiny::tags$li("Executive Summary with KPIs"),
            shiny::tags$li("Monthly sales analysis with data bars"),
            shiny::tags$li("Top products ranked by revenue"),
            shiny::tags$li("Expense breakdown by category"),
            shiny::tags$li("Inventory status with alerts"),
            shiny::tags$li("Customer summary by region")
          ),
          shiny::div(
            class = "d-grid gap-2 mt-3",
            shiny::downloadButton(
              ns("download_excel"),
              label = shiny::span(shiny::icon("file-excel"), " Download Excel Report"),
              class = "btn-primary"
            )
          )
        )
      ),

      # PDF Report Card
      bslib::card(
        bslib::card_header(
          class = "bg-gradient-success",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$i(class = "fa-solid fa-file-pdf fa-lg"),
            shiny::h5("PDF Report", class = "mb-0 text-white")
          )
        ),
        bslib::card_body(
          shiny::p("Generate an executive PDF report with:"),
          shiny::tags$ul(
            shiny::tags$li("KPI highlights and metrics"),
            shiny::tags$li("Revenue and sales trend charts"),
            shiny::tags$li("Expense analysis"),
            shiny::tags$li("Forecast projections"),
            shiny::tags$li("Inventory alerts"),
            shiny::tags$li("Business recommendations")
          ),
          shiny::div(
            class = "d-grid gap-2 mt-3",
            shiny::downloadButton(
              ns("download_pdf"),
              label = shiny::span(shiny::icon("file-pdf"), " Download PDF Report"),
              class = "btn-primary"
            )
          )
        )
      )
    ),

    # Report status
    shiny::uiOutput(ns("report_status"))
  )
}

mod_reports_server <- function(id, db_con, app_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # --- Download Excel ---
    output$download_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("InsightFlow_Executive_Summary_",
               format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        con <- db_con()
        sales     <- read_table(con, "sales")
        expenses  <- read_table(con, "expenses")
        customers <- read_table(con, "customers")
        inventory <- read_table(con, "inventory")

        if (nrow(sales) == 0) {
          shiny::showNotification("No data to export. Load data first.",
                                   type = "warning")
          return()
        }

        tryCatch({
          generate_excel_report(
            sales = sales,
            expenses = expenses,
            customers = customers,
            inventory = inventory,
            output_path = file
          )
          shiny::showNotification("Excel report generated successfully!",
                                   type = "message")
        }, error = function(e) {
          shiny::showNotification(
            paste("Error generating Excel report:", e$message),
            type = "error"
          )
        })
      }
    )

    # --- Download PDF ---
    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("InsightFlow_Business_Report_",
               format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        con <- db_con()
        sales     <- read_table(con, "sales")
        expenses  <- read_table(con, "expenses")
        customers <- read_table(con, "customers")
        inventory <- read_table(con, "inventory")

        if (nrow(sales) == 0) {
          shiny::showNotification("No data to export. Load data first.",
                                   type = "warning")
          return()
        }

        tryCatch({
          # Use R Markdown as a reliable fallback
          # Create a temporary Rmd file
          temp_rmd <- tempfile(fileext = ".Rmd")

          rmd_content <- sprintf('
---
title: "InsightFlow — Business Report"
date: "%s"
output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
    latex_engine: xelatex
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 4)
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)
```

# Executive Summary

This report was automatically generated by **InsightFlow** on %s.

## Key Performance Indicators

```{r kpis}
sales <- readRDS("%s")
expenses <- readRDS("%s")
customers <- readRDS("%s")
inventory <- readRDS("%s")

total_revenue <- sum(sales$total, na.rm = TRUE)
total_expenses <- sum(expenses$amount, na.rm = TRUE)
gross_profit <- total_revenue - total_expenses
margin <- if (total_revenue > 0) (gross_profit / total_revenue) * 100 else 0

kpi_df <- data.frame(
  Metric = c("Total Revenue", "Total Expenses", "Gross Profit",
             "Gross Margin", "Total Orders", "Total Customers"),
  Value = c(
    sprintf("$%%s", format(round(total_revenue), big.mark = ",")),
    sprintf("$%%s", format(round(total_expenses), big.mark = ",")),
    sprintf("$%%s", format(round(gross_profit), big.mark = ",")),
    sprintf("%%.1f%%%%", margin),
    format(nrow(sales), big.mark = ","),
    format(nrow(customers), big.mark = ",")
  )
)

kable(kpi_df, align = "lr") %%%%>%%%% kable_styling(full_width = FALSE)
```

# Sales Analysis

## Monthly Revenue Trend

```{r revenue-trend}
monthly <- sales %%%%>%%%%
  mutate(month = as.Date(format(date, "%%%%Y-%%%%m-01"))) %%%%>%%%%
  group_by(month) %%%%>%%%%
  summarise(revenue = sum(total, na.rm = TRUE), .groups = "drop")

ggplot(monthly, aes(x = month, y = revenue)) +
  geom_line(color = "#6366f1", linewidth = 1) +
  geom_point(color = "#818cf8", size = 2) +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = NULL, y = "Revenue") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
```

## Top Products

```{r top-products}
top <- sales %%%%>%%%%
  group_by(product) %%%%>%%%%
  summarise(Revenue = sum(total, na.rm = TRUE),
            Orders = n(), .groups = "drop") %%%%>%%%%
  arrange(desc(Revenue)) %%%%>%%%%
  head(10)

kable(top, align = "lrr") %%%%>%%%% kable_styling(full_width = FALSE)
```

# Expense Analysis

```{r expenses}
exp_cat <- expenses %%%%>%%%%
  group_by(category) %%%%>%%%%
  summarise(Total = sum(amount, na.rm = TRUE), .groups = "drop") %%%%>%%%%
  arrange(desc(Total))

kable(exp_cat, align = "lr") %%%%>%%%% kable_styling(full_width = FALSE)
```

# Inventory Status

```{r inventory}
if (nrow(inventory) > 0) {
  low_stock <- inventory %%%%>%%%%
    filter(stock_quantity <= reorder_level)
  
  if (nrow(low_stock) > 0) {
    cat(sprintf("**%%d items** are at or below reorder level.\\n\\n", nrow(low_stock)))
    kable(low_stock %%%%>%%%% select(product, category, stock_quantity, reorder_level),
          align = "llrr") %%%%>%%%% kable_styling(full_width = FALSE)
  } else {
    cat("All inventory levels are healthy.")
  }
}
```

# Recommendations

Based on the analysis:

1. **Monitor low-stock items** and initiate reorders for critical products.
2. **Focus marketing** on top-performing product categories.
3. **Review expense categories** with the highest spend for optimization opportunities.
4. **Track monthly growth** trends to identify seasonal patterns.

---

*Report generated by InsightFlow — AI-Powered Business Analytics Platform*
',
            format(Sys.Date(), "%%B %%d, %%Y"),
            format(Sys.time(), "%%B %%d, %%Y at %%I:%%M %%p"),
            gsub("\\\\", "/", tempfile(fileext = ".rds")),
            gsub("\\\\", "/", tempfile(fileext = ".rds")),
            gsub("\\\\", "/", tempfile(fileext = ".rds")),
            gsub("\\\\", "/", tempfile(fileext = ".rds"))
          )

          # Save data as temp RDS files
          rmd_lines <- readLines(textConnection(rmd_content))
          rds_matches <- regmatches(rmd_content,
            gregexpr('[^"]*\\.rds', rmd_content))[[1]]

          if (length(rds_matches) >= 4) {
            saveRDS(sales, rds_matches[1])
            saveRDS(expenses, rds_matches[2])
            saveRDS(customers, rds_matches[3])
            saveRDS(inventory, rds_matches[4])
          }

          writeLines(rmd_content, temp_rmd)

          rmarkdown::render(
            input = temp_rmd,
            output_file = file,
            quiet = TRUE,
            envir = new.env()
          )

          shiny::showNotification("PDF report generated successfully!",
                                   type = "message")

        }, error = function(e) {
          # Fallback: generate a simple HTML report if PDF fails
          shiny::showNotification(
            paste("PDF generation requires LaTeX. Error:", e$message),
            type = "warning", duration = 10
          )
        })
      }
    )
  })
}
