# ============================================================================
# InsightFlow — Excel Report Export Utilities
# Creates styled multi-sheet Excel reports using openxlsx2 + mschart.
# ============================================================================

#' Generate a styled Executive Summary Excel report
#'
#' @param sales Sales tibble
#' @param expenses Expenses tibble
#' @param customers Customers tibble
#' @param inventory Inventory tibble
#' @param output_path File path for the output .xlsx file
#' @return Path to the generated file
generate_excel_report <- function(sales, expenses, customers, inventory,
                                   output_path = "exports/Executive_Summary.xlsx") {
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  wb <- openxlsx2::wb_workbook()

  # --- Styles ---
  header_style <- openxlsx2::create_dxfs_style(
    font_bold = TRUE,
    font_color = openxlsx2::wb_color("#FFFFFF"),
    bg_fill = openxlsx2::wb_color("#4f46e5"),
    font_size = 11
  )

  subheader_style <- openxlsx2::create_dxfs_style(
    font_bold = TRUE,
    font_color = openxlsx2::wb_color("#FFFFFF"),
    bg_fill = openxlsx2::wb_color("#334155"),
    font_size = 10
  )

  # ===== Sheet 1: Executive Summary =====
  wb$add_worksheet("Executive Summary")

  # Title
  wb$add_data(x = "InsightFlow — Executive Summary", dims = "A1")
  wb$add_data(x = paste("Generated:", format(Sys.time(), "%B %d, %Y at %I:%M %p")),
              dims = "A2")
  wb$add_font(dims = "A1", bold = TRUE, size = 16, color = openxlsx2::wb_color("#4f46e5"),
              sheet = "Executive Summary")
  wb$add_font(dims = "A2", italic = TRUE, color = openxlsx2::wb_color("#64748b"),
              sheet = "Executive Summary")

  # KPI Table
  if (!is.null(sales) && nrow(sales) > 0) {
    kpis <- generate_kpi_summary(sales, expenses, customers, inventory)

    kpi_table <- data.frame(
      Metric = c("Total Revenue", "Total Expenses", "Gross Profit",
                  "Gross Margin", "Total Orders", "Avg Order Value",
                  "Monthly Growth", "Total Customers", "Inventory Turnover",
                  "Monthly Burn Rate"),
      Value = c(
        sprintf("$%s", format(round(kpis$total_revenue), big.mark = ",")),
        sprintf("$%s", format(round(kpis$total_expenses), big.mark = ",")),
        sprintf("$%s", format(round(kpis$gross_profit), big.mark = ",")),
        sprintf("%.1f%%", kpis$gross_margin),
        format(kpis$total_orders, big.mark = ","),
        sprintf("$%.2f", kpis$avg_order_value),
        sprintf("%.1f%%", kpis$monthly_growth),
        format(kpis$total_customers, big.mark = ","),
        sprintf("%.1fx", kpis$inventory_turnover),
        sprintf("$%s", format(round(kpis$burn_rate), big.mark = ","))
      ),
      stringsAsFactors = FALSE
    )

    wb$add_data(x = kpi_table, dims = "A4", sheet = "Executive Summary")

    # Style KPI header row
    wb$add_fill(dims = "A4:B4", color = openxlsx2::wb_color("#4f46e5"),
                sheet = "Executive Summary")
    wb$add_font(dims = "A4:B4", bold = TRUE,
                color = openxlsx2::wb_color("#FFFFFF"),
                sheet = "Executive Summary")

    # Conditional formatting: highlight profit
    wb$add_conditional_formatting(
      sheet = "Executive Summary",
      dims = "B7",
      rule = "Gross Profit",
      type = "containsText"
    )

    # Column widths
    wb$set_col_widths(sheet = "Executive Summary",
                      cols = 1:2, widths = c(25, 25))
  }

  # ===== Sheet 2: Sales Data =====
  if (!is.null(sales) && nrow(sales) > 0) {
    wb$add_worksheet("Sales Analysis")

    # Monthly summary
    monthly <- sales %>%
      dplyr::mutate(Month = format(date, "%Y-%m")) %>%
      dplyr::group_by(Month) %>%
      dplyr::summarise(
        Revenue = sum(total, na.rm = TRUE),
        Orders = dplyr::n(),
        Units = sum(quantity, na.rm = TRUE),
        `Avg Order Value` = round(Revenue / Orders, 2),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Month) %>%
      as.data.frame()

    wb$add_data(x = "Monthly Sales Summary", dims = "A1",
                sheet = "Sales Analysis")
    wb$add_font(dims = "A1", bold = TRUE, size = 14,
                color = openxlsx2::wb_color("#4f46e5"),
                sheet = "Sales Analysis")

    wb$add_data(x = monthly, dims = "A3", sheet = "Sales Analysis")

    # Style header
    n_cols <- ncol(monthly)
    header_range <- paste0("A3:", LETTERS[n_cols], "3")
    wb$add_fill(dims = header_range, color = openxlsx2::wb_color("#4f46e5"),
                sheet = "Sales Analysis")
    wb$add_font(dims = header_range, bold = TRUE,
                color = openxlsx2::wb_color("#FFFFFF"),
                sheet = "Sales Analysis")

    # Add data bars to Revenue column
    data_end <- nrow(monthly) + 3
    wb$add_conditional_formatting(
      sheet = "Sales Analysis",
      dims = paste0("B4:B", data_end),
      type = "dataBar"
    )

    wb$set_col_widths(sheet = "Sales Analysis",
                      cols = 1:n_cols, widths = c(15, 15, 10, 10, 15))

    # Top products
    top_start <- data_end + 3
    wb$add_data(x = "Top 10 Products", dims = paste0("A", top_start),
                sheet = "Sales Analysis")
    wb$add_font(dims = paste0("A", top_start), bold = TRUE, size = 12,
                color = openxlsx2::wb_color("#4f46e5"),
                sheet = "Sales Analysis")

    top_products <- calc_top_products(sales, 10) %>% as.data.frame()
    wb$add_data(x = top_products, dims = paste0("A", top_start + 2),
                sheet = "Sales Analysis")

    top_header <- paste0("A", top_start + 2, ":", LETTERS[ncol(top_products)],
                          top_start + 2)
    wb$add_fill(dims = top_header, color = openxlsx2::wb_color("#0891b2"),
                sheet = "Sales Analysis")
    wb$add_font(dims = top_header, bold = TRUE,
                color = openxlsx2::wb_color("#FFFFFF"),
                sheet = "Sales Analysis")
  }

  # ===== Sheet 3: Expenses =====
  if (!is.null(expenses) && nrow(expenses) > 0) {
    wb$add_worksheet("Expenses")

    exp_summary <- calc_expenses_by_category(expenses) %>% as.data.frame()

    wb$add_data(x = "Expense Breakdown by Category", dims = "A1",
                sheet = "Expenses")
    wb$add_font(dims = "A1", bold = TRUE, size = 14,
                color = openxlsx2::wb_color("#4f46e5"),
                sheet = "Expenses")

    wb$add_data(x = exp_summary, dims = "A3", sheet = "Expenses")

    exp_header <- paste0("A3:", LETTERS[ncol(exp_summary)], "3")
    wb$add_fill(dims = exp_header, color = openxlsx2::wb_color("#f59e0b"),
                sheet = "Expenses")
    wb$add_font(dims = exp_header, bold = TRUE,
                color = openxlsx2::wb_color("#000000"),
                sheet = "Expenses")

    wb$set_col_widths(sheet = "Expenses", cols = 1:ncol(exp_summary),
                      widths = c(20, 15, 10))
  }

  # ===== Sheet 4: Inventory =====
  if (!is.null(inventory) && nrow(inventory) > 0) {
    wb$add_worksheet("Inventory")

    inv_display <- inventory %>%
      dplyr::mutate(
        Status = dplyr::case_when(
          stock_quantity <= reorder_level * 0.5 ~ "CRITICAL",
          stock_quantity <= reorder_level ~ "LOW",
          stock_quantity <= reorder_level * 2 ~ "Normal",
          TRUE ~ "Overstocked"
        ),
        `Inventory Value` = round(stock_quantity * unit_cost, 2)
      ) %>%
      as.data.frame()

    wb$add_data(x = "Inventory Status", dims = "A1", sheet = "Inventory")
    wb$add_font(dims = "A1", bold = TRUE, size = 14,
                color = openxlsx2::wb_color("#4f46e5"),
                sheet = "Inventory")

    wb$add_data(x = inv_display, dims = "A3", sheet = "Inventory")

    inv_header <- paste0("A3:", LETTERS[min(ncol(inv_display), 26)], "3")
    wb$add_fill(dims = inv_header, color = openxlsx2::wb_color("#10b981"),
                sheet = "Inventory")
    wb$add_font(dims = inv_header, bold = TRUE,
                color = openxlsx2::wb_color("#FFFFFF"),
                sheet = "Inventory")
  }

  # ===== Sheet 5: Customer Summary =====
  if (!is.null(customers) && nrow(customers) > 0) {
    wb$add_worksheet("Customers")

    cust_summary <- customers %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(
        Customers = dplyr::n(),
        `Avg Total Purchases` = round(mean(total_purchases, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      as.data.frame()

    wb$add_data(x = "Customer Summary by Region", dims = "A1",
                sheet = "Customers")
    wb$add_font(dims = "A1", bold = TRUE, size = 14,
                color = openxlsx2::wb_color("#4f46e5"),
                sheet = "Customers")

    wb$add_data(x = cust_summary, dims = "A3", sheet = "Customers")

    cust_header <- paste0("A3:", LETTERS[ncol(cust_summary)], "3")
    wb$add_fill(dims = cust_header, color = openxlsx2::wb_color("#7c3aed"),
                sheet = "Customers")
    wb$add_font(dims = cust_header, bold = TRUE,
                color = openxlsx2::wb_color("#FFFFFF"),
                sheet = "Customers")
  }

  # Save
  wb$save(output_path, overwrite = TRUE)
  output_path
}
