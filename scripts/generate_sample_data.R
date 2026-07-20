# ============================================================================
# InsightFlow — Sample Data Generator
# Creates realistic synthetic business data for demonstration.
# Run once: source("scripts/generate_sample_data.R")
# ============================================================================

library(openxlsx2)
library(lubridate)

set.seed(42)
cat("Generating sample business data...\n")

output_dir <- file.path("data", "sample")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Helper data ---
products <- data.frame(
  product_id = sprintf("P%03d", 1:50),
  product = c(
    "Wireless Mouse", "Mechanical Keyboard", "USB-C Hub", "Webcam HD",
    "Monitor Stand", "Laptop Sleeve", "Noise-Cancel Headphones", "Desk Lamp",
    "Ergonomic Chair", "Standing Desk Mat", "Cable Organizer", "Phone Charger",
    "Bluetooth Speaker", "Portable SSD", "Screen Protector", "Mouse Pad XL",
    "USB Flash Drive", "HDMI Cable", "Power Strip", "Desk Fan",
    "Tablet Stand", "Stylus Pen", "Laptop Backpack", "Ring Light",
    "Wi-Fi Extender", "Smart Plug", "Action Camera", "Drone Mini",
    "VR Headset", "Gaming Controller", "Drawing Tablet", "Mic Arm",
    "Pop Filter", "Audio Interface", "Studio Monitors", "Capture Card",
    "Stream Deck", "Green Screen", "Tripod", "Gimbal Stabilizer",
    "LED Strip Lights", "Desk Shelf", "Footrest", "Wrist Rest",
    "Blue Light Glasses", "Desk Clock", "Whiteboard", "Sticky Notes Pack",
    "Desk Organizer", "Water Bottle"
  ),
  category = rep(c("Accessories", "Peripherals", "Audio", "Office",
                    "Gaming", "Streaming", "Wellness", "Productivity",
                    "Smart Home", "Photography"), each = 5),
  unit_price = round(c(
    29.99, 89.99, 49.99, 69.99, 39.99,
    34.99, 199.99, 44.99, 449.99, 29.99,
    14.99, 24.99, 79.99, 119.99, 12.99,
    19.99, 15.99, 9.99, 29.99, 22.99,
    24.99, 39.99, 79.99, 54.99, 44.99,
    29.99, 249.99, 399.99, 299.99, 59.99,
    189.99, 34.99, 14.99, 149.99, 349.99,
    179.99, 249.99, 89.99, 44.99, 199.99,
    34.99, 49.99, 39.99, 19.99, 24.99,
    29.99, 39.99, 9.99, 34.99, 24.99
  ), 2),
  unit_cost = round(c(
    12.00, 38.00, 22.00, 28.00, 16.00,
    14.00, 78.00, 18.00, 180.00, 12.00,
    5.00, 8.00, 30.00, 48.00, 4.00,
    7.00, 5.00, 3.00, 11.00, 9.00,
    10.00, 16.00, 32.00, 22.00, 18.00,
    12.00, 100.00, 160.00, 120.00, 24.00,
    76.00, 14.00, 5.00, 60.00, 140.00,
    72.00, 100.00, 36.00, 18.00, 80.00,
    14.00, 20.00, 16.00, 8.00, 10.00,
    12.00, 16.00, 3.00, 14.00, 10.00
  ), 2),
  stringsAsFactors = FALSE
)

regions <- c("Northeast", "Southeast", "Midwest", "West", "Southwest")
channels <- c("Online", "Retail", "Wholesale", "Marketplace")
customer_segments <- c("Consumer", "Small Business", "Enterprise")

# --- 1. SALES DATA (2000 rows, 2 years) ---
cat("  → Sales.xlsx\n")

start_date <- as.Date("2024-01-01")
end_date   <- as.Date("2025-12-31")
n_sales <- 2000

dates <- sample(seq(start_date, end_date, by = "day"), n_sales, replace = TRUE)
dates <- sort(dates)

# Add seasonality: more orders in Nov-Dec and summer
month_weights <- c(0.7, 0.6, 0.8, 0.9, 1.0, 1.1, 1.0, 1.1, 1.0, 0.9, 1.3, 1.5)
seasonal_adj <- month_weights[month(dates)]

prod_idx <- sample(1:50, n_sales, replace = TRUE,
                    prob = c(rep(0.03, 10), rep(0.02, 20), rep(0.015, 20)))
quantities <- pmax(1, round(rpois(n_sales, 2) * seasonal_adj))

sales <- data.frame(
  order_id    = sprintf("ORD-%05d", 1:n_sales),
  date        = format(dates, "%Y-%m-%d"),
  product     = products$product[prod_idx],
  category    = products$category[prod_idx],
  quantity    = quantities,
  unit_price  = products$unit_price[prod_idx],
  total       = round(quantities * products$unit_price[prod_idx], 2),
  customer_id = sprintf("C%04d", sample(1:500, n_sales, replace = TRUE)),
  region      = sample(regions, n_sales, replace = TRUE,
                        prob = c(0.25, 0.20, 0.20, 0.25, 0.10)),
  channel     = sample(channels, n_sales, replace = TRUE,
                        prob = c(0.45, 0.25, 0.15, 0.15)),
  stringsAsFactors = FALSE
)

# Inject some data quality issues for cleaning demos
sales$date[sample(1:n_sales, 5)]   <- NA
sales$total[sample(1:n_sales, 8)]  <- NA
dup_rows <- sales[sample(1:n_sales, 10), ]
sales <- rbind(sales, dup_rows)

wb_sales <- wb_workbook()$
  add_worksheet("Sales")$
  add_data(x = sales)
wb_save(wb_sales, file.path(output_dir, "Sales.xlsx"), overwrite = TRUE)

# --- 2. INVENTORY DATA (50 products) ---
cat("  → Inventory.xlsx\n")

inventory <- data.frame(
  product_id      = products$product_id,
  product         = products$product,
  category        = products$category,
  stock_quantity  = sample(10:500, 50, replace = TRUE),
  reorder_level   = sample(20:80, 50, replace = TRUE),
  unit_cost       = products$unit_cost,
  warehouse       = sample(c("Warehouse A", "Warehouse B", "Warehouse C"),
                           50, replace = TRUE),
  last_restocked  = format(sample(seq(as.Date("2025-06-01"),
                                       as.Date("2025-12-15"), by = "day"),
                                   50, replace = TRUE), "%Y-%m-%d"),
  stringsAsFactors = FALSE
)

# Make some items critically low
low_stock_idx <- sample(1:50, 8)
inventory$stock_quantity[low_stock_idx] <- sample(1:15, 8, replace = TRUE)

wb_inv <- wb_workbook()$
  add_worksheet("Inventory")$
  add_data(x = inventory)
wb_save(wb_inv, file.path(output_dir, "Inventory.xlsx"), overwrite = TRUE)

# --- 3. EXPENSES DATA (500 rows) ---
cat("  → Expenses.xlsx\n")

expense_categories <- c("Marketing", "Operations", "Salaries", "Rent",
                         "Software", "Utilities", "Travel", "Equipment",
                         "Insurance", "Miscellaneous")
departments <- c("Sales", "Engineering", "Marketing", "Operations", "HR",
                  "Finance")

exp_dates <- sample(seq(start_date, end_date, by = "day"), 500, replace = TRUE)
exp_dates <- sort(exp_dates)

expenses <- data.frame(
  date        = format(exp_dates, "%Y-%m-%d"),
  category    = sample(expense_categories, 500, replace = TRUE,
                        prob = c(0.18, 0.15, 0.22, 0.10, 0.08, 0.06,
                                 0.05, 0.07, 0.04, 0.05)),
  amount      = round(runif(500, 100, 15000), 2),
  description = paste("Expense for",
                       sample(c("Q1 campaign", "office supplies", "cloud hosting",
                                "team event", "contractor", "software license",
                                "travel reimbursement", "equipment upgrade",
                                "insurance premium", "maintenance"),
                              500, replace = TRUE)),
  department  = sample(departments, 500, replace = TRUE),
  stringsAsFactors = FALSE
)

# Make salaries higher
salary_idx <- which(expenses$category == "Salaries")
expenses$amount[salary_idx] <- round(runif(length(salary_idx), 4000, 15000), 2)

# Make rent consistent monthly
rent_idx <- which(expenses$category == "Rent")
expenses$amount[rent_idx] <- 8500

wb_exp <- wb_workbook()$
  add_worksheet("Expenses")$
  add_data(x = expenses)
wb_save(wb_exp, file.path(output_dir, "Expenses.xlsx"), overwrite = TRUE)

# --- 4. CUSTOMERS DATA (500 rows) ---
cat("  → Customers.xlsx\n")

first_names <- c("James", "Mary", "Robert", "Patricia", "John", "Jennifer",
                  "Michael", "Linda", "David", "Elizabeth", "William", "Barbara",
                  "Richard", "Susan", "Joseph", "Jessica", "Thomas", "Sarah",
                  "Christopher", "Karen", "Daniel", "Lisa", "Matthew", "Nancy",
                  "Anthony", "Betty", "Mark", "Sandra", "Donald", "Margaret",
                  "Steven", "Ashley", "Paul", "Dorothy", "Andrew", "Kimberly",
                  "Joshua", "Emily", "Kenneth", "Donna", "Kevin", "Michelle",
                  "Brian", "Carol", "George", "Amanda", "Timothy", "Melissa",
                  "Ronald", "Deborah")
last_names <- c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia",
                 "Miller", "Davis", "Rodriguez", "Martinez", "Hernandez",
                 "Lopez", "Gonzalez", "Wilson", "Anderson", "Thomas", "Taylor",
                 "Moore", "Jackson", "Martin", "Lee", "Perez", "Thompson",
                 "White", "Harris")

cust_n <- 500
cust_join <- sample(seq(as.Date("2022-01-01"), as.Date("2025-12-01"),
                         by = "day"), cust_n, replace = TRUE)

customers <- data.frame(
  customer_id      = sprintf("C%04d", 1:cust_n),
  name             = paste(sample(first_names, cust_n, replace = TRUE),
                           sample(last_names, cust_n, replace = TRUE)),
  email            = paste0(tolower(paste0(
                       substr(sample(first_names, cust_n, replace = TRUE), 1, 1),
                       sample(last_names, cust_n, replace = TRUE),
                       sample(10:99, cust_n, replace = TRUE)
                     )), "@",
                     sample(c("gmail.com", "yahoo.com", "outlook.com",
                              "company.com", "mail.com"),
                            cust_n, replace = TRUE)),
  join_date        = format(cust_join, "%Y-%m-%d"),
  region           = sample(regions, cust_n, replace = TRUE),
  segment          = sample(customer_segments, cust_n, replace = TRUE,
                             prob = c(0.55, 0.30, 0.15)),
  total_purchases  = round(rlnorm(cust_n, meanlog = 5.5, sdlog = 1.2), 2),
  last_purchase_date = format(
    pmin(as.Date("2025-12-31"),
         cust_join + sample(30:1200, cust_n, replace = TRUE)),
    "%Y-%m-%d"
  ),
  stringsAsFactors = FALSE
)

# Inject a few missing emails
customers$email[sample(1:cust_n, 5)] <- NA

wb_cust <- wb_workbook()$
  add_worksheet("Customers")$
  add_data(x = customers)
wb_save(wb_cust, file.path(output_dir, "Customers.xlsx"), overwrite = TRUE)

cat("\n✓ Sample data generated in:", normalizePath(output_dir), "\n")
cat("  - Sales.xlsx      (", nrow(sales), "rows )\n")
cat("  - Inventory.xlsx  (", nrow(inventory), "rows )\n")
cat("  - Expenses.xlsx   (", nrow(expenses), "rows )\n")
cat("  - Customers.xlsx  (", nrow(customers), "rows )\n")
