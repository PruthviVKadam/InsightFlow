# InsightFlow

### AI-Powered Business Performance & Forecasting Platform

> A decision-support system that transforms raw business data into forecasts, KPIs, automated reports, and actionable recommendations.

---

## 🚀 Features

| Feature | Description |
|:---|:---|
| **Excel Data Import** | Upload Sales, Inventory, Expenses, Customers `.xlsx` files with auto-detection and cleaning |
| **KPI Dashboard** | 8 real-time KPI value boxes: Revenue, Profit, Margin, Orders, Growth, AOV, Customers, Burn Rate |
| **Interactive Charts** | Revenue trends, category breakdown, top products, expense analysis (Plotly) |
| **Forecasting Engine** | ARIMA, ETS, and XGBoost with automatic model comparison and selection |
| **Customer Segmentation** | RFM analysis + K-Means clustering with business-labeled segments |
| **Anomaly Detection** | Z-Score, STL Decomposition, and Isolation Forest methods |
| **Scenario Simulator** | What-if analysis with 5 adjustable parameters and instant projected impact |
| **Excel Reports** | Multi-sheet styled workbook with conditional formatting and data bars |
| **PDF Reports** | Professional executive report with charts, KPIs, and recommendations |
| **Automated Insights** | Natural-language business insights generated from KPI analysis |

---

## 📋 Prerequisites

- **R** >= 4.2.0
- **RStudio** (recommended) or any R IDE
- **Rtools** (Windows only — required for some package compilation)

---

## ⚙️ Installation

### 1. Clone or download the project

```bash
git clone <repository-url>
cd InsightFlow
```

### 2. Install R packages

Open R/RStudio in the `InsightFlow/` directory and run:

```r
source("install_packages.R")
```

This installs all ~35 required packages grouped by purpose.

### 3. Generate sample data

```r
setwd("InsightFlow")
source("scripts/generate_sample_data.R")
```

Creates realistic synthetic data in `data/sample/`:
- `Sales.xlsx` (~2,000 rows)
- `Inventory.xlsx` (50 products)
- `Expenses.xlsx` (500 entries)
- `Customers.xlsx` (500 customers)

---

## 🎮 Running the Application

```r
setwd("InsightFlow")
shiny::runApp()
```

Or in RStudio, open `app.R` and click **Run App**.

### First-time setup:
1. Navigate to **More → Data Upload**
2. Click **Use Sample Data** or upload your own `.xlsx` files
3. Explore the dashboard, run forecasts, and generate reports

---

## 📁 Project Structure

```
InsightFlow/
├── app.R                          # Main application entry point
├── R/                             # Modules and utilities
│   ├── mod_dashboard.R            # KPI overview dashboard
│   ├── mod_sales.R                # Sales analysis with filters
│   ├── mod_customers.R            # Customer segmentation (RFM + K-Means)
│   ├── mod_forecast.R             # Multi-model forecasting
│   ├── mod_inventory.R            # Stock management & alerts
│   ├── mod_anomalies.R            # Anomaly detection
│   ├── mod_simulator.R            # Scenario what-if analysis
│   ├── mod_reports.R              # Excel & PDF report generation
│   ├── mod_upload.R               # Data upload & cleaning
│   ├── mod_settings.R             # App settings & data management
│   ├── utils_cleaning.R           # Data cleaning functions
│   ├── utils_kpi.R                # KPI calculations
│   ├── utils_forecast.R           # Forecasting pipeline
│   ├── utils_segmentation.R       # RFM + clustering
│   ├── utils_anomaly.R            # Anomaly detection algorithms
│   ├── utils_insights.R           # Auto-generated text insights
│   ├── utils_excel_export.R       # Styled Excel report builder
│   └── utils_db.R                 # SQLite database helpers
├── www/styles.css                 # Premium dark-theme CSS
├── reports/executive_report.qmd   # Quarto PDF template
├── scripts/generate_sample_data.R # Sample data generator
├── data/sample/                   # Demo datasets
├── config/config.yml              # Application configuration
├── tests/testthat/                # Unit tests
├── DESCRIPTION                    # Package metadata
└── install_packages.R             # One-click dependency installer
```

---

## 🛠 Tech Stack

| Layer | Technology |
|:---|:---|
| **Language** | R |
| **Dashboard** | Shiny + bslib (Bootstrap 5) |
| **Visualization** | Plotly, ggplot2 |
| **Data Tables** | DT |
| **Data Wrangling** | dplyr, tidyr, lubridate, janitor |
| **Database** | SQLite (DBI + RSQLite) |
| **Forecasting** | forecast (ARIMA/ETS), xgboost |
| **Machine Learning** | cluster, factoextra, solitude |
| **Excel I/O** | readxl, openxlsx2, mschart |
| **PDF Reports** | Quarto / R Markdown |
| **Theme** | Custom CSS + Inter font |

---

## 🧪 Running Tests

```r
setwd("InsightFlow")
testthat::test_dir("tests/testthat")
```

---

## 📊 Architecture

```
Excel Files (.xlsx)
        ↓
  Data Cleaning (janitor, dplyr)
        ↓
  SQLite Database
        ↓
  ┌─────────────────────────────────┐
  │  Analytics Engine               │
  │  ├── KPI Calculator             │
  │  ├── Forecast Engine            │
  │  │   ├── ARIMA                  │
  │  │   ├── ETS                    │
  │  │   └── XGBoost                │
  │  ├── Customer Segmentation      │
  │  │   ├── RFM Analysis           │
  │  │   └── K-Means Clustering     │
  │  ├── Anomaly Detection          │
  │  │   ├── Z-Score                │
  │  │   ├── STL Decomposition      │
  │  │   └── Isolation Forest       │
  │  └── Insight Generator          │
  └─────────────────────────────────┘
        ↓
  ┌─────────────────────────────────┐
  │  Shiny Dashboard (bslib)        │
  │  ├── Dashboard (KPIs + Charts)  │
  │  ├── Sales Analysis             │
  │  ├── Customer Segments          │
  │  ├── Forecast                   │
  │  ├── Inventory                  │
  │  ├── Anomalies                  │
  │  ├── Scenario Simulator         │
  │  └── Reports                    │
  └─────────────────────────────────┘
        ↓
  ┌─────────────────────────────────┐
  │  Exports                        │
  │  ├── Excel Report (openxlsx2)   │
  │  └── PDF Report (Quarto)        │
  └─────────────────────────────────┘
```

---

## 📈 Skills Demonstrated

- Statistical computing in R
- Data wrangling and ETL pipelines
- Interactive dashboard development (Shiny + bslib)
- Time-series forecasting (ARIMA, ETS, XGBoost)
- Machine learning workflows (K-Means, Isolation Forest)
- Excel automation and professional reporting
- Data visualization (Plotly, ggplot2)
- Business analytics and KPI design
- Reproducible reporting with Quarto/R Markdown
- Software engineering (modular architecture, testing, documentation)
- Database management (SQLite)

---

## 📝 License

MIT License

---

*Built with ❤️ using R and Shiny*
