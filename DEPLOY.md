# Deploying InsightFlow

InsightFlow deploys to [shinyapps.io](https://www.shinyapps.io) — Posit's hosted
Shiny service, which has a free tier suitable for a portfolio demo.

## One-time account setup

1. Create a free account at https://www.shinyapps.io (sign in with Google or GitHub).
2. In the dashboard, open **Account → Tokens → Show**, and copy the
   `rsconnect::setAccountInfo(...)` snippet shown there.
3. Paste and run that snippet once in an R session. It saves your token and
   secret to your local user config — they are **never** committed to this repo.

## Deploy

From the `InsightFlow/` directory:

```r
Rscript deploy.R
```

or, inside R:

```r
source("deploy.R")
```

The first deploy compiles the dependency bundle on shinyapps.io and can take
10–30 minutes because of the heavier packages (xgboost, forecast, tidyverse).
Later deploys are much faster. On success, rsconnect prints the live URL
(`https://<account>.shinyapps.io/insightflow/`).

## What gets bundled

`deploy.R` uploads only the files the app needs at runtime:

- `app.R`, everything in `R/`, `www/`, `config/config.yml`, `reports/`
- `DESCRIPTION` (so the dependency set is explicit)
- `data/sample/*.xlsx` — so the live app's **Use Sample Data** button works

Excluded: the local `insightflow.db` (recreated on the server), `tests/`,
`data/raw`, `data/clean`, `exports/`, and IDE files.

## First run on the live app

The app starts with an empty database. On the deployed site:

1. Go to **More → Data Upload**.
2. Click **Use Sample Data**.
3. The dashboard, forecasts, segments, and reports populate.

## Notes

- **Free-tier limits:** 5 apps and 25 active hours/month. The app sleeps when
  idle and wakes on the next visit.
- **PDF reports** rely on Quarto being available on the server; the Excel report
  and all in-app analytics work regardless.
