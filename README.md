# Portfolio Intelligence Lab

**A Shiny workspace for self-directed investors who want to stress-test a portfolio—weights, benchmark, and risk—in one place before they commit to a story about “why this allocation.”**

*GitHub:* [nakim12/investment-performance-tracker](https://github.com/nakim12/investment-performance-tracker) — the product direction in the roadmap uses the name **Portfolio Intelligence Lab**; the repo title may still say “Investment Performance Tracker.”

---

## What is it?

**Portfolio Intelligence Lab** is a Shiny app in the browser. You define a **weighted portfolio** (tickers and weights, or a **sample template**), choose a **benchmark** such as SPY or QQQ, and pull a shared history of adjusted prices from Yahoo Finance. On top of that data you get **Diagnosis** and **Performance** for portfolio-level risk, return, and drawdowns versus the benchmark, plus **Price Trend**, **Forecast** (exploratory), and **Risk Analysis** when you want more detail on individual holdings and how they move together.

It is built for questions like: *How does this mix behave versus a simple passive alternative, and where are the pressure points—concentration, correlation, tail risk?* Start from **Build Portfolio**, then use **Diagnosis** and **Performance** as the main portfolio story.

---

## What problem does this solve?

Brokerage apps show **positions** and **P&L**. They rarely help you **compare your actual allocation** to a baseline, **see portfolio-level drawdowns and correlations**, or **replay how a basket would have behaved** over a chosen history. This project is a **research sandbox**: you supply the thesis (tickers + weights), and the app returns transparent metrics and charts—**educational tooling, not financial advice.**

---

## How the session flows

1. **Define** — Tickers, weights (normalized to 100%), optional sample templates, benchmark (SPY, QQQ, VTI, DIA).
2. **Load** — Daily adjusted prices via Yahoo Finance (`quantmod`); you pick the analysis window on the app.
3. **Diagnose** — Portfolio-level risk/return, drawdowns, rolling metrics, concentration and correlation context vs. the benchmark.
4. **Explore** — Performance (cumulative and return mechanics), optional ARIMA-style **Forecast** (exploratory, not a trading signal), and **Risk Analysis** for per-holding and cross-holding views.

Everything stays in one Shiny session so you are not jumping between spreadsheets and disconnected chart tools.

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Shiny client (browser)                                      │
│  Navbar: Home · Build Portfolio · Diagnosis · Price Trend · │
│          Performance · Forecast · Risk Analysis              │
└─────────────────────────────┬───────────────────────────────┘
                              │ reactive inputs + outputs
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  app.R + R/ modules                                          │
│  config · helpers · portfolio analytics · landing UI · theme│
└─────────────────────────────┬───────────────────────────────┘
                              │ getSymbols / merges / stats
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  Market data (Yahoo Finance, via quantmod)                     │
└─────────────────────────────────────────────────────────────┘
```

---

## Tech stack

| Layer | Technology |
|-------|------------|
| App runtime | [R](https://www.r-project.org/) + [Shiny](https://shiny.posit.co/) |
| Data | `quantmod`, `zoo` |
| Analytics & viz | `dplyr`, `tidyr`, `ggplot2`, `scales`, `reshape2`, `forecast` |
| UX | `shinycssloaders` |
| CI deploy | GitHub Actions → [shinyapps.io](https://www.shinyapps.io/) (`rsconnect`) |
| Static sibling | `Investment Performance Tracker.Rmd` (original report-style analysis; not required to run the app) |

Dependencies for local runs and CI are declared in `DESCRIPTION`.

---

## Repository layout

```
Investment-Performance-Tracker/
├── app.R                         # Main Shiny UI + server
├── DESCRIPTION                   # Package imports (used by Actions for deps)
├── R/
│   ├── config.R                  # Sectors, samples, benchmarks, theme
│   ├── helpers.R
│   ├── portfolio.R               # Portfolio-level metrics & diagnosis
│   ├── landing_ui.R              # Home / landing experience
│   └── app_theme.R               # Shared CSS
├── scripts/
│   └── deploy_shinyapps.R        # CI / manual publish helper
├── .github/workflows/            # Deploy workflow
├── Investment Performance Tracker.Rmd
└── Investment Performance Tracker.Rproj
```

---

## Run locally

**Prerequisites:** R 4.x, RStudio optional.

1. Install dependencies (from R, at the project root):

   ```r
   install.packages(c(
     "shiny", "shinycssloaders", "quantmod", "zoo", "forecast",
     "tidyr", "scales", "reshape2", "dplyr", "ggplot2"
   ))
   ```

   Or use `pak::local_install_dev_deps()` / `remotes::install_deps()` if you treat the folder as a package.

2. Launch the app:

   ```r
   shiny::runApp()
   ```

   Or open `app.R` in RStudio and click **Run App**.

---

## Live demo

**[Open the hosted app on shinyapps.io](https://nathankim.shinyapps.io/investment-performance-tracker/)**

---

## Deploy from GitHub

Pushing to `main` can auto-deploy when app-related paths change. Secrets and manual runs are documented in [`.github/DEPLOY_SHINYAPPS.md`](.github/DEPLOY_SHINYAPPS.md).

---

## License & disclaimer

MIT — see repository license if present.

**Educational use only.** Nothing here is investment, tax, or legal advice. Past performance and backtests do not guarantee future results. You are responsible for your own decisions.

---

## Author

**Nathan Kim**  
[GitHub](https://github.com/nakim12) · [LinkedIn](https://linkedin.com/in/kim-a-nathan)
