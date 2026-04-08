# Helm

**A Shiny workspace for self-directed investors who want to stress-test a portfolio—weights, benchmark, and risk—in one place before they commit to a story about “why this allocation.”**

*GitHub:* [nakim12/investment-performance-tracker](https://github.com/nakim12/investment-performance-tracker) — repo folder name may still say “Investment Performance Tracker”; the live product name is **Helm**.

---

## What is it?

**Helm** is a Shiny app in the browser. You define a **weighted portfolio** (tickers and weights, or a **sample template**), choose a **benchmark** such as SPY or QQQ, and pull a shared history of adjusted prices from Yahoo Finance. On top of that data you get **Diagnosis** (KPIs, insights, sector and return attribution, CSV/text exports) and **Performance** (your portfolio vs the benchmark plus per-ticker exploration). **Scenarios** replays preset stress windows, applies an optional mechanical daily return shock, and can draw a **bootstrap fan** from historical portfolio returns. **Allocation Lab** suggests alternative long-only weights (min-variance with optional caps, inverse vol, equal weight, projected max Sharpe) on the same window. **Methodology** documents definitions and limits. **Holdings Explorer** groups **Price Trend** and exploratory **Forecast** views; **Risk Analysis** adds deeper single-name risk metrics.

It is built for questions like: *How does this mix behave versus a simple passive alternative, where are the pressure points, and what happens in rough historical patches?* Start from **Build Portfolio**, then use **Diagnosis**, **Performance**, **Scenarios** (including optional bootstrap fan), and **Allocation Lab** for alternative weighting ideas on the same data window.

---

## What problem does this solve?

Brokerage apps show **positions** and **P&L**. They rarely help you **compare your actual allocation** to a baseline, **see portfolio-level drawdowns and correlations**, or **replay how a basket would have behaved** over a chosen history. This project is a **research sandbox**: you supply the thesis (tickers + weights), and the app returns transparent metrics and charts—**educational tooling, not financial advice.**

---

## How the session flows

1. **Define** — Tickers, weights (normalized to 100%), optional sample templates, benchmark (SPY, QQQ, VTI, DIA). You can **download or load a portfolio JSON** snapshot (weights, benchmark, dates) to reuse or share a setup.
2. **Load** — Daily adjusted prices via Yahoo Finance (`quantmod`); you pick the analysis window on the app.
3. **Diagnose** — Portfolio-level risk/return, drawdowns, rolling metrics, concentration, correlations, holding and sector attribution, plain-language insights, and optional “what to consider next” prompts; export **.txt**, **.csv**, or a **full report (.zip)**. **Pins** (session-only) let you compare the headline metrics table before and after you change the portfolio.
4. **Performance, scenarios & allocation** — Cumulative paths vs benchmark; **Scenarios** for historical episodes, bps stress, and bootstrap fan; **Allocation Lab** for model weights vs yours.
5. **Explore** — **Methodology** for formulas; **Holdings Explorer** (price trends and exploratory forecasts) and **Risk Analysis** for per-ticker views.

Everything stays in one Shiny session so you are not jumping between spreadsheets and disconnected chart tools.

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Shiny client (browser)                                      │
│  Navbar: Home · Build · Diagnosis · Holdings Explorer · Performance · │
│          Scenarios · Allocation Lab · Risk · Methodology │
└─────────────────────────────┬───────────────────────────────┘
                              │ reactive inputs + outputs
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  app.R (entry) + R/ modules                                    │
│  global.R loads: config · helpers · theme · portfolio · snapshots · │
│  scenarios · allocation · landing · methodology · app_ui · app_server │
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
| Analytics & viz | `dplyr`, `tidyr`, `ggplot2`, `scales`, `reshape2`, `forecast`, `quadprog` |
| UX | `shinycssloaders` |
| CI deploy | GitHub Actions → [shinyapps.io](https://www.shinyapps.io/) (`rsconnect`) |
| Static sibling | `Investment Performance Tracker.Rmd` (original report-style analysis; not required to run the app) |

Dependencies for local runs and CI are declared in `DESCRIPTION`.

---

## Repository layout

```
Investment-Performance-Tracker/
├── app.R                         # Shiny entry (calls app_ui / app_server from R/)
├── global.R                      # Sources all modules in R/ (runs before app.R)
├── DESCRIPTION                   # Package imports (used by Actions for deps)
├── R/
│   ├── config.R                  # Sectors, samples, benchmarks, ggplot theme
│   ├── helpers.R
│   ├── portfolio.R               # Portfolio metrics, attribution, insights
│   ├── snapshots.R               # JSON snapshots, diagnosis pin compare helpers
│   ├── scenarios.R               # Stress presets & shock helpers
│   ├── allocation.R              # Allocation Lab optimizers (quadprog, heuristics)
│   ├── landing_ui.R              # Home / landing experience
│   ├── methodology_ui.R          # Methodology tab copy
│   ├── app_ui.R                  # navbarPage UI definition
│   ├── app_server.R              # Main server function
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
     "tidyr", "scales", "reshape2", "dplyr", "ggplot2", "quadprog", "jsonlite"
   ))
   ```

   Or use `pak::local_install_dev_deps()` / `remotes::install_deps()` if you treat the folder as a package.

2. Launch the app:

   ```r
   shiny::runApp()
   ```

   Or open `app.R` in RStudio and click **Run App**. Keep the working directory at the project root so **`global.R`** loads the `R/` modules before **`app.R`**.

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
