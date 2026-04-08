# Main Shiny UI (navbar). Depends on GLOBAL_APP_CSS, landing_panel(), methodology_panel().

app_ui <- function() {
  app_css <- "
.kpi-card {
  text-align: center;
  padding: 18px 10px;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 10px;
  margin-bottom: 15px;
  border: 1px solid rgba(255, 255, 255, 0.1);
}
.kpi-value {
  font-size: 22px;
  font-weight: 700;
  margin-bottom: 4px;
}
.kpi-label {
  font-size: 11px;
  color: #94a3b8;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}
.kpi-positive { color: #4ade80; }
.kpi-negative { color: #f87171; }
.kpi-neutral  { color: #cbd5e1; }
.insight-panel {
  background: rgba(34, 211, 238, 0.08);
  border-left: 4px solid #22d3ee;
  padding: 15px 20px;
  border-radius: 8px;
  margin-top: 15px;
  color: #e2e8f0;
}
.insight-panel li {
  margin-bottom: 8px;
  line-height: 1.6;
}
.builder-header {
  padding: 10px 0 20px 0;
}
.empty-state {
  text-align: center;
  padding: 60px 20px;
  color: #64748b;
}
.weight-ok    { color: #4ade80; font-weight: bold; }
.weight-warn  { color: #fbbf24; font-weight: bold; }
.insight-card {
  border-radius: 10px;
  padding: 14px 16px;
  margin-bottom: 12px;
  min-height: 88px;
  border: 1px solid rgba(255, 255, 255, 0.12);
  font-size: 14px;
  line-height: 1.5;
  color: #e2e8f0;
}
.insight-card--neutral {
  background: rgba(148, 163, 184, 0.12);
  border-left: 4px solid #94a3b8;
}
.insight-card--ok {
  background: rgba(74, 222, 128, 0.1);
  border-left: 4px solid #4ade80;
}
.insight-card--warn {
  background: rgba(248, 113, 113, 0.12);
  border-left: 4px solid #f87171;
}
.insight-card--action {
  background: rgba(34, 211, 238, 0.1);
  border-left: 4px solid #22d3ee;
}
.diagnosis-downloads .btn { margin-left: 6px; margin-bottom: 6px; }
  "
  navbarPage(
  title = "Helm",
  id    = "main_nav",
  header = tags$head(
    tags$style(HTML(paste(app_css, GLOBAL_APP_CSS, sep = "\n"))),
    landing_head_extras()
  ),

  # ── Tab 1: Home (landing) ───────────────────────────────────────────────────

  tabPanel("Home", landing_panel()),

  # ── Tab 2: Build Portfolio ──────────────────────────────────────────────────

  tabPanel("Build Portfolio",
    div(class = "builder-header",
      h3("Build Your Portfolio"),
      tags$p(class = "text-muted",
        "Define holdings and weights, select a benchmark, then analyze to see",
        "risk, return, and performance diagnostics."
      )
    ),
    fluidRow(
      column(4,
        wellPanel(
          h4("Add Holdings"),
          selectizeInput("portfolio_ticker_input", "Ticker Symbol:",
            choices  = stock_sectors,
            options  = list(create = TRUE,
                            placeholder = "Type or select a ticker...")
          ),
          numericInput("portfolio_weight_input", "Weight (%):",
            value = 20, min = 0.1, max = 100, step = 0.5),
          actionButton("add_holding", "Add to Portfolio",
            class = "btn-primary btn-sm", style = "margin-bottom: 15px;"),
          hr(),

          h4("Quick Start"),
          selectInput("sample_portfolio", "Load a Sample Portfolio:",
            choices = c("Choose..." = "", names(sample_portfolios))),
          actionButton("load_sample", "Load Sample",
            class = "btn-info btn-sm"),
          hr(),

          h4("Benchmark"),
          selectInput("benchmark_ticker", "Compare Against:",
            choices = benchmark_choices, selected = "SPY"),
          hr(),

          dateRangeInput("portfolio_date_range", "Analysis Period:",
            start = Sys.Date() - 365,
            end   = Sys.Date(),
            max   = Sys.Date()),
          hr(),

          actionButton("analyze_portfolio", "Analyze Portfolio",
            class = "btn-success btn-lg", style = "width: 100%;"),
          br(), br(),
          actionButton("clear_portfolio", "Clear All Holdings",
            class = "btn-warning btn-sm"),
          hr(),
          h4("Save & share"),
          textInput("portfolio_snapshot_label", "Snapshot label (optional)",
            placeholder = "e.g. Taxable account"),
          downloadButton("download_portfolio_snapshot", "Download portfolio (.json)",
            class = "btn-default btn-sm", style = "margin-top: 6px;"),
          helpText(style = "font-size: 11px;",
            "Exports weights, benchmark, and date range. Share the file or reload it here."),
          fileInput("portfolio_snapshot_upload", "Load portfolio snapshot",
            accept = c("application/json", ".json"),
            buttonLabel = "Choose JSON…", placeholder = "No file selected",
            width = "100%"),
          helpText(style = "font-size: 11px;",
            "After load, click ", tags$b("Analyze Portfolio"), " again to refresh metrics.")
        )
      ),

      column(8,
        h4("Current Holdings"),
        tableOutput("portfolio_holdings_table"),
        fluidRow(
          column(4,
            selectInput("remove_ticker_select", "Select holding to remove:",
              choices = NULL)
          ),
          column(4, style = "margin-top: 25px;",
            actionButton("remove_holding", "Remove Selected",
              class = "btn-danger btn-sm")
          )
        ),
        hr(),
        fluidRow(
          column(4, tags$div(class = "kpi-card",
            tags$div(class = "kpi-label", "Total Weight"),
            uiOutput("total_weight_display")
          )),
          column(4, tags$div(class = "kpi-card",
            tags$div(class = "kpi-label", "Holdings"),
            textOutput("num_holdings_display")
          )),
          column(4, tags$div(class = "kpi-card",
            tags$div(class = "kpi-label", "Concentration (HHI)"),
            textOutput("concentration_display")
          ))
        ),
        fluidRow(
          column(6, plotOutput("allocation_pie", height = "300px")),
          column(6, plotOutput("sector_pie",     height = "300px"))
        )
      )
    ),

    uiOutput("portfolio_results_section")
  ),

  # ── Tab 3: Diagnosis ───────────────────────────────────────────────────────

  tabPanel("Diagnosis",
    uiOutput("diagnosis_content")
  ),

  # ── Tab 4: Holdings Explorer (single-name: trend + forecast) ────────────────

  tabPanel("Holdings Explorer",
    tags$p(class = "text-muted",
      "Single-name charts and exploratory forecasts. For portfolio-level results use ",
      tags$b("Diagnosis"), ", ", tags$b("Performance"), ", and ", tags$b("Risk Analysis"), "."
    ),
    tabsetPanel(
      id = "holdings_explorer_tabs",
      tabPanel("Price Trend",
        sidebarLayout(
          sidebarPanel(
            selectInput("sector_filter", "Filter by Sector:",
              choices  = c("All Sectors" = "all", names(stock_sectors)),
              selected = "all"),
            selectInput("tickers", "Select Stocks to Compare:",
              choices  = NULL, selected = NULL, multiple = TRUE),
            actionButton("select_all", "Select All",
              class = "btn-sm btn-primary", style = "margin-bottom: 10px;"),
            actionButton("clear_all", "Clear All",
              class = "btn-sm btn-warning", style = "margin-bottom: 10px;"),
            sliderInput("ma_window", "Moving Average Window (days):",
              min = 5, max = 100, value = 20),
            hr(),
            helpText("Select stocks and adjust the moving average window to analyze price trends."),
            br(),
            tags$small(
              tags$b("Available Sectors:"),
              tags$ul(
                tags$li("Technology: FAANG + semiconductors"),
                tags$li("Financial: Banks & payment processors"),
                tags$li("Healthcare: Pharma & insurance"),
                tags$li("Consumer: Retail & restaurants"),
                tags$li("Energy: Oil & gas companies")
              )
            )
          ),
          mainPanel(
            withSpinner(plotOutput("pricePlot"), type = 4, color = "#22d3ee")
          )
        )
      ),
      tabPanel("Forecast",
        sidebarLayout(
          sidebarPanel(
            selectInput("forecast_sector", "Filter by Sector:",
              choices  = c("All Sectors" = "all", names(stock_sectors)),
              selected = "all"),
            selectInput("forecast_ticker", "Select Ticker to Forecast:",
              choices = NULL),
            sliderInput("horizon", "Forecast Horizon (days):",
              min = 5, max = 90, value = 30),
            hr(),
            radioButtons("forecast_mode", "Forecast Mode:",
              choices  = c("Conservative (Recommended)" = "conservative",
                           "Aggressive (Less Reliable)"  = "aggressive"),
              selected = "conservative"),
            helpText("Conservative mode requires more historical data for reliable forecasts."),
            hr(),
            textOutput("dataAvailability")
          ),
          mainPanel(
            withSpinner(plotOutput("forecastPlot"), type = 4, color = "#22d3ee"),
            br(),
            wellPanel(
              h4("Model Evaluation Metrics"),
              verbatimTextOutput("forecastMetrics")
            ),
            withSpinner(plotOutput("forecastAccuracyPlot"), type = 4, color = "#22d3ee")
          )
        )
      )
    )
  ),

  # ── Tab 5: Performance (daily + cumulative returns) ───────────────────────

  tabPanel("Performance",
    mainPanel(
      uiOutput("performance_portfolio_block"),
      hr(),
      h3("Per-ticker exploration"),
      tags$p(class = "text-muted",
        "Uses tickers selected under ", tags$b("Holdings Explorer"), " → ",
        tags$b("Price Trend"), " (last ~1 year of Yahoo data)."
      ),
      h4("Cumulative performance"),
      tags$p(class = "text-muted",
        "Growth of $1 invested over the loaded period."
      ),
      withSpinner(plotOutput("cumulativePlot", height = "400px"),
        type = 4, color = "#22d3ee"),
      hr(),
      h3("Daily returns"),
      withSpinner(plotOutput("returnsPlot", height = "380px"),
        type = 4, color = "#22d3ee"),
      hr(),
      h3("Return distributions"),
      withSpinner(plotOutput("returnDistribution", height = "400px"),
        type = 4, color = "#22d3ee"),
      hr(),
      h3("Performance summary"),
      withSpinner(tableOutput("performanceSummary"), type = 4, color = "#22d3ee")
    )
  ),

  # ── Tab 6: Scenarios & stress ───────────────────────────────────────────────

  tabPanel("Scenarios",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Historical episode"),
        selectInput("stress_preset", "Preset or custom",
          choices  = stress_preset_choices(),
          selected = "custom"),
        dateRangeInput("stress_custom_range", "Date range",
          start = Sys.Date() - 365, end = Sys.Date(), max = Sys.Date()),
        helpText("Choosing a preset fills these dates; you can edit them anytime."),
        hr(),
        h4("Additive stress (full sample)"),
        radioButtons("stress_scope", "Apply extra daily return to",
          choices = c("All holdings" = "all", "One sector" = "sector"),
          selected = "all"),
        conditionalPanel(
          condition = "input.stress_scope == 'sector'",
          selectInput("stress_sector_pick", "Sector",
            choices = names(stock_sectors), selected = names(stock_sectors)[[1]])
        ),
        sliderInput("stress_bps", "Extra daily return (basis points, every day)",
          min = -100, max = 100, value = 0, step = 5),
        helpText("Mechanical sensitivity only: same bps added to each affected holding’s daily return, then the portfolio is recomputed. ",
                 "0 bps = baseline path."),
        hr(),
        h4("Bootstrap fan (portfolio)"),
        sliderInput("stress_fan_horizon", "Forward horizon (trading days)",
          min = 5, max = 252, value = 63, step = 1),
        sliderInput("stress_fan_n_sims", "Number of simulated paths",
          min = 100, max = 3000, value = 800, step = 50),
        numericInput("stress_fan_seed", "Random seed", value = 1, min = 1, max = 99999, step = 1),
        helpText("Resamples historical portfolio daily returns with your fixed weights. ",
                 "Illustrative dispersion only—not a forecast of future regimes."),
        hr(),
        tags$p(class = "text-muted", style = "font-size: 12px;",
          "Run ", tags$b("Analyze Portfolio"), " on ",
          tags$b("Build Portfolio"), " first."
        )
      ),
      mainPanel(
        width = 9,
        h3("Historical episode (rebased to window start)"),
        withSpinner(plotOutput("stress_hist_plot", height = "300px"),
          type = 4, color = "#22d3ee"),
        tableOutput("stress_hist_table"),
        hr(),
        h3("Stressed vs baseline (full analysis period)"),
        withSpinner(plotOutput("stress_shock_plot", height = "340px"),
          type = 4, color = "#22d3ee"),
        tags$p(class = "text-muted",
          "Baseline uses your analyzed portfolio as computed on the ",
          tags$b("Diagnosis"), " tab. Stressed path applies the bps adjustment only to the scope you selected."
        ),
        hr(),
        h3("Bootstrap fan chart (fixed weights)"),
        withSpinner(plotOutput("stress_fan_plot", height = "320px"),
          type = 4, color = "#22d3ee"),
        tags$p(class = "text-muted",
          "Each path stacks independent draws from your portfolio’s realized daily returns; ",
          "the band is the 5th–95th percentile across simulations."
        )
      )
    )
  ),

  # ── Tab 7: Allocation Lab ───────────────────────────────────────────────────

  tabPanel("Allocation Lab",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Alternative weights"),
        selectInput("alloc_method", "Construction method",
          choices = c(
            "Min variance (long-only, optional cap)" = "minvar",
            "Inverse volatility" = "invvol",
            "Equal weight" = "equal",
            "Max Sharpe (projected long-only)" = "maxsharpe_proj"
          ),
          selected = "minvar"),
        conditionalPanel(
          condition = "input.alloc_method == 'minvar'",
          sliderInput("alloc_max_w_pct", "Max weight per name (%)",
            min = 5, max = 100, value = 100, step = 1),
          helpText("100% = no cap. Lower caps tighten concentration; the solver stays long-only and fully invested.")
        ),
        hr(),
        tags$p(class = "text-muted", style = "font-size: 12px;",
          "Uses the same price history as your last ", tags$b("Analyze Portfolio"), " run. ",
          "Compares your current weights to a model portfolio on that window."
        )
      ),
      mainPanel(
        width = 9,
        h3("Current vs proposed allocation"),
        uiOutput("alloc_lab_need_analyze"),
        fluidRow(
          column(7, withSpinner(tableOutput("alloc_weights_table"), type = 4, color = "#22d3ee")),
          column(5, withSpinner(tableOutput("alloc_metrics_table"), type = 4, color = "#22d3ee"))
        ),
        withSpinner(plotOutput("alloc_weights_plot", height = "360px"),
          type = 4, color = "#22d3ee"),
        tags$p(class = "text-muted",
          tags$b("Max Sharpe (projected)"), " solves the unconstrained tangency portfolio, ",
          "then clips negative weights and renormalizes—an approximation, not a constrained optimizer."
        )
      )
    )
  ),

  # ── Tab 8: Risk Analysis ────────────────────────────────────────────────────

  tabPanel("Risk Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("risk_sector", "Filter by Sector:",
          choices  = c("All Sectors" = "all", names(stock_sectors)),
          selected = "all"),
        selectInput("risk_tickers", "Select Stocks for Analysis:",
          choices = NULL, selected = NULL, multiple = TRUE),
        dateRangeInput("risk_date_range", "Analysis Period:",
          start = Sys.Date() - 365,
          end   = Sys.Date(),
          max   = Sys.Date()),
        hr(),
        helpText("Risk metrics help evaluate the volatility and risk-adjusted returns of your investments."),
        br(),
        h5("Metric Definitions:"),
        tags$small(
          tags$ul(
            tags$li(tags$b("Volatility:"), " Annual standard deviation of returns"),
            tags$li(tags$b("Sharpe Ratio:"), " Risk-adjusted return (higher is better)"),
            tags$li(tags$b("Max Drawdown:"), " Largest peak-to-trough decline"),
            tags$li(tags$b("VaR (95%):"), " Maximum expected daily loss with 95% confidence")
          )
        )
      ),
      mainPanel(
        h3("Risk Metrics Summary"),
        withSpinner(tableOutput("riskMetricsTable"), type = 4, color = "#22d3ee"),
        hr(),
        h3("Correlation Analysis"),
        withSpinner(plotOutput("correlationHeatmap", height = "400px"),
          type = 4, color = "#22d3ee"),
        hr(),
        h3("Risk-Return Scatter"),
        withSpinner(plotOutput("riskReturnPlot", height = "400px"),
          type = 4, color = "#22d3ee")
      )
    )
  ),

  # ── Tab 9: Methodology ─────────────────────────────────────────────────────

  tabPanel("Methodology", methodology_panel())
  )
}
