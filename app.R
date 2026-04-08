# Libraries, config, and helper functions are auto-loaded from R/ directory

# --- Custom CSS ---
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

# --- UI ---

ui <- navbarPage(
  title = "Portfolio Intelligence Lab",
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
            class = "btn-warning btn-sm")
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

  # ── Tab 4: Price Trend ─────────────────────────────────────────────────────

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

  # ── Tab 5: Performance (daily + cumulative returns) ───────────────────────

  tabPanel("Performance",
    mainPanel(
      uiOutput("performance_portfolio_block"),
      hr(),
      h3("Per-ticker exploration"),
      tags$p(class = "text-muted",
        "Uses tickers selected on the Price Trend tab (last ~1 year of Yahoo data)."
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

  # ── Tab 8: Forecast ─────────────────────────────────────────────────────────

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
  ),

  # ── Tab 9: Risk Analysis ────────────────────────────────────────────────────

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

  # ── Tab 10: Methodology ─────────────────────────────────────────────────────

  tabPanel("Methodology", methodology_panel())
)

# --- Server ---

server <- function(input, output, session) {

  # Home tab: lock viewport height (no scroll past landing content)
  observe({
    v <- input$main_nav
    home <- is.null(v) || identical(v, "Home")
    session$sendCustomMessage("setHomeTab", list(home = home))
  })

  # ══════════════════════════════════════════════════════════════════════════

  # PORTFOLIO BUILDER
  # ══════════════════════════════════════════════════════════════════════════

  # Classify insight copy for card styling (warn / ok / neutral).
  insight_card_class <- function(text) {
    tl <- tolower(text)
    warn_pat <- c(
      "underperform", "negative sharpe", "limited diversification",
      "significant downside", "heavily concentrated", "dominant risk",
      "diverge meaningfully", "highly correlated"
    )
    ok_pat <- c(
      "outperformed", "strong risk-adjusted", "strong diversification",
      "moderate diversification benefit"
    )
    if (any(vapply(warn_pat, function(p) grepl(p, tl, fixed = TRUE), logical(1)))) {
      return("insight-card insight-card--warn")
    }
    if (any(vapply(ok_pat, function(p) grepl(p, tl, fixed = TRUE), logical(1)))) {
      return("insight-card insight-card--ok")
    }
    "insight-card insight-card--neutral"
  }

  portfolio <- reactiveValues(
    holdings          = tibble(ticker = character(), weight = numeric(), sector = character()),
    analyzed          = FALSE,
    price_data        = NULL,
    returns           = NULL,
    benchmark_data    = NULL,
    benchmark_returns = NULL,
    benchmark_name    = "SPY",
    metrics           = NULL,
    weights_frac      = NULL,
    sector_weights    = NULL,
    drawdown          = NULL,
    risk_contrib      = NULL,
    diversification   = NULL,
    holding_cor       = NULL,
    return_attrib     = NULL,
    sector_return_attrib = NULL
  )

  # -- Landing page CTAs ----------------------------------------------------

  observeEvent(input$landing_start, {
    updateNavbarPage(session, inputId = "main_nav", selected = "Build Portfolio")
  }, ignoreInit = TRUE)

  observeEvent(input$landing_sample, {
    sample_name <- names(sample_portfolios)[[1]]
    sample      <- sample_portfolios[[sample_name]]
    sectors     <- sapply(sample$tickers, get_sector_for_ticker)
    portfolio$holdings <- tibble(
      ticker = sample$tickers,
      weight = sample$weights,
      sector = sectors
    )
    portfolio$analyzed <- FALSE
    updateSelectInput(session, "sample_portfolio", selected = sample_name)
    updateNavbarPage(session, inputId = "main_nav", selected = "Build Portfolio")
    showNotification(paste("Loaded sample:", sample_name), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$landing_methodology, {
    updateNavbarPage(session, inputId = "main_nav", selected = "Methodology")
  }, ignoreInit = TRUE)

  observeEvent(input$landing_scenarios, {
    updateNavbarPage(session, inputId = "main_nav", selected = "Scenarios")
  }, ignoreInit = TRUE)

  # -- Add holding ----------------------------------------------------------

  observeEvent(input$add_holding, {
    req(input$portfolio_ticker_input)
    ticker <- toupper(trimws(input$portfolio_ticker_input))
    weight <- input$portfolio_weight_input

    if (nchar(ticker) == 0) {
      showNotification("Please enter a ticker symbol.", type = "error")
      return()
    }
    if (is.na(weight) || weight <= 0) {
      showNotification("Weight must be greater than 0.", type = "error")
      return()
    }

    if (ticker %in% portfolio$holdings$ticker) {
      portfolio$holdings$weight[portfolio$holdings$ticker == ticker] <- weight
      showNotification(paste(ticker, "weight updated to", weight, "%"), type = "message")
    } else {
      sector <- get_sector_for_ticker(ticker)
      portfolio$holdings <- bind_rows(
        portfolio$holdings,
        tibble(ticker = ticker, weight = weight, sector = sector)
      )
      showNotification(paste(ticker, "added at", weight, "%"), type = "message")
    }

    updateSelectizeInput(session, "portfolio_ticker_input", selected = "")
    portfolio$analyzed <- FALSE
  })

  # -- Load sample ----------------------------------------------------------

  observeEvent(input$load_sample, {
    req(input$sample_portfolio, input$sample_portfolio != "")
    sample <- sample_portfolios[[input$sample_portfolio]]
    sectors <- sapply(sample$tickers, get_sector_for_ticker)
    portfolio$holdings <- tibble(
      ticker = sample$tickers,
      weight = sample$weights,
      sector = sectors
    )
    portfolio$analyzed <- FALSE
    showNotification(paste("Loaded:", input$sample_portfolio), type = "message")
  })

  # -- Remove holding -------------------------------------------------------

  observeEvent(input$remove_holding, {
    req(input$remove_ticker_select)
    portfolio$holdings <- portfolio$holdings %>%
      filter(ticker != input$remove_ticker_select)
    portfolio$analyzed <- FALSE
  })

  # -- Clear all holdings ---------------------------------------------------

  observeEvent(input$clear_portfolio, {
    portfolio$holdings  <- tibble(ticker = character(), weight = numeric(), sector = character())
    portfolio$analyzed  <- FALSE
    portfolio$returns   <- NULL
    portfolio$metrics   <- NULL
    portfolio$return_attrib <- NULL
    portfolio$sector_return_attrib <- NULL
  })

  # -- Keep remove dropdown in sync -----------------------------------------

  observe({
    tickers <- portfolio$holdings$ticker
    updateSelectInput(session, "remove_ticker_select",
      choices  = if (length(tickers) > 0) tickers else c("(none)" = ""))
  })

  # -- Holdings table -------------------------------------------------------

  output$portfolio_holdings_table <- renderTable({
    req(nrow(portfolio$holdings) > 0)
    portfolio$holdings %>%
      mutate(`Weight (%)` = sprintf("%.1f%%", weight)) %>%
      select(Ticker = ticker, `Weight (%)`, Sector = sector)
  }, striped = TRUE, hover = TRUE, align = "c")

  # -- Summary stats --------------------------------------------------------

  output$total_weight_display <- renderUI({
    total <- sum(portfolio$holdings$weight)
    cls <- if (abs(total - 100) < 0.5) "weight-ok" else "weight-warn"
    label <- if (abs(total - 100) < 0.5) {
      sprintf("%.1f%%", total)
    } else {
      sprintf("%.1f%% (%s)", total,
              if (total > 100) "over-allocated" else "under-allocated")
    }
    tags$div(class = paste("kpi-value", cls), label)
  })

  output$num_holdings_display <- renderText({
    nrow(portfolio$holdings)
  })

  output$concentration_display <- renderText({
    if (nrow(portfolio$holdings) == 0) return("--")
    score <- compute_concentration_score(portfolio$holdings$weight)
    sprintf("%.2f", score)
  })

  # -- Allocation pie -------------------------------------------------------

  output$allocation_pie <- renderPlot({
    req(nrow(portfolio$holdings) > 0)
    df <- portfolio$holdings %>% mutate(pct = weight / sum(weight))
    ggplot(df, aes(x = "", y = pct, fill = ticker)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      labs(title = "Allocation by Holding", fill = "Ticker") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_void(base_size = 13) +
      theme(legend.position = "right")
  })

  # -- Sector pie -----------------------------------------------------------

  output$sector_pie <- renderPlot({
    req(nrow(portfolio$holdings) > 0)
    sw <- compute_sector_weights(portfolio$holdings$ticker, portfolio$holdings$weight) %>%
      mutate(pct = weight / sum(weight))
    ggplot(sw, aes(x = "", y = pct, fill = sector)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      labs(title = "Allocation by Sector", fill = "Sector") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_void(base_size = 13) +
      theme(legend.position = "right")
  })

  # -- Analyze portfolio ----------------------------------------------------

  observeEvent(input$analyze_portfolio, {
    holdings <- portfolio$holdings
    if (nrow(holdings) == 0) {
      showNotification("Add at least one holding first.", type = "error")
      return()
    }

    weights <- holdings$weight
    names(weights) <- holdings$ticker
    weights_frac <- weights / sum(weights)

    withProgress(message = "Analyzing portfolio...", value = 0, {

      incProgress(0.15, detail = "Fetching holdings data...")
      price_data <- fetch_stock_data(
        holdings$ticker,
        from_date = input$portfolio_date_range[1],
        to_date   = input$portfolio_date_range[2])

      if (is.null(price_data)) {
        showNotification("Could not fetch price data. Check ticker symbols.", type = "error")
        return()
      }

      failed <- setdiff(holdings$ticker, unique(price_data$ticker))
      if (length(failed) > 0) {
        showNotification(
          paste("No data for:", paste(failed, collapse = ", ")),
          type = "warning", duration = 6)
      }

      incProgress(0.15, detail = "Fetching benchmark data...")
      benchmark_data <- fetch_stock_data(
        input$benchmark_ticker,
        from_date = input$portfolio_date_range[1],
        to_date   = input$portfolio_date_range[2])

      incProgress(0.2, detail = "Computing portfolio returns...")
      port_returns <- compute_portfolio_returns(price_data, weights_frac)

      if (is.null(port_returns)) {
        showNotification("Could not compute portfolio returns.", type = "error")
        return()
      }

      bench_returns <- NULL
      if (!is.null(benchmark_data)) {
        bw <- c(1)
        names(bw) <- input$benchmark_ticker
        bench_returns <- compute_portfolio_returns(benchmark_data, bw)
      }

      incProgress(0.2, detail = "Computing metrics...")
      metrics <- compute_portfolio_metrics(port_returns, bench_returns)

      sector_w <- compute_sector_weights(holdings$ticker, holdings$weight)

      incProgress(0.1, detail = "Computing diagnostics...")
      dd_series <- compute_drawdown_series(port_returns)
      risk_c    <- compute_risk_contribution(price_data, weights_frac)
      div_ratio <- compute_diversification_ratio(price_data, weights_frac)
      hold_cor  <- compute_holding_correlations(price_data)
      ret_attr  <- compute_return_attribution(price_data, weights_frac)
      sec_attr  <- if (!is.null(ret_attr)) aggregate_return_attrib_by_sector(ret_attr) else NULL

      portfolio$price_data        <- price_data
      portfolio$returns           <- port_returns
      portfolio$benchmark_data    <- benchmark_data
      portfolio$benchmark_returns <- bench_returns
      portfolio$benchmark_name    <- names(benchmark_choices)[benchmark_choices == input$benchmark_ticker]
      portfolio$metrics           <- metrics
      portfolio$weights_frac      <- weights_frac
      portfolio$sector_weights    <- sector_w
      portfolio$drawdown          <- dd_series
      portfolio$risk_contrib      <- risk_c
      portfolio$diversification   <- div_ratio
      portfolio$holding_cor       <- hold_cor
      portfolio$return_attrib     <- ret_attr
      portfolio$sector_return_attrib <- sec_attr
      portfolio$analyzed          <- TRUE

      incProgress(0.3, detail = "Done!")
    })

    showNotification("Portfolio analysis complete!", type = "message", duration = 3)
  })

  # -- Results section (dynamic UI) -----------------------------------------

  output$portfolio_results_section <- renderUI({
    if (!portfolio$analyzed) {
      return(div(class = "empty-state",
        tags$h4("No portfolio analyzed yet"),
        tags$p("Add holdings above and click ",
               tags$b("Analyze Portfolio"), " to see results.")
      ))
    }

    m  <- portfolio$metrics
    sw <- portfolio$sector_weights

    ret_cls   <- if (m$annual_return >= 0) "kpi-positive" else "kpi-negative"
    sharp_cls <- if (m$sharpe_ratio > 0.5) "kpi-positive" else
                 if (m$sharpe_ratio >= 0)  "kpi-neutral"  else "kpi-negative"
    beta_val  <- if (!is.null(m$beta) && !is.na(m$beta)) sprintf("%.2f", m$beta) else "N/A"

    insights <- generate_insights(m, sw)

    tagList(
      hr(),
      h3("Portfolio Analysis Results"),

      fluidRow(
        column(2, offset = 1, tags$div(class = "kpi-card",
          tags$div(class = paste("kpi-value", ret_cls),
            sprintf("%.1f%%", m$annual_return * 100)),
          tags$div(class = "kpi-label", "Annual Return")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = "kpi-value kpi-neutral",
            sprintf("%.1f%%", m$annual_volatility * 100)),
          tags$div(class = "kpi-label", "Volatility")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = paste("kpi-value", sharp_cls),
            sprintf("%.2f", m$sharpe_ratio)),
          tags$div(class = "kpi-label", "Sharpe Ratio")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = "kpi-value kpi-negative",
            sprintf("%.1f%%", m$max_drawdown * 100)),
          tags$div(class = "kpi-label", "Max Drawdown")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = "kpi-value kpi-neutral", beta_val),
          tags$div(class = "kpi-label", "Beta")
        ))
      ),

      br(),
      fluidRow(
        column(12,
          withSpinner(plotOutput("portfolio_perf_chart", height = "420px"),
            type = 4, color = "#22d3ee")
        )
      ),

      tags$p(class = "text-muted text-center", style = "margin-top: 15px; font-size: 14px;",
        "See the ", tags$b("Diagnosis"),
        " tab for detailed risk analysis, rolling metrics, diversification diagnostics, and insights."
      )
    )
  })

  # -- Performance chart ----------------------------------------------------

  output$portfolio_perf_chart <- renderPlot({
    req(portfolio$analyzed, portfolio$returns)

    port_df <- portfolio$returns %>%
      transmute(date, cumulative_return, series = "Portfolio")

    if (!is.null(portfolio$benchmark_returns)) {
      bench_df <- portfolio$benchmark_returns %>%
        transmute(date, cumulative_return, series = portfolio$benchmark_name)
      combined <- bind_rows(port_df, bench_df)
    } else {
      combined <- port_df
    }

    ggplot(combined, aes(x = date, y = cumulative_return, color = series)) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(
        title = "Portfolio vs Benchmark: Cumulative Return",
        x = "Date", y = "Cumulative Return", color = ""
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c("Portfolio" = "#2c3e50",
                                    setNames("#e74c3c", portfolio$benchmark_name))) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # DIAGNOSIS TAB
  # ══════════════════════════════════════════════════════════════════════════

  # -- Rolling metrics reactive (recomputes when window slider changes) -----

  diag_rolling <- reactive({
    req(portfolio$analyzed, portfolio$returns)
    window <- if (!is.null(input$diag_rolling_window)) input$diag_rolling_window else 30
    compute_rolling_metrics(portfolio$returns, portfolio$benchmark_returns, window)
  })

  # -- Diagnosis content (dynamic UI) ---------------------------------------

  output$diagnosis_content <- renderUI({
    if (!portfolio$analyzed) {
      return(div(class = "empty-state",
        tags$h4("No portfolio analyzed yet"),
        tags$p("Go to ", tags$b("Build Portfolio"),
               " to create and analyze a portfolio first.")
      ))
    }

    m  <- portfolio$metrics
    sw <- portfolio$sector_weights
    rc <- portfolio$risk_contrib
    dr <- portfolio$diversification

    ret_cls     <- if (m$annual_return >= 0) "kpi-positive" else "kpi-negative"
    sharp_cls   <- if (m$sharpe_ratio > 0.5) "kpi-positive" else
                   if (m$sharpe_ratio >= 0)  "kpi-neutral"  else "kpi-negative"
    sortino_val <- if (!is.na(m$sortino_ratio)) sprintf("%.2f", m$sortino_ratio) else "N/A"
    sortino_cls <- if (!is.na(m$sortino_ratio) && m$sortino_ratio > 0.5) "kpi-positive" else
                   if (!is.na(m$sortino_ratio) && m$sortino_ratio >= 0)  "kpi-neutral"  else "kpi-negative"
    beta_val    <- if (!is.null(m$beta) && !is.na(m$beta)) sprintf("%.2f", m$beta) else "N/A"

    insights <- generate_diagnosis_insights(m, sw, rc, dr, portfolio$holding_cor)
    actions  <- generate_action_suggestions(
      m, portfolio$return_attrib, rc, dr, portfolio$holding_cor)

    tagList(
      div(class = "builder-header",
        fluidRow(
          column(6,
            h3("Portfolio Diagnosis"),
            tags$p(class = "text-muted",
              sprintf("Analysis of %d holdings over the selected period against %s.",
                      nrow(portfolio$holdings), portfolio$benchmark_name))
          ),
          column(6,
            div(class = "diagnosis-downloads", style = "text-align: right; padding-top: 4px;",
              downloadButton("download_diagnosis", "Diagnosis (.txt)",
                class = "btn-default btn-sm"),
              downloadButton("download_metrics_csv", "Metrics (.csv)",
                class = "btn-default btn-sm"),
              downloadButton("download_holding_attrib_csv", "Holding attribution (.csv)",
                class = "btn-default btn-sm"),
              downloadButton("download_sector_attrib_csv", "Sector attribution (.csv)",
                class = "btn-default btn-sm")
            )
          )
        )
      ),

      # ── KPI Cards ──
      fluidRow(
        column(2, tags$div(class = "kpi-card",
          tags$div(class = paste("kpi-value", ret_cls),
            sprintf("%.1f%%", m$annual_return * 100)),
          tags$div(class = "kpi-label", "Annual Return")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = "kpi-value kpi-neutral",
            sprintf("%.1f%%", m$annual_volatility * 100)),
          tags$div(class = "kpi-label", "Volatility")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = paste("kpi-value", sharp_cls),
            sprintf("%.2f", m$sharpe_ratio)),
          tags$div(class = "kpi-label", "Sharpe Ratio")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = paste("kpi-value", sortino_cls), sortino_val),
          tags$div(class = "kpi-label", "Sortino Ratio")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = "kpi-value kpi-negative",
            sprintf("%.1f%%", m$max_drawdown * 100)),
          tags$div(class = "kpi-label", "Max Drawdown")
        )),
        column(2, tags$div(class = "kpi-card",
          tags$div(class = "kpi-value kpi-neutral", beta_val),
          tags$div(class = "kpi-label", "Beta")
        ))
      ),

      # ── Insights (plain-language cards) ──
      if (length(insights) > 0) {
        tagList(
          h4(style = "margin-top: 20px;", "Insights"),
          fluidRow(
            lapply(seq_along(insights), function(k) {
              column(4, div(class = insight_card_class(insights[k]),
                p(style = "margin: 0;", insights[k])))
            })
          )
        )
      },

      # ── What to consider next (Phase 2) ──
      if (length(actions) > 0) {
        tagList(
          h4(style = "margin-top: 24px;", "What to consider next"),
          tags$p(class = "text-muted", style = "font-size: 13px;",
            "Ideas to stress-test your allocation — not recommendations."),
          fluidRow(
            lapply(seq_along(actions), function(k) {
              column(4, div(class = "insight-card insight-card--action",
                p(style = "margin: 0;", actions[k])))
            })
          )
        )
      },

      # ── Performance Section ──
      tags$h4(style = "border-bottom: 2px solid #eee; padding-bottom: 8px; margin-top: 30px;",
        "Performance"),
      fluidRow(
        column(7, withSpinner(plotOutput("diag_perf_chart", height = "380px"),
          type = 4, color = "#22d3ee")),
        column(5, withSpinner(plotOutput("diag_drawdown_chart", height = "380px"),
          type = 4, color = "#22d3ee"))
      ),

      # ── Rolling Metrics Section ──
      tags$h4(style = "border-bottom: 2px solid #eee; padding-bottom: 8px; margin-top: 30px;",
        "Rolling Metrics"),
      fluidRow(
        column(3,
          sliderInput("diag_rolling_window", "Rolling Window (days):",
            min = 10, max = 60, value = 30, step = 5)
        )
      ),
      fluidRow(
        column(6, withSpinner(plotOutput("diag_rolling_vol_chart", height = "350px"),
          type = 4, color = "#22d3ee")),
        column(6, withSpinner(plotOutput("diag_rolling_sharpe_chart", height = "350px"),
          type = 4, color = "#22d3ee"))
      ),

      # ── Risk & Diversification Section ──
      tags$h4(style = "border-bottom: 2px solid #eee; padding-bottom: 8px; margin-top: 30px;",
        "Risk & Diversification"),
      fluidRow(
        column(6, withSpinner(plotOutput("diag_risk_contrib_chart", height = "380px"),
          type = 4, color = "#22d3ee")),
        column(6, withSpinner(plotOutput("diag_corr_heatmap", height = "380px"),
          type = 4, color = "#22d3ee"))
      ),
      fluidRow(
        column(6,
          withSpinner(plotOutput("diag_sector_chart", height = "320px"),
            type = 4, color = "#22d3ee"),
          h5("Sector weights"),
          tableOutput("diag_sector_table")
        ),
        column(6,
          wellPanel(style = "margin-top: 0;",
            h4("Diversification score"),
            tags$div(class = "kpi-card",
              tags$div(class = paste("kpi-value",
                if (!is.na(dr) && dr > 1.3) "kpi-positive" else
                if (!is.na(dr) && dr > 1.0) "kpi-neutral"  else "kpi-negative"),
                if (!is.na(dr)) sprintf("%.2f", dr) else "N/A"),
              tags$div(class = "kpi-label", "Diversification ratio")
            ),
            tags$p(class = "text-muted", style = "margin-top: 10px; font-size: 13px;",
              "Values above 1.0 indicate diversification benefit.",
              "Higher values mean holdings are less correlated,",
              "reducing portfolio risk below the weighted average of individual risks."
            )
          )
        )
      ),

      # ── Return attribution (linear, daily) ──
      tags$h4(style = "border-bottom: 2px solid #eee; padding-bottom: 8px; margin-top: 30px;",
        "Return attribution"),
      tags$p(class = "text-muted",
        "Each holding's share of the sum of daily portfolio returns (fixed weights).",
        "Use with risk contribution below; names can differ when correlations matter."),
      fluidRow(
        column(6, tableOutput("diag_return_attrib_table")),
        column(6, withSpinner(plotOutput("diag_return_attrib_chart", height = "320px"),
          type = 4, color = "#22d3ee"))
      ),

      # ── Sector return attribution ──
      tags$h4(style = "border-bottom: 2px solid #eee; padding-bottom: 8px; margin-top: 30px;",
        "Sector return attribution"),
      tags$p(class = "text-muted",
        "Sector weights sum holdings; linear share sums each holding’s linear attribution within the sector. ",
        "Weighted period return is a weight blend of holding period returns inside that sector."),
      fluidRow(
        column(6, tableOutput("diag_sector_return_table")),
        column(6, withSpinner(plotOutput("diag_sector_return_chart", height = "320px"),
          type = 4, color = "#22d3ee"))
      ),

      # ── Detailed Metrics Table ──
      tags$h4(style = "border-bottom: 2px solid #eee; padding-bottom: 8px; margin-top: 30px;",
        "Detailed Metrics"),
      tableOutput("diag_metrics_table"),
      br()
    )
  })

  # -- Diagnosis: Cumulative performance chart ------------------------------

  output$diag_perf_chart <- renderPlot({
    req(portfolio$analyzed, portfolio$returns)

    port_df <- portfolio$returns %>%
      transmute(date, cumulative_return, series = "Portfolio")

    if (!is.null(portfolio$benchmark_returns)) {
      bench_df <- portfolio$benchmark_returns %>%
        transmute(date, cumulative_return, series = portfolio$benchmark_name)
      combined <- bind_rows(port_df, bench_df)
    } else {
      combined <- port_df
    }

    ggplot(combined, aes(x = date, y = cumulative_return, color = series)) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(title = "Cumulative Return", x = "Date", y = "Cumulative Return", color = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c("Portfolio" = "#2c3e50",
                                    setNames("#e74c3c", portfolio$benchmark_name))) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # -- Diagnosis: Drawdown chart --------------------------------------------

  output$diag_drawdown_chart <- renderPlot({
    req(portfolio$analyzed, portfolio$drawdown)

    ggplot(portfolio$drawdown, aes(x = date, y = drawdown)) +
      geom_area(fill = "#e74c3c", alpha = 0.3) +
      geom_line(color = "#e74c3c", linewidth = 0.7) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      labs(title = "Portfolio Drawdown", x = "Date", y = "Drawdown") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 13)
  })

  # -- Diagnosis: Rolling volatility ----------------------------------------

  output$diag_rolling_vol_chart <- renderPlot({
    rolling <- diag_rolling()
    req(rolling)

    plot_df <- tibble(date = rolling$date, Portfolio = rolling$rolling_vol)
    if ("bench_rolling_vol" %in% names(rolling)) {
      plot_df[[portfolio$benchmark_name]] <- rolling$bench_rolling_vol
    }

    plot_long <- plot_df %>%
      pivot_longer(-date, names_to = "series", values_to = "volatility") %>%
      filter(!is.na(volatility))

    window <- if (!is.null(input$diag_rolling_window)) input$diag_rolling_window else 30

    ggplot(plot_long, aes(x = date, y = volatility, color = series)) +
      geom_line(linewidth = 1) +
      labs(title = paste0("Rolling ", window, "-Day Volatility (Annualized)"),
           x = "Date", y = "Volatility", color = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c("Portfolio" = "#2c3e50",
                                    setNames("#e74c3c", portfolio$benchmark_name))) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # -- Diagnosis: Rolling Sharpe --------------------------------------------

  output$diag_rolling_sharpe_chart <- renderPlot({
    rolling <- diag_rolling()
    req(rolling)

    plot_df <- tibble(date = rolling$date, Portfolio = rolling$rolling_sharpe)
    if ("bench_rolling_sharpe" %in% names(rolling)) {
      plot_df[[portfolio$benchmark_name]] <- rolling$bench_rolling_sharpe
    }

    plot_long <- plot_df %>%
      pivot_longer(-date, names_to = "series", values_to = "sharpe") %>%
      filter(!is.na(sharpe))

    window <- if (!is.null(input$diag_rolling_window)) input$diag_rolling_window else 30

    ggplot(plot_long, aes(x = date, y = sharpe, color = series)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      labs(title = paste0("Rolling ", window, "-Day Sharpe Ratio (Annualized)"),
           x = "Date", y = "Sharpe Ratio", color = "") +
      scale_color_manual(values = c("Portfolio" = "#2c3e50",
                                    setNames("#e74c3c", portfolio$benchmark_name))) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # -- Diagnosis: Risk contribution -----------------------------------------

  output$diag_risk_contrib_chart <- renderPlot({
    req(portfolio$analyzed, portfolio$risk_contrib)
    rc <- portfolio$risk_contrib

    ggplot(rc, aes(x = reorder(ticker, pct_contribution), y = pct_contribution,
                   fill = ticker)) +
      geom_col(width = 0.7) +
      coord_flip() +
      labs(title = "Risk Contribution by Holding",
           x = "", y = "% of Portfolio Risk") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  # -- Diagnosis: Correlation heatmap ---------------------------------------

  output$diag_corr_heatmap <- renderPlot({
    req(portfolio$analyzed, portfolio$holding_cor)
    cor_matrix <- portfolio$holding_cor
    melted     <- melt(cor_matrix)

    ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3.5) +
      scale_fill_gradient2(low = "#3498db", high = "#e74c3c", mid = "white",
                           midpoint = 0, limit = c(-1, 1), name = "Correlation") +
      labs(title = "Holdings Correlation Matrix") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title   = element_blank(),
            panel.grid   = element_blank()) +
      coord_fixed()
  })

  # -- Diagnosis: Sector concentration chart --------------------------------

  output$diag_sector_chart <- renderPlot({
    req(portfolio$analyzed, portfolio$sector_weights)
    sw <- portfolio$sector_weights %>%
      mutate(pct = weight / sum(weight))

    eq_weight <- 1 / nrow(sw)

    ggplot(sw, aes(x = reorder(sector, pct), y = pct, fill = sector)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = eq_weight, linetype = "dashed",
                 color = "gray50", alpha = 0.7) +
      coord_flip() +
      labs(title = "Sector Allocation",
           x = "", y = "Portfolio Weight",
           caption = "Dashed line = equal sector weight") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  # -- Diagnosis: Detailed metrics table ------------------------------------

  output$diag_metrics_table <- renderTable({
    req(portfolio$analyzed, portfolio$metrics)
    build_diagnosis_metrics_tibble(portfolio$metrics)
  }, striped = TRUE, hover = TRUE, align = "lr", width = "100%")

  output$diag_sector_table <- renderTable({
    req(portfolio$analyzed, portfolio$sector_weights)
    sw <- portfolio$sector_weights
    tot <- sum(sw$weight)
    sw %>%
      mutate(`Weight (%)` = sprintf("%.1f%%", weight / tot * 100)) %>%
      select(Sector = sector, `Weight (%)`)
  }, striped = TRUE, align = "lr", width = "100%")

  output$diag_return_attrib_table <- renderTable({
    req(portfolio$analyzed, portfolio$return_attrib)
    portfolio$return_attrib %>%
      transmute(
        Ticker = ticker,
        `Weight (%)` = sprintf("%.1f%%", weight * 100),
        `Period return` = sprintf("%.1f%%", period_total_return * 100),
        `Linear share` = ifelse(is.na(linear_share), "N/A",
          sprintf("%.0f%%", linear_share * 100))
      )
  }, striped = TRUE, align = "lrrr", width = "100%")

  output$diag_return_attrib_chart <- renderPlot({
    req(portfolio$analyzed, portfolio$return_attrib)
    ra <- portfolio$return_attrib %>% filter(!is.na(linear_share))
    req(nrow(ra) > 0)
    ggplot(ra, aes(x = reorder(ticker, linear_share), y = linear_share, fill = ticker)) +
      geom_col(width = 0.72) +
      coord_flip() +
      labs(title = "Linear return attribution (share of sum of daily portfolio returns)",
           x = "", y = "Share", fill = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$diag_sector_return_table <- renderTable({
    if (!portfolio$analyzed) return(NULL)
    sr <- portfolio$sector_return_attrib
    if (is.null(sr) || nrow(sr) == 0) {
      return(data.frame(Note = "No sector attribution for this run.", check.names = FALSE))
    }
    tot_w <- sum(sr$weight_in_portfolio)
    sr %>%
      transmute(
        Sector = sector,
        `Weight (%)` = sprintf("%.1f%%", weight_in_portfolio / tot_w * 100),
        `Linear share` = ifelse(is.na(linear_share_sector), "N/A",
          sprintf("%.0f%%", linear_share_sector * 100)),
        `Weighted period return` = sprintf("%.1f%%", weighted_period_return * 100)
      )
  }, striped = TRUE, align = "lrrr", width = "100%")

  output$diag_sector_return_chart <- renderPlot({
    if (!portfolio$analyzed) return(invisible(NULL))
    sr <- portfolio$sector_return_attrib
    if (is.null(sr) || nrow(sr) == 0) {
      plot.new()
      text(0.5, 0.5, "No sector attribution for this run.")
      return()
    }
    if (all(is.na(sr$linear_share_sector))) {
      plot_df <- sr %>%
        mutate(
          plot_y = weight_in_portfolio / sum(weight_in_portfolio),
          metric = "Portfolio weight share"
        )
      sub <- "Portfolio weight share (linear share unavailable)"
    } else {
      plot_df <- sr %>%
        mutate(
          plot_y = linear_share_sector,
          metric = "Linear return share"
        )
      sub <- "Sum of holding linear shares within each sector"
    }
    ggplot(plot_df, aes(x = reorder(sector, plot_y), y = plot_y, fill = sector)) +
      geom_col(width = 0.72) +
      coord_flip() +
      labs(title = "Sector attribution", subtitle = sub, x = "", y = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$download_diagnosis <- downloadHandler(
    filename = function() {
      sprintf("portfolio-diagnosis-%s.txt", Sys.Date())
    },
    content = function(file) {
      req(portfolio$analyzed)
      m   <- portfolio$metrics
      sw  <- portfolio$sector_weights
      rc  <- portfolio$risk_contrib
      dr  <- portfolio$diversification
      ins <- generate_diagnosis_insights(m, sw, rc, dr, portfolio$holding_cor)
      act <- generate_action_suggestions(
        m, portfolio$return_attrib, rc, dr, portfolio$holding_cor)
      lines <- c(
        "Portfolio Intelligence Lab — Diagnosis export",
        paste("Generated:", format(Sys.time(), usetz = TRUE)),
        "",
        paste("Benchmark:", portfolio$benchmark_name),
        paste("Holdings:", paste(portfolio$holdings$ticker, collapse = ", ")),
        "",
        "--- Insights ---"
      )
      lines <- if (length(ins)) c(lines, paste0("- ", ins)) else c(lines, "(none)")
      lines <- c(lines, "", "--- What to consider next ---")
      lines <- if (length(act)) c(lines, paste0("- ", act)) else c(lines, "(none)")
      lines <- c(lines, "", "--- Sector weights ---")
      if (nrow(sw) > 0) {
        tot <- sum(sw$weight)
        lines <- c(lines, capture.output(
          print(sw %>% mutate(pct = sprintf("%.1f%%", weight / tot * 100)), n = 100)
        ))
      }
      lines <- c(lines, "", "--- Return attribution (holdings) ---")
      if (!is.null(portfolio$return_attrib) && nrow(portfolio$return_attrib) > 0) {
        lines <- c(lines, capture.output(print(portfolio$return_attrib, n = 100)))
      } else {
        lines <- c(lines, "(not available)")
      }
      lines <- c(lines, "", "--- Sector return attribution ---")
      if (!is.null(portfolio$sector_return_attrib) && nrow(portfolio$sector_return_attrib) > 0) {
        lines <- c(lines, capture.output(print(portfolio$sector_return_attrib, n = 100)))
      } else {
        lines <- c(lines, "(not available)")
      }
      lines <- c(lines, "", "--- Key metrics ---")
      lines <- c(lines, capture.output(print(build_diagnosis_metrics_tibble(m), n = 100)))
      lines <- c(lines, "",
        "Educational use only — not investment advice.",
        "Past performance does not guarantee future results.")
      writeLines(lines, file)
    }
  )

  output$download_metrics_csv <- downloadHandler(
    filename = function() sprintf("portfolio-metrics-%s.csv", Sys.Date()),
    content = function(file) {
      req(portfolio$analyzed, portfolio$metrics)
      utils::write.csv(
        build_diagnosis_metrics_tibble(portfolio$metrics),
        file,
        row.names = FALSE
      )
    }
  )

  output$download_holding_attrib_csv <- downloadHandler(
    filename = function() sprintf("holding-attribution-%s.csv", Sys.Date()),
    content = function(file) {
      req(portfolio$analyzed)
      ra <- portfolio$return_attrib
      if (is.null(ra) || nrow(ra) == 0) {
        utils::write.csv(
          data.frame(note = "No holding attribution for this run."),
          file,
          row.names = FALSE
        )
      } else {
        utils::write.csv(ra, file, row.names = FALSE)
      }
    }
  )

  output$download_sector_attrib_csv <- downloadHandler(
    filename = function() sprintf("sector-attribution-%s.csv", Sys.Date()),
    content = function(file) {
      req(portfolio$analyzed)
      sr <- portfolio$sector_return_attrib
      if (is.null(sr) || nrow(sr) == 0) {
        utils::write.csv(
          data.frame(note = "No sector attribution for this run."),
          file,
          row.names = FALSE
        )
      } else {
        utils::write.csv(sr, file, row.names = FALSE)
      }
    }
  )

  # ══════════════════════════════════════════════════════════════════════════
  # PERFORMANCE TAB — portfolio vs benchmark (Phase 2)
  # ══════════════════════════════════════════════════════════════════════════

  output$performance_portfolio_block <- renderUI({
    if (!portfolio$analyzed) {
      return(wellPanel(
        tags$p(class = "text-muted",
          "Run ", tags$b("Analyze Portfolio"), " on the ",
          tags$b("Build Portfolio"), " tab to see cumulative return, cumulative ",
          "excess return vs your benchmark, and a period summary for your weights ",
          "and date range."
        )
      ))
    }
    tagList(
      h3("Your portfolio vs benchmark"),
      tags$p(class = "text-muted",
        "Aligned to the analysis period and benchmark from Build Portfolio."),
      fluidRow(
        column(6, withSpinner(plotOutput("perf_port_cumul", height = "300px"),
          type = 4, color = "#22d3ee")),
        column(6, withSpinner(plotOutput("perf_port_excess", height = "300px"),
          type = 4, color = "#22d3ee"))
      ),
      fluidRow(column(12,
        h5("Period summary"),
        tableOutput("perf_port_summary")
      ))
    )
  })

  output$perf_port_cumul <- renderPlot({
    req(portfolio$analyzed, portfolio$returns)
    port_df <- portfolio$returns %>%
      transmute(date, cumulative_return, series = "Portfolio")
    if (!is.null(portfolio$benchmark_returns)) {
      bench_df <- portfolio$benchmark_returns %>%
        transmute(date, cumulative_return, series = portfolio$benchmark_name)
      combined <- bind_rows(port_df, bench_df)
    } else {
      combined <- port_df
    }
    ggplot(combined, aes(x = date, y = cumulative_return, color = series)) +
      geom_line(linewidth = 1.1) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(title = "Cumulative return", x = "Date", y = "Cumulative return", color = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c(
        "Portfolio" = "#2c3e50",
        setNames("#e74c3c", portfolio$benchmark_name)
      )) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$perf_port_excess <- renderPlot({
    req(portfolio$analyzed, portfolio$returns)
    if (is.null(portfolio$benchmark_returns)) {
      plot.new()
      text(0.5, 0.5, "Load benchmark data from Build Portfolio\nto see cumulative excess return.")
      return()
    }
    p_df <- portfolio$returns %>% select(date, r_p = portfolio_return)
    b_df <- portfolio$benchmark_returns %>% select(date, r_b = portfolio_return)
    joined <- inner_join(p_df, b_df, by = "date") %>%
      arrange(date) %>%
      mutate(
        excess        = r_p - r_b,
        cum_excess    = cumprod(1 + excess) - 1
      )
    req(nrow(joined) > 2)
    ggplot(joined, aes(x = date, y = cum_excess)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      geom_line(linewidth = 1.1, color = "#22d3ee") +
      labs(
        title = paste0("Cumulative excess return vs ", portfolio$benchmark_name),
        x = "Date",
        y = "Cumulative excess return"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 13)
  })

  output$perf_port_summary <- renderTable({
    req(portfolio$analyzed, portfolio$metrics, portfolio$returns)
    m <- portfolio$metrics
    ptr <- portfolio$returns
    port_tr <- dplyr::last(ptr$cumulative_return)
    bench_tr <- NA_real_
    bench_vol <- NA_character_
    bench_sharpe <- NA_character_
    if (!is.null(portfolio$benchmark_returns)) {
      btr <- portfolio$benchmark_returns
      bench_tr <- dplyr::last(btr$cumulative_return)
      br <- btr$portfolio_return
      bench_vol <- sprintf("%.1f%%", sd(br, na.rm = TRUE) * sqrt(252) * 100)
      bs <- if (sd(br, na.rm = TRUE) > 0) {
        mean(br, na.rm = TRUE) / sd(br, na.rm = TRUE) * sqrt(252)
      } else NA_real_
      bench_sharpe <- if (is.finite(bs)) sprintf("%.2f", bs) else "N/A"
    }
    has_bench <- !is.null(portfolio$benchmark_returns)
    tibble(
      Series = c("Portfolio", portfolio$benchmark_name),
      `Period total return` = c(
        sprintf("%.1f%%", port_tr * 100),
        if (!has_bench || is.na(bench_tr)) "N/A" else sprintf("%.1f%%", bench_tr * 100)
      ),
      `Ann. volatility` = c(
        sprintf("%.1f%%", m$annual_volatility * 100),
        if (has_bench) bench_vol else "N/A"
      ),
      `Sharpe` = c(
        sprintf("%.2f", m$sharpe_ratio),
        if (has_bench) bench_sharpe else "N/A"
      ),
      `Max drawdown` = c(
        sprintf("%.1f%%", m$max_drawdown * 100),
        if (has_bench) "—" else "N/A"
      ),
      `Beta` = c(
        if (!is.null(m$beta) && !is.na(m$beta)) sprintf("%.2f", m$beta) else "N/A",
        if (has_bench) "1.00" else "N/A"
      ),
      `Excess return (ann.)` = c(
        if (!is.null(m$excess_return)) sprintf("%.1f%%", m$excess_return * 100) else "N/A",
        if (has_bench) "—" else "N/A"
      ),
      `Tracking error (ann.)` = c(
        if (!is.null(m$tracking_error)) sprintf("%.1f%%", m$tracking_error * 100) else "N/A",
        if (has_bench) "—" else "N/A"
      )
    )
  }, align = "c", width = "100%")

  # ══════════════════════════════════════════════════════════════════════════
  # SCENARIOS TAB
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$stress_preset, {
    req(input$stress_preset)
    if (input$stress_preset != "custom") {
      p <- stress_presets[[input$stress_preset]]
      if (!is.null(p)) {
        updateDateRangeInput(session, "stress_custom_range",
          start = p$start, end = p$end)
      }
    }
  }, ignoreInit = TRUE)

  observe({
    hp <- portfolio$holdings$ticker
    if (length(hp) == 0) return()
    secs <- names(stock_sectors)[vapply(names(stock_sectors), function(s) {
      any(hp %in% stock_sectors[[s]])
    }, logical(1))]
    if (length(secs) == 0) return()
    cur <- input$stress_sector_pick
    sel <- if (!is.null(cur) && cur %in% secs) cur else secs[[1]]
    updateSelectInput(session, "stress_sector_pick", choices = secs, selected = sel)
  })

  stress_window_dates <- reactive({
    req(input$stress_custom_range)
    list(start = input$stress_custom_range[1], end = input$stress_custom_range[2])
  })

  output$stress_hist_plot <- renderPlot({
    req(portfolio$analyzed, portfolio$returns)
    dates <- stress_window_dates()
    ps <- slice_returns_by_date(portfolio$returns, dates$start, dates$end)
    validate(need(!is.null(ps) && nrow(ps) >= 2,
      "No overlapping portfolio data in this date range. Widen the range or re-analyze."))

    p_re <- rebase_cumulative_from_slice(ps)
    if (is.null(p_re) || nrow(p_re) < 2) {
      plot.new()
      text(0.5, 0.5, "Insufficient data in window.")
      return()
    }

    if (!is.null(portfolio$benchmark_returns)) {
      bs <- slice_returns_by_date(portfolio$benchmark_returns, dates$start, dates$end)
      if (!is.null(bs) && nrow(bs) >= 2) {
        b_re <- rebase_cumulative_from_slice(bs)
        if (!is.null(b_re) && nrow(b_re) >= 2) {
          combined <- bind_rows(
            p_re %>% transmute(date, cumulative_return, series = "Portfolio"),
            b_re %>% transmute(date, cumulative_return, series = portfolio$benchmark_name)
          )
          return(
            ggplot(combined, aes(x = date, y = cumulative_return, color = series)) +
              geom_line(linewidth = 1.05) +
              geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
              labs(title = "Cumulative return in window (rebased)", x = "Date", y = "", color = "") +
              scale_y_continuous(labels = scales::percent_format()) +
              scale_color_manual(values = c(
                "Portfolio" = "#2c3e50",
                setNames("#e74c3c", portfolio$benchmark_name)
              )) +
              theme_minimal(base_size = 13) +
              theme(legend.position = "bottom")
          )
        }
      }
    }

    ggplot(p_re, aes(x = date, y = cumulative_return)) +
      geom_line(linewidth = 1.05, color = "#2c3e50") +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(title = "Cumulative return in window (rebased, portfolio only)",
           x = "Date", y = "Cumulative return") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 13)
  })

  output$stress_hist_table <- renderTable({
    req(portfolio$analyzed, portfolio$returns)
    dates <- stress_window_dates()
    ps <- slice_returns_by_date(portfolio$returns, dates$start, dates$end)
    validate(need(!is.null(ps) && nrow(ps) >= 2, "Adjust the date range."))

    sm_p <- summarize_return_slice(ps)
    sm_b <- NULL
    if (!is.null(portfolio$benchmark_returns)) {
      bs <- slice_returns_by_date(portfolio$benchmark_returns, dates$start, dates$end)
      if (!is.null(bs) && nrow(bs) >= 2) sm_b <- summarize_return_slice(bs)
    }

    fmt_pct <- function(x) if (is.null(x)) "N/A" else sprintf("%.2f%%", x * 100)
    fmt_pct2 <- function(x) if (is.null(x)) "N/A" else sprintf("%.2f%%", x * 100)

    tibble(
      Metric = c(
        "Trading days", "Total return", "Ann. volatility", "Max drawdown",
        "Worst day", "Best day"
      ),
      Portfolio = c(
        as.character(sm_p$n_days),
        fmt_pct(sm_p$total_return),
        fmt_pct2(sm_p$ann_vol),
        fmt_pct(sm_p$max_dd),
        fmt_pct2(sm_p$worst_day),
        fmt_pct2(sm_p$best_day)
      ),
      Benchmark = c(
        if (!is.null(sm_b)) as.character(sm_b$n_days) else "N/A",
        if (!is.null(sm_b)) fmt_pct(sm_b$total_return) else "N/A",
        if (!is.null(sm_b)) fmt_pct2(sm_b$ann_vol) else "N/A",
        if (!is.null(sm_b)) fmt_pct(sm_b$max_dd) else "N/A",
        if (!is.null(sm_b)) fmt_pct2(sm_b$worst_day) else "N/A",
        if (!is.null(sm_b)) fmt_pct2(sm_b$best_day) else "N/A"
      )
    )
  }, striped = TRUE, align = "lrr", width = "100%")

  output$stress_shock_plot <- renderPlot({
    req(portfolio$analyzed, portfolio$price_data, portfolio$weights_frac)
    rw <- returns_wide_from_price_data(portfolio$price_data)
    w  <- portfolio$weights_frac
    base <- portfolio_returns_from_wide(rw, w)
    validate(need(!is.null(base) && nrow(base) >= 3, "Could not build return series for stress."))

    bps <- if (is.null(input$stress_bps)) 0 else input$stress_bps
    tickers_shock <- names(w)
    if (!is.null(input$stress_scope) && input$stress_scope == "sector" &&
          !is.null(input$stress_sector_pick) && nzchar(input$stress_sector_pick)) {
      tickers_shock <- holdings_tickers_in_sector(names(w), input$stress_sector_pick)
      if (length(tickers_shock) == 0) tickers_shock <- names(w)
    }

    rw2 <- if (bps == 0) {
      rw
    } else {
      apply_daily_return_shock_wide(rw, tickers_shock, bps / 10000)
    }
    shocked <- portfolio_returns_from_wide(rw2, w)
    validate(need(!is.null(shocked) && nrow(shocked) >= 3, "Stressed path could not be computed."))

    aligned <- inner_join(
      base %>% select(date, baseline = cumulative_return),
      shocked %>% select(date, stressed = cumulative_return),
      by = "date"
    )
    validate(need(nrow(aligned) >= 3, "No overlapping dates between baseline and stressed series."))

    long_df <- aligned %>%
      pivot_longer(c(baseline, stressed), names_to = "series", values_to = "cumulative_return") %>%
      mutate(series = ifelse(series == "baseline", "Baseline", "Stressed"))

    sub <- if (bps == 0) {
      "No daily bps shock (paths should coincide)."
    } else {
      paste0(
        bps, " bps/day to ",
        if (!is.null(input$stress_scope) && input$stress_scope == "sector" &&
              !is.null(input$stress_sector_pick)) input$stress_sector_pick else "all holdings"
      )
    }

    ggplot(long_df, aes(x = date, y = cumulative_return, color = series)) +
      geom_line(linewidth = 1.05) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(
        title = "Full-sample cumulative return: baseline vs stressed",
        subtitle = sub,
        x = "Date", y = "Cumulative return", color = ""
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c("Baseline" = "#94a3b8", "Stressed" = "#22d3ee")) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$stress_fan_plot <- renderPlot({
    req(portfolio$analyzed, portfolio$returns)
    h <- input$stress_fan_horizon
    ns <- input$stress_fan_n_sims
    sd <- input$stress_fan_seed
    fan <- bootstrap_fan_from_returns(
      portfolio$returns$portfolio_return, h, ns, sd)
    validate(need(!is.null(fan), "Need a longer return history for the bootstrap fan (at least ~10 days)."))

    sm <- fan$sample_matrix
    df_line <- bind_rows(lapply(seq_len(nrow(sm)), function(i) {
      tibble(day = fan$days, cum = sm[i, ], sim = i)
    }))
    ribbon <- tibble(
      day = fan$days,
      ymin = fan$p05,
      ymax = fan$p95,
      mid = fan$p50
    )

    ggplot() +
      geom_line(
        data = df_line, aes(x = day, y = cum, group = sim),
        alpha = 0.07, color = "#64748b", linewidth = 0.35
      ) +
      geom_ribbon(
        data = ribbon, aes(x = day, ymin = ymin, ymax = ymax),
        fill = "#22d3ee", alpha = 0.18
      ) +
      geom_line(
        data = ribbon, aes(x = day, y = mid),
        color = "#0ea5e9", linewidth = 1.05
      ) +
      labs(
        title = "Bootstrap cumulative return fan (portfolio, fixed weights)",
        subtitle = paste("Median path (teal); 5–95% band;", ns, "simulations ×", h, "days ahead"),
        x = "Trading days ahead",
        y = "Cumulative return"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 13)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # ALLOCATION LAB
  # ══════════════════════════════════════════════════════════════════════════

  output$alloc_lab_need_analyze <- renderUI({
    if (isTRUE(portfolio$analyzed)) return(NULL)
    div(
      class = "empty-state",
      p("Run ", tags$b("Analyze Portfolio"), " on ", tags$b("Build Portfolio"),
        " to compare your weights to alternative constructions.")
    )
  })

  alloc_lab_data <- reactive({
    req(isTRUE(portfolio$analyzed))
    req(!is.null(portfolio$price_data), !is.null(portfolio$weights_frac))
    ms <- compute_mu_sigma_annual(portfolio$price_data, names(portfolio$weights_frac))
    validate(need(
      !is.null(ms),
      "Not enough overlapping daily returns to estimate mean and covariance. Widen the analysis window."
    ))
    tick <- colnames(ms$Sigma)
    w0 <- align_weights(portfolio$weights_frac, tick)
    validate(need(sum(w0) > 1e-12 && length(w0) >= 1L, "Could not align weights to the return sample."))

    method <- input$alloc_method
    mw <- if (identical(method, "minvar") && !is.null(input$alloc_max_w_pct)) {
      input$alloc_max_w_pct / 100
    } else {
      1
    }
    n <- length(w0)
    if (identical(method, "minvar") && mw < 1 / n - 1e-9) {
      validate(need(
        FALSE,
        paste0(
          "Max weight must be at least ",
          sprintf("%.1f%%", 100 / n),
          " so ", n, " positive weights can sum to 100%."
        )
      ))
    }

    w1 <- propose_allocation(method, ms$Sigma, ms$mu, max_w = mw)
    w1 <- w1[tick]
    w1[is.na(w1)] <- 0
    if (sum(w1) < 1e-12) {
      w1 <- rep(1 / length(tick), length(tick))
    } else {
      w1 <- w1 / sum(w1)
    }
    names(w1) <- tick

    list(
      ms = ms,
      w0 = w0,
      w1 = w1,
      m0 = portfolio_ann_metrics(w0, ms$mu, ms$Sigma),
      m1 = portfolio_ann_metrics(w1, ms$mu, ms$Sigma)
    )
  })

  output$alloc_weights_table <- renderTable({
    req(isTRUE(portfolio$analyzed))
    d <- alloc_lab_data()
    tick <- names(d$w0)
    tibble(
      Ticker = tick,
      `Current %` = sprintf("%.1f", 100 * d$w0),
      `Proposed %` = sprintf("%.1f", 100 * d$w1[tick]),
      `Delta (pp)` = sprintf("%+.1f", 100 * (d$w1[tick] - d$w0))
    )
  }, striped = TRUE, align = "lrrr", width = "100%")

  output$alloc_metrics_table <- renderTable({
    req(isTRUE(portfolio$analyzed))
    d <- alloc_lab_data()
    f_pct <- function(x) sprintf("%.2f%%", 100 * x)
    f_sh <- function(x) if (is.na(x)) "—" else sprintf("%.2f", x)
    tibble(
      Metric = c("Ann. return", "Ann. volatility", "Sharpe (RF=0)"),
      Current = c(
        f_pct(d$m0$ann_return),
        f_pct(d$m0$ann_vol),
        f_sh(d$m0$sharpe)
      ),
      Proposed = c(
        f_pct(d$m1$ann_return),
        f_pct(d$m1$ann_vol),
        f_sh(d$m1$sharpe)
      )
    )
  }, striped = TRUE, align = "lrr", width = "100%")

  output$alloc_weights_plot <- renderPlot({
    req(isTRUE(portfolio$analyzed))
    d <- alloc_lab_data()
    tick <- names(d$w0)
    long_df <- tibble(
      ticker = factor(tick, levels = rev(sort(tick))),
      Current = d$w0,
      Proposed = d$w1[tick]
    ) %>%
      pivot_longer(-ticker, names_to = "series", values_to = "weight") %>%
      mutate(series = factor(series, levels = c("Current", "Proposed")))

    ggplot(long_df, aes(x = ticker, y = weight, fill = series)) +
      geom_col(position = position_dodge(width = 0.85), width = 0.8) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.03))) +
      scale_fill_manual(values = c("Current" = "#94a3b8", "Proposed" = "#22d3ee")) +
      labs(
        title = "Weight comparison",
        x = NULL, y = "Weight", fill = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # EXISTING TABS — Price Trend / Returns / Cumulative / Forecast / Risk
  # ══════════════════════════════════════════════════════════════════════════

  # -- Sector filter observers (Price Trend) --------------------------------

  observe({
    selected_sector <- input$sector_filter
    if (selected_sector == "all") {
      updateSelectInput(session, "tickers",
        choices  = all_stocks,
        selected = input$tickers)
    } else {
      sector_stocks <- stock_sectors[[selected_sector]]
      updateSelectInput(session, "tickers",
        choices  = sector_stocks,
        selected = sector_stocks[1:min(2, length(sector_stocks))])
    }
  })

  observe({
    selected_sector <- input$forecast_sector
    if (selected_sector == "all") {
      updateSelectInput(session, "forecast_ticker", choices = all_stocks)
    } else {
      updateSelectInput(session, "forecast_ticker",
        choices = stock_sectors[[selected_sector]])
    }
  })

  observe({
    selected_sector <- input$risk_sector
    if (selected_sector == "all") {
      updateSelectInput(session, "risk_tickers",
        choices  = all_stocks,
        selected = input$risk_tickers)
    } else {
      sector_stocks <- stock_sectors[[selected_sector]]
      updateSelectInput(session, "risk_tickers",
        choices  = sector_stocks,
        selected = sector_stocks[1:min(2, length(sector_stocks))])
    }
  })

  observeEvent(input$select_all, {
    sector <- input$sector_filter
    choices <- if (sector == "all") all_stocks else stock_sectors[[sector]]
    updateSelectInput(session, "tickers", selected = choices)
  })

  observeEvent(input$clear_all, {
    updateSelectInput(session, "tickers", selected = character(0))
  })

  # -- Stock data reactive (Price Trend / Returns / Cumulative) -------------

  stock_data <- reactive({
    if (is.null(input$tickers) || length(input$tickers) == 0) return(NULL)

    withProgress(message = "Loading stock data...", value = 0, {
      data_list <- lapply(seq_along(input$tickers), function(i) {
        ticker <- input$tickers[i]
        incProgress(1 / length(input$tickers), detail = paste("Loading", ticker))
        tryCatch({
          df <- getSymbols(ticker, src = "yahoo",
                           from = Sys.Date() - 365,
                           to   = Sys.Date(),
                           auto.assign = FALSE)
          if (is.null(df) || nrow(df) == 0) return(NULL)
          df <- fortify.zoo(df) %>% as_tibble()
          colnames(df)[1] <- "date"
          if (ncol(df) >= 6) {
            names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
          }
          df$ticker <- ticker
          if (sum(!is.na(df$adjusted)) < 10) return(NULL)
          df
        }, error = function(e) NULL)
      })
      data_list <- data_list[!sapply(data_list, is.null)]
      if (length(data_list) == 0) return(NULL)
      bind_rows(data_list)
    })
  })

  # -- Price Plot -----------------------------------------------------------

  output$pricePlot <- renderPlot({
    df <- stock_data()
    if (is.null(df) || nrow(df) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "No data to display")
      text(1, 1, "Please select at least one ticker", cex = 1.5, col = "gray40")
      return()
    }
    validate_data(df, "stock price data")
    ma_window <- input$ma_window

    df <- df %>%
      arrange(date) %>%
      group_by(ticker) %>%
      mutate(ma = if (n() >= ma_window)
        rollmean(adjusted, k = ma_window, fill = NA, align = "right") else NA) %>%
      ungroup()

    insufficient <- df %>% group_by(ticker) %>%
      summarise(has_ma = any(!is.na(ma))) %>% filter(!has_ma)
    if (nrow(insufficient) > 0)
      showNotification(paste("Insufficient data for", ma_window, "day MA for:",
        paste(insufficient$ticker, collapse = ", ")), type = "warning")

    ggplot(df, aes(x = date, y = adjusted, color = ticker)) +
      geom_line(alpha = 0.7, linewidth = 1) +
      geom_line(aes(y = ma), linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
      labs(title = paste("Adjusted Price with", ma_window, "Day Moving Average"),
           x = "Date", y = "Adjusted Price", color = "Stock") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  # -- Daily Returns --------------------------------------------------------

  output$returnsPlot <- renderPlot({
    df <- stock_data()
    validate_data(df, "returns data")
    df <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      ungroup()
    if (sum(!is.na(df$daily_return)) == 0) validate("No return data available")

    ggplot(df, aes(x = date, y = daily_return, color = ticker)) +
      geom_line(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(title = "Daily Returns by Ticker", x = "Date", y = "Daily Return",
           color = "Stock") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  # -- Return Distribution --------------------------------------------------

  output$returnDistribution <- renderPlot({
    df <- stock_data()
    validate_data(df, "returns data")
    df <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      ungroup()

    ggplot(df, aes(x = daily_return, fill = ticker)) +
      geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
      facet_wrap(~ticker, scales = "free_y") +
      labs(title = "Distribution of Daily Returns",
           x = "Daily Return", y = "Frequency", fill = "Stock") +
      scale_x_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 12)
  })

  # -- Cumulative Returns ---------------------------------------------------

  output$cumulativePlot <- renderPlot({
    df <- stock_data()
    validate_data(df, "cumulative returns data")
    df <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(
        daily_return      = adjusted / dplyr::lag(adjusted) - 1,
        cumulative_return = cumprod(1 + replace_na(daily_return, 0)) - 1
      ) %>%
      filter(!is.na(cumulative_return)) %>%
      ungroup()

    ggplot(df, aes(x = date, y = cumulative_return, color = ticker)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(title = "Cumulative Returns (Growth of $1 Invested)",
           x = "Date", y = "Cumulative Return", color = "Stock") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # -- Performance Summary --------------------------------------------------

  output$performanceSummary <- renderTable({
    df <- stock_data()
    validate_data(df, "performance data")
    df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      summarise(
        `Total Return`       = sprintf("%.2f%%",  (last(adjusted) / first(adjusted) - 1) * 100),
        `Volatility (Annual)` = sprintf("%.2f%%", sd(daily_return) * sqrt(252) * 100),
        `Sharpe Ratio`       = sprintf("%.2f",    mean(daily_return) / sd(daily_return) * sqrt(252)),
        `Best Day`           = sprintf("%.2f%%",  max(daily_return) * 100),
        `Worst Day`          = sprintf("%.2f%%",  min(daily_return) * 100),
        `Current Price`      = sprintf("$%.2f",   last(adjusted))
      ) %>%
      rename(Stock = ticker)
  }, align = "c")

  # -- Forecast data reactive -----------------------------------------------

  forecast_data <- reactive({
    if (is.null(input$forecast_ticker) || input$forecast_ticker == "") {
      showNotification("Please select a ticker for forecasting", type = "error", duration = 5)
      return(tibble())
    }
    withProgress(message = paste("Loading data for", input$forecast_ticker), value = 0.5, {
      tryCatch({
        df <- getSymbols(input$forecast_ticker, src = "yahoo",
                         from = Sys.Date() - 365,
                         to   = Sys.Date(),
                         auto.assign = FALSE)
        if (is.null(df) || nrow(df) == 0) {
          showNotification(paste("No data found for", input$forecast_ticker), type = "error")
          return(tibble())
        }
        df <- fortify.zoo(df) %>% as_tibble()
        colnames(df)[1] <- "date"
        if (ncol(df) >= 6) {
          names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
        }
        df <- df %>% arrange(date)
        if (sum(!is.na(df$adjusted)) < 30)
          showNotification("Insufficient data for forecasting", type = "warning")
        return(df)
      }, error = function(e) {
        showNotification(paste("Failed to load data for forecast:", e$message), type = "error")
        return(tibble())
      })
    })
  })

  output$dataAvailability <- renderText({
    df <- forecast_data()
    if (nrow(df) == 0) return("No data available")
    days_available <- nrow(df)
    min_for_forecast <- if (input$forecast_mode == "conservative") {
      max(50, input$horizon * 2)
    } else {
      max(30, input$horizon * 0.5)
    }
    min_for_backtest <- input$horizon + 60

    if (days_available < min_for_forecast) {
      paste("Insufficient:", days_available, "days available,", min_for_forecast, "needed")
    } else if (days_available < min_for_backtest) {
      paste("Limited:", days_available, "days (can forecast but not backtest)")
    } else {
      paste("Good:", days_available, "days of data available")
    }
  })

  # -- Forecast Plot --------------------------------------------------------

  output$forecastPlot <- renderPlot({
    df <- forecast_data()
    if (nrow(df) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "No data to display")
      text(1, 1, "Please select a ticker for forecasting", cex = 1.5, col = "gray40")
      return()
    }
    validate_data(df, "forecast data")

    min_required <- if (input$forecast_mode == "conservative") {
      max(50, input$horizon * 2)
    } else {
      max(30, input$horizon * 0.5)
    }

    if (nrow(df) < min_required) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Insufficient Data")
      text(1, 1, paste("Need at least", min_required, "days of data\n",
                        "for a", input$horizon, "day forecast\n",
                        "Current data:", nrow(df), "days\n\n",
                        "Try: Aggressive mode or shorter horizon"),
           cex = 1.2, col = "gray40")
      showNotification(paste("Insufficient data: Have", nrow(df), "days, need", min_required),
                       type = "warning", duration = 5)
      return()
    }

    withProgress(message = "Generating forecast...", value = 0.5, {
      ts_data <- ts(df$adjusted, frequency = 252)
      tryCatch({
        model <- auto.arima(ts_data)
        forecasted <- forecast(model, h = input$horizon)
        autoplot(forecasted) +
          labs(title = paste(input$forecast_ticker, "Price Forecast (Next",
                             input$horizon, "Days)"),
               x = "Time (Trading Days)", y = "Adjusted Price ($)") +
          theme_minimal(base_size = 12)
      }, error = function(e) {
        showNotification(paste("Forecast error:", e$message), type = "error")
        validate("Unable to generate forecast. Please try a different ticker or horizon.")
      })
    })
  })

  # -- Forecast Evaluation --------------------------------------------------

  output$forecastMetrics <- renderPrint({
    df <- forecast_data()
    if (nrow(df) == 0) { cat("No data available for evaluation\n"); return() }
    horizon      <- input$horizon
    min_required <- horizon + 60

    if (nrow(df) <= min_required) {
      cat("Insufficient Data for Backtesting\n")
      cat("================================\n")
      cat("To evaluate forecast accuracy, we need:\n")
      cat("  - Historical data for training\n")
      cat("  - Actual data to compare against forecast\n\n")
      cat("Current data points:", nrow(df), "\n")
      cat("Required data points:", min_required, "\n")
      cat("  (", horizon, "days forecast +", 60, "days minimum training)\n\n")
      cat("Try reducing the forecast horizon or wait for more data.\n")
      return()
    }

    withProgress(message = "Evaluating forecast accuracy...", value = 0.5, {
      df    <- df %>% arrange(date)
      train <- head(df$adjusted, -horizon)
      test  <- tail(df$adjusted, horizon)
      if (length(train) < 30) { cat("Insufficient training data for evaluation\n"); return() }
      ts_train <- ts(train, frequency = 252)

      tryCatch({
        model <- auto.arima(ts_train)
        fc    <- forecast(model, h = horizon)
        rmse  <- sqrt(mean((fc$mean - test)^2, na.rm = TRUE))
        mae   <- mean(abs(fc$mean - test), na.rm = TRUE)
        mape  <- mean(abs((fc$mean - test) / test), na.rm = TRUE) * 100

        cat("Forecast Model: ARIMA",
            paste0("(", paste(model$arma[c(1, 6, 2)], collapse = ","), ")\n"))
        cat("----------------\n")
        cat("  Forecast Horizon:", horizon, "days\n")
        cat("  Training samples:", length(train), "\n")
        cat("  RMSE:", sprintf("$%.2f", rmse), "\n")
        cat("  MAE :", sprintf("$%.2f", mae), "\n")
        cat("  MAPE:", sprintf("%.2f%%", mape), "\n")
      }, error = function(e) {
        cat("Error in forecast evaluation:", e$message, "\n")
      })
    })
  })

  # -- Forecast Accuracy Plot -----------------------------------------------

  output$forecastAccuracyPlot <- renderPlot({
    df <- forecast_data()
    if (nrow(df) == 0) return()
    horizon      <- input$horizon
    min_required <- horizon + 60
    if (nrow(df) <= min_required) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Cannot Generate Accuracy Plot")
      text(1, 1, paste("Need", min_required - nrow(df), "more days of data\n",
                        "to show forecast vs actual comparison"),
           cex = 1.2, col = "gray40")
      return()
    }

    df         <- df %>% arrange(date)
    train      <- head(df$adjusted, -horizon)
    test       <- tail(df$adjusted, horizon)
    test_dates <- tail(df$date, horizon)
    if (length(train) < 30) validate("Insufficient training data for accuracy plot")

    withProgress(message = "Creating accuracy plot...", value = 0.5, {
      ts_train <- ts(train, frequency = 252)
      tryCatch({
        model <- auto.arima(ts_train)
        fc    <- forecast(model, h = horizon)
        forecast_df <- data.frame(
          date     = test_dates,
          actual   = as.numeric(test),
          forecast = as.numeric(fc$mean),
          lower_80 = fc$lower[, 1], upper_80 = fc$upper[, 1],
          lower_95 = fc$lower[, 2], upper_95 = fc$upper[, 2]
        )
        ggplot(forecast_df, aes(x = date)) +
          geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
                      fill = "lightblue", alpha = 0.3) +
          geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
                      fill = "lightblue", alpha = 0.5) +
          geom_line(aes(y = actual),   color = "red",  linewidth = 1.2) +
          geom_line(aes(y = forecast), color = "blue", linetype = "dashed", linewidth = 1) +
          geom_point(aes(y = actual),   color = "red",  size = 2, alpha = 0.5) +
          geom_point(aes(y = forecast), color = "blue", size = 2, alpha = 0.5) +
          labs(title = paste(input$forecast_ticker,
                             "Forecast vs. Actual (Last", horizon, "Days)"),
               x = "Date", y = "Adjusted Price ($)",
               caption = "Red = Actual | Blue = Forecast | Shaded = Confidence Intervals") +
          scale_y_continuous(labels = scales::dollar_format()) +
          theme_minimal(base_size = 12)
      }, error = function(e) {
        showNotification(paste("Error creating accuracy plot:", e$message), type = "error")
        validate("Unable to create accuracy plot")
      })
    })
  })

  # -- Risk Analysis data reactive ------------------------------------------

  risk_data <- reactive({
    req(input$risk_tickers)
    if (length(input$risk_tickers) == 0) {
      showNotification("Please select at least one ticker for risk analysis", type = "error")
      return(tibble())
    }
    withProgress(message = "Loading risk analysis data...", value = 0, {
      data_list <- lapply(seq_along(input$risk_tickers), function(i) {
        ticker <- input$risk_tickers[i]
        incProgress(1 / length(input$risk_tickers), detail = paste("Loading", ticker))
        tryCatch({
          df <- getSymbols(ticker, src = "yahoo",
                           from = input$risk_date_range[1],
                           to   = input$risk_date_range[2],
                           auto.assign = FALSE)
          if (is.null(df) || nrow(df) == 0) return(NULL)
          df <- fortify.zoo(df) %>% as_tibble()
          colnames(df)[1] <- "date"
          if (ncol(df) >= 6) {
            names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
          }
          df$ticker <- ticker
          df
        }, error = function(e) NULL)
      })
    })
    data_list <- data_list[!sapply(data_list, is.null)]
    if (length(data_list) == 0) return(tibble())
    bind_rows(data_list)
  })

  # -- Risk Metrics Table ---------------------------------------------------

  output$riskMetricsTable <- renderTable({
    df <- risk_data()
    validate_data(df, "risk analysis")
    df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(
        daily_return      = (adjusted / dplyr::lag(adjusted)) - 1,
        cumulative_return = cumprod(1 + replace_na(daily_return, 0)) - 1
      ) %>%
      filter(!is.na(daily_return)) %>%
      summarise(
        `Annual Return`  = mean(daily_return, na.rm = TRUE) * 252,
        `Volatility`     = sd(daily_return, na.rm = TRUE) * sqrt(252),
        `Sharpe Ratio`   = `Annual Return` / `Volatility`,
        `Max Drawdown`   = min(cumulative_return, na.rm = TRUE),
        `VaR (95%)`      = quantile(daily_return, 0.05, na.rm = TRUE),
        `Best Day`       = max(daily_return, na.rm = TRUE),
        `Worst Day`      = min(daily_return, na.rm = TRUE),
        `Positive Days`  = sum(daily_return > 0, na.rm = TRUE) / n()
      ) %>%
      mutate(
        across(c(`Annual Return`, `Volatility`, `Max Drawdown`, `VaR (95%)`,
                  `Best Day`, `Worst Day`, `Positive Days`),
               ~ sprintf("%.2f%%", . * 100)),
        `Sharpe Ratio` = sprintf("%.2f", `Sharpe Ratio`)
      ) %>%
      rename(Stock = ticker)
  }, align = "c", striped = TRUE, hover = TRUE)

  # -- Correlation Heatmap --------------------------------------------------

  output$correlationHeatmap <- renderPlot({
    df <- risk_data()
    validate_data(df, "correlation analysis")
    returns_wide <- df %>%
      select(date, ticker, adjusted) %>%
      arrange(date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      select(date, ticker, daily_return) %>%
      pivot_wider(names_from = ticker, values_from = daily_return)

    if (ncol(returns_wide) < 3) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Correlation Matrix")
      text(1, 1, "Select at least 2 stocks for correlation analysis",
           cex = 1.2, col = "gray40")
      return()
    }

    cor_matrix <- cor(returns_wide[, -1], use = "complete.obs")
    melted_cor <- melt(cor_matrix)

    ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +
      scale_fill_gradient2(low = "#3498db", high = "#e74c3c", mid = "white",
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = "Correlation") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title   = element_blank(),
            panel.grid   = element_blank()) +
      labs(title = "Stock Return Correlation Matrix") +
      coord_fixed()
  })

  # -- Risk-Return Plot -----------------------------------------------------

  output$riskReturnPlot <- renderPlot({
    df <- risk_data()
    validate_data(df, "risk-return analysis")
    risk_return <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      summarise(
        annual_return = mean(daily_return, na.rm = TRUE) * 252,
        volatility    = sd(daily_return, na.rm = TRUE) * sqrt(252)
      )

    ggplot(risk_return, aes(x = volatility, y = annual_return)) +
      geom_point(aes(color = ticker), size = 5, alpha = 0.7) +
      geom_text(aes(label = ticker), vjust = -1.5, size = 4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      labs(title = "Risk-Return Profile",
           x = "Annual Volatility (Risk)", y = "Annual Return", color = "Stock") +
      scale_x_continuous(labels = scales::percent_format()) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none") +
      annotate("text", x = max(risk_return$volatility) * 0.9,
               y = max(risk_return$annual_return) * 0.9,
               label = "Higher return,\nHigher risk",
               size = 3, color = "gray40", hjust = 1) +
      annotate("text", x = min(risk_return$volatility) * 1.1,
               y = min(risk_return$annual_return) * 1.1,
               label = "Lower return,\nLower risk",
               size = 3, color = "gray40", hjust = 0)
  })
}

# --- Run ---
shinyApp(ui = ui, server = server)
