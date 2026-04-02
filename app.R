# Libraries, config, and helper functions are auto-loaded from R/ directory

# --- Custom CSS ---
app_css <- "
.kpi-card {
  text-align: center;
  padding: 18px 10px;
  background: #f8f9fa;
  border-radius: 8px;
  margin-bottom: 15px;
  border: 1px solid #e9ecef;
}
.kpi-value {
  font-size: 22px;
  font-weight: 700;
  margin-bottom: 4px;
}
.kpi-label {
  font-size: 11px;
  color: #6c757d;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}
.kpi-positive { color: #28a745; }
.kpi-negative { color: #dc3545; }
.kpi-neutral  { color: #495057; }
.insight-panel {
  background: #f0f7ff;
  border-left: 4px solid #007bff;
  padding: 15px 20px;
  border-radius: 4px;
  margin-top: 15px;
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
  color: #999;
}
.weight-ok    { color: #28a745; font-weight: bold; }
.weight-warn  { color: #ffc107; font-weight: bold; }
"

# --- UI ---

ui <- navbarPage(
  title = "Portfolio Intelligence Lab",
  id    = "main_nav",
  header = tags$head(
    tags$style(HTML(app_css)),
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
        withSpinner(plotOutput("pricePlot"), type = 4, color = "darkturquoise")
      )
    )
  ),

  # ── Tab 5: Performance (daily + cumulative returns) ───────────────────────

  tabPanel("Performance",
    mainPanel(
      h3("Cumulative performance"),
      tags$p(class = "text-muted",
        "Growth of $1 invested over the selected period (same tickers as Price Trend)."
      ),
      withSpinner(plotOutput("cumulativePlot", height = "400px"),
        type = 4, color = "darkturquoise"),
      hr(),
      h3("Daily returns"),
      withSpinner(plotOutput("returnsPlot", height = "380px"),
        type = 4, color = "darkturquoise"),
      hr(),
      h3("Return distributions"),
      withSpinner(plotOutput("returnDistribution", height = "400px"),
        type = 4, color = "darkturquoise"),
      hr(),
      h3("Performance summary"),
      withSpinner(tableOutput("performanceSummary"), type = 4, color = "darkturquoise")
    )
  ),

  # ── Tab 6: Forecast ─────────────────────────────────────────────────────────

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
        withSpinner(plotOutput("forecastPlot"), type = 4, color = "darkturquoise"),
        br(),
        wellPanel(
          h4("Model Evaluation Metrics"),
          verbatimTextOutput("forecastMetrics")
        ),
        withSpinner(plotOutput("forecastAccuracyPlot"), type = 4, color = "darkturquoise")
      )
    )
  ),

  # ── Tab 7: Risk Analysis ────────────────────────────────────────────────────

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
        withSpinner(tableOutput("riskMetricsTable"), type = 4, color = "darkturquoise"),
        hr(),
        h3("Correlation Analysis"),
        withSpinner(plotOutput("correlationHeatmap", height = "400px"),
          type = 4, color = "darkturquoise"),
        hr(),
        h3("Risk-Return Scatter"),
        withSpinner(plotOutput("riskReturnPlot", height = "400px"),
          type = 4, color = "darkturquoise")
      )
    )
  )
)

# --- Server ---

server <- function(input, output, session) {

  # ══════════════════════════════════════════════════════════════════════════

  # PORTFOLIO BUILDER
  # ══════════════════════════════════════════════════════════════════════════

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
    holding_cor       = NULL
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
            type = 4, color = "darkturquoise")
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

    insights <- generate_diagnosis_insights(m, sw, rc, dr)

    tagList(
      div(class = "builder-header",
        h3("Portfolio Diagnosis"),
        tags$p(class = "text-muted",
          sprintf("Analysis of %d holdings over the selected period against %s.",
                  nrow(portfolio$holdings), portfolio$benchmark_name))
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

      # ── Insights ──
      if (length(insights) > 0) {
        div(class = "insight-panel", style = "margin-top: 20px;",
          h4("Portfolio Insights"),
          tags$ul(lapply(insights, tags$li))
        )
      },

      # ── Performance Section ──
      tags$h4(style = "border-bottom: 2px solid #eee; padding-bottom: 8px; margin-top: 30px;",
        "Performance"),
      fluidRow(
        column(7, withSpinner(plotOutput("diag_perf_chart", height = "380px"),
          type = 4, color = "darkturquoise")),
        column(5, withSpinner(plotOutput("diag_drawdown_chart", height = "380px"),
          type = 4, color = "darkturquoise"))
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
          type = 4, color = "darkturquoise")),
        column(6, withSpinner(plotOutput("diag_rolling_sharpe_chart", height = "350px"),
          type = 4, color = "darkturquoise"))
      ),

      # ── Risk & Diversification Section ──
      tags$h4(style = "border-bottom: 2px solid #eee; padding-bottom: 8px; margin-top: 30px;",
        "Risk & Diversification"),
      fluidRow(
        column(6, withSpinner(plotOutput("diag_risk_contrib_chart", height = "380px"),
          type = 4, color = "darkturquoise")),
        column(6, withSpinner(plotOutput("diag_corr_heatmap", height = "380px"),
          type = 4, color = "darkturquoise"))
      ),
      fluidRow(
        column(6, withSpinner(plotOutput("diag_sector_chart", height = "350px"),
          type = 4, color = "darkturquoise")),
        column(6,
          wellPanel(style = "margin-top: 15px;",
            h4("Diversification Score"),
            tags$div(class = "kpi-card", style = "background: white;",
              tags$div(class = paste("kpi-value",
                if (!is.na(dr) && dr > 1.3) "kpi-positive" else
                if (!is.na(dr) && dr > 1.0) "kpi-neutral"  else "kpi-negative"),
                if (!is.na(dr)) sprintf("%.2f", dr) else "N/A"),
              tags$div(class = "kpi-label", "Diversification Ratio")
            ),
            tags$p(class = "text-muted", style = "margin-top: 10px; font-size: 13px;",
              "Values above 1.0 indicate diversification benefit.",
              "Higher values mean holdings are less correlated,",
              "reducing portfolio risk below the weighted average of individual risks."
            )
          )
        )
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
    m <- portfolio$metrics

    base <- tibble(
      Metric = c("Annual Return", "Annual Volatility", "Sharpe Ratio",
                  "Sortino Ratio", "Max Drawdown", "VaR (95%)", "CVaR (95%)",
                  "Best Day", "Worst Day", "Positive Days"),
      Value  = c(
        sprintf("%.2f%%", m$annual_return * 100),
        sprintf("%.2f%%", m$annual_volatility * 100),
        sprintf("%.2f",   m$sharpe_ratio),
        if (!is.na(m$sortino_ratio)) sprintf("%.2f", m$sortino_ratio) else "N/A",
        sprintf("%.2f%%", m$max_drawdown * 100),
        sprintf("%.2f%%", m$var_95 * 100),
        sprintf("%.2f%%", m$cvar_95 * 100),
        sprintf("%.2f%%", m$best_day * 100),
        sprintf("%.2f%%", m$worst_day * 100),
        sprintf("%.1f%%", m$positive_days * 100)
      )
    )

    if (!is.null(m$beta)) {
      bench <- tibble(
        Metric = c("Beta", "Alpha", "Excess Return", "Tracking Error",
                    "Information Ratio"),
        Value  = c(
          sprintf("%.2f", m$beta),
          sprintf("%.2f%%", m$alpha * 100),
          sprintf("%.2f%%", m$excess_return * 100),
          sprintf("%.2f%%", m$tracking_error * 100),
          if (!is.na(m$information_ratio)) sprintf("%.2f", m$information_ratio) else "N/A"
        )
      )
      base <- bind_rows(base, bench)
    }

    base
  }, striped = TRUE, hover = TRUE, align = "lr", width = "100%")


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
