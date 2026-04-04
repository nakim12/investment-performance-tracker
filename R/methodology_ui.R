# Static methodology / limitations copy for the Methodology tab.

methodology_panel <- function() {
  tagList(
    div(class = "builder-header",
      h3("Methodology & limitations"),
      tags$p(class = "text-muted",
        "How metrics are computed in this app, and what not to infer from them."
      )
    ),

    wellPanel(
      h4("Data"),
      tags$ul(
        tags$li("Daily adjusted close prices from Yahoo Finance via ", tags$code("quantmod"), "."),
        tags$li("Your analysis window is the date range chosen on ", tags$b("Build Portfolio"), "."),
        tags$li("The Performance tab’s per-ticker charts use the tickers selected on ",
                tags$b("Price Trend"), " and about the last year of data (separate from the portfolio builder window).")
      )
    ),

    wellPanel(
      h4("Portfolio return"),
      tags$ul(
        tags$li("Weights are fixed over time: each day, portfolio return is the ",
                tags$b("weighted sum"), " of holding daily simple returns on aligned dates."),
        tags$li("Cumulative return is ", tags$code("cumprod(1 + r) - 1"), " on that series."),
        tags$li("This is ", tags$b("not"), " a full rebalance or cash-flow model; it is a standard back-of-the-envelope backtest.")
      )
    ),

    wellPanel(
      h4("Risk & ratios"),
      tags$ul(
        tags$li(tags$b("Annualized volatility:"), " sample standard deviation of daily returns × √252."),
        tags$li(tags$b("Sharpe:"), " (annualized mean return − risk-free) / annualized vol. Risk-free is assumed 0 here."),
        tags$li(tags$b("Sortino:"), " annualized mean / downside deviation (negative days only), annualized."),
        tags$li(tags$b("Max drawdown:"), " worst peak-to-trough on the cumulative wealth index."),
        tags$li(tags$b("VaR / CVaR (95%):"), " historical 5th percentile of daily returns and mean of returns on days at or below that threshold.")
      )
    ),

    wellPanel(
      h4("Benchmark-relative"),
      tags$ul(
        tags$li("Metrics use ", tags$b("overlapping trading days"), " only."),
        tags$li(tags$b("Beta:"), " Cov(portfolio, benchmark) / Var(benchmark) on daily returns."),
        tags$li(tags$b("Alpha (shown):"), " annualized portfolio return − beta × annualized benchmark return (simplified CAPM-style decomposition)."),
        tags$li(tags$b("Excess return:"), " mean daily difference × 252."),
        tags$li(tags$b("Tracking error:"), " annualized std dev of daily excess returns."),
        tags$li(tags$b("Information ratio:"), " annualized excess return / tracking error.")
      )
    ),

    wellPanel(
      h4("Concentration & diversification"),
      tags$ul(
        tags$li(tags$b("HHI-style concentration"), " on weights: normalized Herfindahl so 0 ≈ equal-weight, 1 ≈ single name."),
        tags$li(tags$b("Diversification ratio:"), " weighted average of individual annual volatilities / portfolio annual volatility (from the same return window)."),
        tags$li(tags$b("Risk contribution:"), " Euler / marginal contribution using the covariance matrix of daily returns and normalized weights."),
        tags$li(tags$b("Linear return attribution:"),
                " each name’s sum over days of (weight × daily return), as a share of the sum of daily portfolio returns. ",
                "It matches the fixed-weight daily model but is ", tags$b("not"), " the same as each name’s simple period return.")
      )
    ),

    wellPanel(
      h4("Sectors"),
      tags$p("Sector labels come from a fixed in-app universe (Technology, Financial, etc.). ",
             "Tickers outside that list are grouped as ", tags$b("Other"), ".")
    ),

    wellPanel(
      h4("Forecast tab"),
      tags$p(tags$b("ARIMA"), " on prices is ", tags$b("exploratory"), ": useful for methodology demos, not a trading signal. ",
             "Uncertainty and model error are real; treat forecasts as one input among many.")
    ),

    wellPanel(
      h4("Scenarios & stress"),
      tags$ul(
        tags$li(tags$b("Historical episodes:"), " slice your portfolio (and benchmark) to a calendar window and rebase cumulative return to the first day in that window. ",
                "Summary stats are descriptive only."),
        tags$li(tags$b("Additive daily shock:"), " the same fixed ", tags$b("basis-point"), " amount is added to each selected holding’s ",
                "daily simple return on every day in your analysis sample; the portfolio is recomputed with fixed weights. ",
                "This is a mechanical sensitivity tool, not a model of how markets behave under stress.")
      )
    ),

    wellPanel(
      h4("Disclaimer"),
      tags$p(style = "font-weight: 500;",
        "Educational software only — not investment, tax, or legal advice. ",
        "Past results do not guarantee future performance. You are responsible for your own decisions."
      )
    )
  )
}
