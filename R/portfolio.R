compute_portfolio_returns <- function(price_data, weights) {
  returns_wide <- price_data %>%
    arrange(ticker, date) %>%
    group_by(ticker) %>%
    mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
    filter(!is.na(daily_return)) %>%
    select(date, ticker, daily_return) %>%
    pivot_wider(names_from = ticker, values_from = daily_return) %>%
    arrange(date) %>%
    drop_na()

  available <- intersect(names(weights), names(returns_wide))
  if (length(available) == 0) return(NULL)

  w <- weights[available]
  w <- w / sum(w)

  returns_matrix <- as.matrix(returns_wide[, available])
  portfolio_return <- as.numeric(returns_matrix %*% w)

  tibble(
    date = returns_wide$date,
    portfolio_return = portfolio_return,
    cumulative_return = cumprod(1 + portfolio_return) - 1
  )
}

compute_portfolio_metrics <- function(portfolio_returns, benchmark_returns = NULL,
                                      risk_free = 0) {
  r <- portfolio_returns$portfolio_return

  annual_return <- mean(r, na.rm = TRUE) * 252
  annual_vol    <- sd(r, na.rm = TRUE) * sqrt(252)
  sharpe        <- (annual_return - risk_free) / annual_vol

  cum      <- cumprod(1 + r)
  peak     <- cummax(cum)
  drawdown <- (cum - peak) / peak
  max_dd   <- min(drawdown)

  var_95  <- as.numeric(quantile(r, 0.05, na.rm = TRUE))
  cvar_95 <- mean(r[r <= var_95], na.rm = TRUE)

  downside_r   <- r[r < 0]
  downside_vol <- if (length(downside_r) > 1) sd(downside_r, na.rm = TRUE) * sqrt(252) else NA
  sortino      <- if (!is.na(downside_vol) && downside_vol > 0) annual_return / downside_vol else NA

  metrics <- list(
    annual_return     = annual_return,
    annual_volatility = annual_vol,
    sharpe_ratio      = sharpe,
    sortino_ratio     = sortino,
    max_drawdown      = max_dd,
    var_95            = var_95,
    cvar_95           = cvar_95,
    positive_days     = sum(r > 0) / length(r),
    best_day          = max(r, na.rm = TRUE),
    worst_day         = min(r, na.rm = TRUE)
  )

  if (!is.null(benchmark_returns)) {
    common_dates <- intersect(
      as.character(portfolio_returns$date),
      as.character(benchmark_returns$date)
    )
    p_aligned <- portfolio_returns %>%
      filter(as.character(date) %in% common_dates) %>% arrange(date)
    b_aligned <- benchmark_returns %>%
      filter(as.character(date) %in% common_dates) %>% arrange(date)

    if (nrow(p_aligned) > 10 && nrow(b_aligned) > 10) {
      excess <- p_aligned$portfolio_return - b_aligned$portfolio_return

      metrics$benchmark_return  <- mean(b_aligned$portfolio_return, na.rm = TRUE) * 252
      metrics$excess_return     <- mean(excess, na.rm = TRUE) * 252
      metrics$tracking_error    <- sd(excess, na.rm = TRUE) * sqrt(252)
      metrics$information_ratio <- if (metrics$tracking_error > 0) {
        metrics$excess_return / metrics$tracking_error
      } else NA

      cov_pb <- cov(p_aligned$portfolio_return, b_aligned$portfolio_return,
                     use = "complete.obs")
      var_b  <- var(b_aligned$portfolio_return, na.rm = TRUE)
      metrics$beta  <- if (var_b > 0) cov_pb / var_b else NA
      metrics$alpha <- metrics$annual_return - metrics$beta * metrics$benchmark_return
    }
  }

  metrics
}

compute_concentration_score <- function(weights) {
  w   <- weights / sum(weights)
  n   <- length(w)
  if (n <= 1) return(1)
  hhi <- sum(w^2)
  (hhi - 1 / n) / (1 - 1 / n)
}

get_sector_for_ticker <- function(ticker) {
  for (sector_name in names(stock_sectors)) {
    if (ticker %in% stock_sectors[[sector_name]]) return(sector_name)
  }
  "Other"
}

compute_sector_weights <- function(tickers, weights) {
  sectors <- sapply(tickers, get_sector_for_ticker)
  tibble(ticker = tickers, weight = weights, sector = sectors) %>%
    group_by(sector) %>%
    summarise(weight = sum(weight), .groups = "drop") %>%
    arrange(desc(weight))
}

generate_insights <- function(metrics, sector_weights) {
  insights <- character()

  if (!is.null(metrics$annual_return)) {
    if (metrics$annual_return >= 0) {
      insights <- c(insights, sprintf(
        "Portfolio generated a %.1f%% annual return over the analysis period.",
        metrics$annual_return * 100))
    } else {
      insights <- c(insights, sprintf(
        "Portfolio declined %.1f%% annually over the analysis period.",
        abs(metrics$annual_return) * 100))
    }
  }

  if (!is.null(metrics$excess_return)) {
    if (metrics$excess_return > 0) {
      insights <- c(insights, sprintf(
        "Portfolio outperformed the benchmark by %.1f%% annually.",
        metrics$excess_return * 100))
    } else {
      insights <- c(insights, sprintf(
        "Portfolio underperformed the benchmark by %.1f%% annually.",
        abs(metrics$excess_return) * 100))
    }
  }

  if (!is.null(metrics$sharpe_ratio)) {
    if (metrics$sharpe_ratio > 1) {
      insights <- c(insights, sprintf(
        "Sharpe ratio of %.2f indicates strong risk-adjusted returns.",
        metrics$sharpe_ratio))
    } else if (metrics$sharpe_ratio > 0.5) {
      insights <- c(insights, sprintf(
        "Sharpe ratio of %.2f suggests adequate risk-adjusted returns.",
        metrics$sharpe_ratio))
    } else if (metrics$sharpe_ratio > 0) {
      insights <- c(insights, sprintf(
        "Sharpe ratio of %.2f indicates below-average risk-adjusted returns.",
        metrics$sharpe_ratio))
    } else {
      insights <- c(insights,
        "Negative Sharpe ratio indicates the portfolio underperformed a risk-free asset.")
    }
  }

  if (nrow(sector_weights) > 0) {
    total_w    <- sum(sector_weights$weight)
    top_sector <- sector_weights %>% slice_max(weight, n = 1, with_ties = FALSE)
    pct        <- top_sector$weight / total_w * 100
    if (pct > 50) {
      insights <- c(insights, sprintf(
        "Portfolio is heavily concentrated in %s (%.0f%% of allocation).",
        top_sector$sector, pct))
    } else if (pct > 35) {
      insights <- c(insights, sprintf(
        "Largest sector exposure is %s at %.0f%% of the portfolio.",
        top_sector$sector, pct))
    }
  }

  if (!is.null(metrics$max_drawdown)) {
    if (abs(metrics$max_drawdown) > 0.20) {
      insights <- c(insights, sprintf(
        "Maximum drawdown of %.1f%% suggests significant downside risk.",
        abs(metrics$max_drawdown) * 100))
    } else {
      insights <- c(insights, sprintf(
        "Maximum drawdown was %.1f%%.",
        abs(metrics$max_drawdown) * 100))
    }
  }

  if (!is.null(metrics$beta)) {
    if (metrics$beta > 1.2) {
      insights <- c(insights, sprintf(
        "Beta of %.2f means this portfolio is more volatile than the benchmark.",
        metrics$beta))
    } else if (metrics$beta < 0.8) {
      insights <- c(insights, sprintf(
        "Beta of %.2f means this portfolio is less volatile than the benchmark.",
        metrics$beta))
    }
  }

  insights
}
