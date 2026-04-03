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

# ── Diagnosis computation functions ───────────────────────────────────────────

compute_drawdown_series <- function(portfolio_returns) {
  r    <- portfolio_returns$portfolio_return
  cum  <- cumprod(1 + r)
  peak <- cummax(cum)
  dd   <- (cum - peak) / peak

  tibble(date = portfolio_returns$date, drawdown = dd)
}

compute_rolling_metrics <- function(portfolio_returns, benchmark_returns = NULL,
                                    window = 30) {
  r     <- portfolio_returns$portfolio_return
  dates <- portfolio_returns$date
  n     <- length(r)

  if (n < window) return(NULL)

  idx <- window:n
  rolling_vol <- sapply(idx, function(i) {
    sd(r[(i - window + 1):i]) * sqrt(252)
  })
  rolling_sharpe <- sapply(idx, function(i) {
    chunk <- r[(i - window + 1):i]
    m <- mean(chunk); s <- sd(chunk)
    if (s == 0) return(NA)
    (m * 252) / (s * sqrt(252))
  })

  result <- tibble(
    date           = dates[idx],
    rolling_vol    = rolling_vol,
    rolling_sharpe = rolling_sharpe
  )

  if (!is.null(benchmark_returns)) {
    common <- intersect(as.character(portfolio_returns$date),
                        as.character(benchmark_returns$date))
    b_aligned <- benchmark_returns %>%
      filter(as.character(date) %in% common) %>% arrange(date)
    b  <- b_aligned$portfolio_return
    nb <- length(b)

    if (nb >= window) {
      b_idx <- window:nb
      bench_vol <- sapply(b_idx, function(i) {
        sd(b[(i - window + 1):i]) * sqrt(252)
      })
      bench_sharpe <- sapply(b_idx, function(i) {
        chunk <- b[(i - window + 1):i]
        m <- mean(chunk); s <- sd(chunk)
        if (s == 0) return(NA)
        (m * 252) / (s * sqrt(252))
      })
      bench_df <- tibble(
        date                 = b_aligned$date[b_idx],
        bench_rolling_vol    = bench_vol,
        bench_rolling_sharpe = bench_sharpe
      )
      result <- result %>% left_join(bench_df, by = "date")
    }
  }

  result
}

compute_risk_contribution <- function(price_data, weights) {
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
  if (length(available) < 2) return(NULL)

  w <- weights[available]
  w <- w / sum(w)

  returns_matrix <- as.matrix(returns_wide[, available])
  cov_matrix     <- cov(returns_matrix, use = "complete.obs")

  port_var <- as.numeric(t(w) %*% cov_matrix %*% w)
  port_vol <- sqrt(port_var)

  marginal  <- as.numeric(cov_matrix %*% w) / port_vol
  component <- w * marginal
  pct_contr <- component / sum(component)

  tibble(
    ticker           = available,
    weight           = as.numeric(w),
    marginal_ctr     = marginal  * sqrt(252),
    component_ctr    = component * sqrt(252),
    pct_contribution = pct_contr
  ) %>%
    arrange(desc(pct_contribution))
}

compute_diversification_ratio <- function(price_data, weights) {
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
  if (length(available) < 2) return(NA)

  w <- weights[available]
  w <- w / sum(w)

  returns_matrix  <- as.matrix(returns_wide[, available])
  individual_vols <- apply(returns_matrix, 2, sd) * sqrt(252)
  weighted_avg    <- sum(w * individual_vols)

  cov_ann  <- cov(returns_matrix, use = "complete.obs") * 252
  port_vol <- sqrt(as.numeric(t(w) %*% cov_ann %*% w))

  if (port_vol == 0) return(NA)
  weighted_avg / port_vol
}

compute_holding_correlations <- function(price_data) {
  returns_wide <- price_data %>%
    arrange(ticker, date) %>%
    group_by(ticker) %>%
    mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
    filter(!is.na(daily_return)) %>%
    select(date, ticker, daily_return) %>%
    pivot_wider(names_from = ticker, values_from = daily_return) %>%
    arrange(date) %>%
    drop_na()

  if (ncol(returns_wide) < 3) return(NULL)
  cor(returns_wide[, -1], use = "complete.obs")
}

# Linear attribution: each holding's sum_t(w_i * r_it) as a share of sum_t(portfolio return).
compute_return_attribution <- function(price_data, weights) {
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
  port_r         <- as.numeric(returns_matrix %*% w)

  linear_contrib <- as.numeric(w * colSums(returns_matrix))
  names(linear_contrib) <- available

  total_lin <- sum(port_r)
  if (abs(total_lin) < 1e-14) {
    linear_share <- rep(NA_real_, length(available))
  } else {
    linear_share <- linear_contrib / total_lin
  }

  period_tbl <- price_data %>%
    filter(ticker %in% available) %>%
    group_by(ticker) %>%
    summarise(
      period_total_return = (dplyr::last(adjusted) / dplyr::first(adjusted)) - 1,
      .groups = "drop"
    )

  tibble(
    ticker              = available,
    weight              = as.numeric(w),
    period_total_return = period_tbl$period_total_return[match(available, period_tbl$ticker)],
    linear_contribution = linear_contrib[available],
    linear_share        = linear_share[available]
  ) %>%
    arrange(desc(abs(linear_share)))
}

# ── Insight generators ────────────────────────────────────────────────────────

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

insight_from_correlation_matrix <- function(cor_matrix) {
  if (is.null(cor_matrix) || !is.matrix(cor_matrix) || nrow(cor_matrix) < 2) {
    return(character())
  }
  dn <- colnames(cor_matrix)
  if (is.null(dn)) dn <- rownames(cor_matrix)
  if (is.null(dn)) return(character())

  cm <- cor_matrix
  diag(cm) <- NA
  cm[lower.tri(cm)] <- NA
  mx <- max(abs(cm), na.rm = TRUE)
  if (!is.finite(mx) || mx < 0.72) return(character())

  widx <- which(abs(cm) == mx & !is.na(cm), arr.ind = TRUE)
  if (nrow(widx) < 1) return(character())

  i <- widx[1, 1]
  j <- widx[1, 2]
  val <- cm[i, j]
  sprintf(
    "%s and %s are highly correlated (%.2f), so they offer limited diversification from each other.",
    dn[i], dn[j], val
  )
}

generate_diagnosis_insights <- function(metrics, sector_weights,
                                        risk_contrib = NULL,
                                        diversification_ratio = NULL,
                                        holding_cor = NULL) {
  base <- generate_insights(metrics, sector_weights)
  extra <- character()

  if (!is.null(metrics$tracking_error) && !is.na(metrics$tracking_error)) {
    if (metrics$tracking_error > 0.12) {
      extra <- c(extra, sprintf(
        "Tracking error is %.1f%% annualized — returns diverge meaningfully from the benchmark.",
        metrics$tracking_error * 100))
    }
  }

  if (!is.null(holding_cor)) {
    cor_line <- insight_from_correlation_matrix(holding_cor)
    if (length(cor_line)) extra <- c(extra, cor_line)
  }

  if (!is.null(diversification_ratio) && !is.na(diversification_ratio)) {
    if (diversification_ratio > 1.5) {
      extra <- c(extra, sprintf(
        "Diversification ratio of %.2f indicates strong diversification benefit across holdings.",
        diversification_ratio))
    } else if (diversification_ratio > 1.1) {
      extra <- c(extra, sprintf(
        "Diversification ratio of %.2f indicates moderate diversification benefit.",
        diversification_ratio))
    } else {
      extra <- c(extra, sprintf(
        "Diversification ratio of %.2f suggests limited diversification — holdings may be highly correlated.",
        diversification_ratio))
    }
  }

  if (!is.null(risk_contrib) && nrow(risk_contrib) > 1) {
    top_risk <- risk_contrib %>% slice_max(pct_contribution, n = 1, with_ties = FALSE)
    if (top_risk$pct_contribution > 0.4) {
      extra <- c(extra, sprintf(
        "%s contributes %.0f%% of total portfolio risk, making it the dominant risk driver.",
        top_risk$ticker, top_risk$pct_contribution * 100))
    }
    if (nrow(risk_contrib) >= 2) {
      top2 <- risk_contrib %>% slice_max(pct_contribution, n = 2, with_ties = FALSE)
      if (sum(top2$pct_contribution) > 0.7) {
        extra <- c(extra, sprintf(
          "%s and %s together account for %.0f%% of portfolio risk.",
          top2$ticker[1], top2$ticker[2], sum(top2$pct_contribution) * 100))
      }
    }
  }

  c(base, extra)
}
