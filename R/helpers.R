validate_data <- function(df, context = "data") {
  validate(
    need(!is.null(df) && nrow(df) > 0,
         paste("No", context, "available. Please check your selection.")),
    need("adjusted" %in% names(df),
         "Price data not found in the dataset.")
  )
}

fetch_stock_data <- function(tickers, from_date = Sys.Date() - 365, to_date = Sys.Date()) {
  data_list <- lapply(tickers, function(ticker) {
    tryCatch({
      df <- getSymbols(ticker, src = "yahoo",
                       from = from_date,
                       to = to_date,
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
    }, error = function(e) {
      NULL
    })
  })

  data_list <- data_list[!sapply(data_list, is.null)]
  if (length(data_list) == 0) return(NULL)

  bind_rows(data_list)
}

compute_daily_returns <- function(df) {
  df %>%
    arrange(ticker, date) %>%
    group_by(ticker) %>%
    mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
    ungroup()
}
