library(shiny)
library(shinycssloaders)
library(quantmod)
library(zoo)
library(forecast)
library(tidyr)
library(scales)
library(reshape2)
library(dplyr)
library(ggplot2)

options(xts.warn_dplyr_breaks_lag = FALSE)

stock_sectors <- list(
  "Technology" = c("AAPL", "MSFT", "GOOGL", "META", "NVDA", "AMZN"),
  "Financial" = c("JPM", "BAC", "WFC", "GS", "MS", "V"),
  "Healthcare" = c("JNJ", "PFE", "UNH", "CVS", "ABBV", "MRK"),
  "Consumer" = c("WMT", "HD", "NKE", "MCD", "SBUX", "TGT"),
  "Energy" = c("XOM", "CVX", "COP", "SLB", "OXY", "PSX")
)

all_stocks <- unlist(stock_sectors)
names(all_stocks) <- all_stocks

validate_data <- function(df, context = "data") {
  validate(
    need(!is.null(df) && nrow(df) > 0, 
         paste("No", context, "available. Please check your selection.")),
    need("adjusted" %in% names(df), 
         "Price data not found in the dataset.")
  )
}

# --- UI ---

ui <- navbarPage("Investment Performance Tracker",
                 tabPanel("Price Trend",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("sector_filter", "Filter by Sector:",
                                          choices = c("All Sectors" = "all", names(stock_sectors)),
                                          selected = "all"),
                              
                              selectInput("tickers", "Select Stocks to Compare:", 
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE),
                              
                              actionButton("select_all", "Select All", 
                                           class = "btn-sm btn-primary",
                                           style = "margin-bottom: 10px;"),
                              actionButton("clear_all", "Clear All", 
                                           class = "btn-sm btn-warning",
                                           style = "margin-bottom: 10px;"),
                              
                              sliderInput("ma_window",
                                          "Moving Average Window (days):",
                                          min = 5,
                                          max = 100,
                                          value = 20),
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
                 
                 tabPanel("Returns",
                          mainPanel(
                            withSpinner(plotOutput("returnsPlot"), type = 4, color = "darkturquoise"),
                            hr(),
                            withSpinner(plotOutput("returnDistribution"), type = 4, color = "darkturquoise")
                          )
                 ),
                 
                 tabPanel("Cumulative Returns",
                          mainPanel(
                            withSpinner(plotOutput("cumulativePlot"), type = 4, color = "darkturquoise"),
                            hr(),
                            h4("Performance Summary"),
                            withSpinner(tableOutput("performanceSummary"), type = 4, color = "darkturquoise")
                          )
                 ),
                 
                 tabPanel("Forecast",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("forecast_sector", "Filter by Sector:",
                                          choices = c("All Sectors" = "all", names(stock_sectors)),
                                          selected = "all"),
                              selectInput("forecast_ticker", "Select Ticker to Forecast:",
                                          choices = NULL),
                              sliderInput("horizon", "Forecast Horizon (days):", 
                                          min = 5, max = 90, value = 30),
                              hr(),
                              radioButtons("forecast_mode", "Forecast Mode:",
                                           choices = c("Conservative (Recommended)" = "conservative",
                                                       "Aggressive (Less Reliable)" = "aggressive"),
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
                 
                 tabPanel("Risk Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("risk_sector", "Filter by Sector:",
                                          choices = c("All Sectors" = "all", names(stock_sectors)),
                                          selected = "all"),
                              selectInput("risk_tickers", "Select Stocks for Analysis:", 
                                          choices = NULL, 
                                          selected = NULL, 
                                          multiple = TRUE),
                              dateRangeInput("risk_date_range", 
                                             "Analysis Period:", 
                                             start = Sys.Date() - 365, 
                                             end = Sys.Date(),
                                             max = Sys.Date()),
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
                              withSpinner(plotOutput("correlationHeatmap", height = "400px"), type = 4, color = "darkturquoise"),
                              hr(),
                              h3("Risk-Return Scatter"),
                              withSpinner(plotOutput("riskReturnPlot", height = "400px"), type = 4, color = "darkturquoise")
                            )
                          )
                 )
)

# --- Server ---

server <- function(input, output, session) {
  
  observe({
    selected_sector <- input$sector_filter
    
    if (selected_sector == "all") {
      updateSelectInput(session, "tickers", 
                        choices = all_stocks,
                        selected = input$tickers)
    } else {
      sector_stocks <- stock_sectors[[selected_sector]]
      updateSelectInput(session, "tickers", 
                        choices = sector_stocks,
                        selected = sector_stocks[1:min(2, length(sector_stocks))])
    }
  })
  
  observe({
    selected_sector <- input$forecast_sector
    
    if (selected_sector == "all") {
      updateSelectInput(session, "forecast_ticker", choices = all_stocks)
    } else {
      updateSelectInput(session, "forecast_ticker", choices = stock_sectors[[selected_sector]])
    }
  })
  
  observe({
    selected_sector <- input$risk_sector
    
    if (selected_sector == "all") {
      updateSelectInput(session, "risk_tickers", 
                        choices = all_stocks,
                        selected = input$risk_tickers)
    } else {
      sector_stocks <- stock_sectors[[selected_sector]]
      updateSelectInput(session, "risk_tickers", 
                        choices = sector_stocks,
                        selected = sector_stocks[1:min(2, length(sector_stocks))])
    }
  })
  
  observeEvent(input$select_all, {
    sector <- input$sector_filter
    
    if (sector == "all") {
      choices <- all_stocks
    } else {
      choices <- stock_sectors[[sector]]
    }
    
    updateSelectInput(session, "tickers",
                      selected = choices)
  })
  
  observeEvent(input$clear_all, {
    updateSelectInput(session, "tickers",
                      selected = character(0))
  })
  
  stock_data <- reactive({
    # Return NULL silently if no tickers are selected
    if (is.null(input$tickers) || length(input$tickers) == 0) {
      return(NULL)
    }
    
    withProgress(message = 'Loading stock data...', value = 0, {
      data_list <- lapply(seq_along(input$tickers), function(i) {
        ticker <- input$tickers[i]
        incProgress(1 / length(input$tickers), detail = paste("Loading", ticker))
        
        tryCatch({
          df <- getSymbols(ticker, src = "yahoo",
                           from = Sys.Date() - 365,
                           to = Sys.Date(),
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
    })
  })
  
  forecast_data <- reactive({
    if (is.null(input$forecast_ticker) || input$forecast_ticker == "") {
      showNotification("Please select a ticker for forecasting", type = "error", duration = 5)
      return(tibble())
    }
    
    withProgress(message = paste('Loading data for', input$forecast_ticker), value = 0.5, {
      tryCatch({
        df <- getSymbols(input$forecast_ticker, src = "yahoo", 
                         from = Sys.Date() - 365,
                         to = Sys.Date(),
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
        
        if (sum(!is.na(df$adjusted)) < 30) {
          showNotification("Insufficient data for forecasting", type = "warning")
        }
        
        return(df)
      }, error = function(e) {
        showNotification(paste("Failed to load data for forecast:", e$message), type = "error")
        return(tibble())
      })
    })
  })
  
  output$dataAvailability <- renderText({
    df <- forecast_data()
    if (nrow(df) == 0) {
      return("No data available")
    }
    
    days_available <- nrow(df)
    
    if (input$forecast_mode == "conservative") {
      min_for_forecast <- max(50, input$horizon * 2)  # Conservative: 2x horizon
    } else {
      min_for_forecast <- max(30, input$horizon * 0.5)  # Aggressive: 0.5x horizon
    }
    min_for_backtest <- input$horizon + 60
    
    status <- if (days_available < min_for_forecast) {
      paste("❌ Insufficient:", days_available, "days available,", min_for_forecast, "needed")
    } else if (days_available < min_for_backtest) {
      paste("⚠️ Limited:", days_available, "days (can forecast but not backtest)")
    } else {
      paste("✓ Good:", days_available, "days of data available")
    }
    
    return(status)
  })
  
  # --- Price Plot ---
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
      mutate(
        ma = if(n() >= ma_window) {
          rollmean(adjusted, k = ma_window, fill = NA, align = "right")
        } else {
          NA
        }
      ) %>%
      ungroup()
    
    insufficient_data <- df %>%
      group_by(ticker) %>%
      summarise(has_ma = any(!is.na(ma))) %>%
      filter(!has_ma)
    
    if (nrow(insufficient_data) > 0) {
      showNotification(
        paste("Insufficient data for", ma_window, "day MA for:", 
              paste(insufficient_data$ticker, collapse = ", ")),
        type = "warning"
      )
    }
    
    ggplot(df, aes(x = date, y = adjusted, color = ticker)) +
      geom_line(alpha = 0.7, linewidth = 1) +
      geom_line(aes(y = ma), linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
      labs(title = paste("Adjusted Price with", ma_window, "Day Moving Average"),
           x = "Date", y = "Adjusted Price",
           color = "Stock") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  # --- Daily Returns ---
  output$returnsPlot <- renderPlot({
    df <- stock_data()
    validate_data(df, "returns data")
    
    df <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      ungroup()
    
    if (sum(!is.na(df$daily_return)) == 0) {
      validate("No return data available")
    }
    
    ggplot(df, aes(x = date, y = daily_return, color = ticker)) +
      geom_line(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(title = "Daily Returns by Ticker",
           x = "Date", y = "Daily Return",
           color = "Stock") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  # --- Return Distribution Plot ---
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
           x = "Daily Return", y = "Frequency",
           fill = "Stock") +
      scale_x_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 12)
  })
  
  # --- Cumulative Returns ---
  output$cumulativePlot <- renderPlot({
    df <- stock_data()
    validate_data(df, "cumulative returns data")
    
    df <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(
        daily_return = adjusted / dplyr::lag(adjusted) - 1,
        cumulative_return = cumprod(1 + replace_na(daily_return, 0)) - 1
      ) %>%
      filter(!is.na(cumulative_return)) %>%
      ungroup()
    
    ggplot(df, aes(x = date, y = cumulative_return, color = ticker)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      labs(
        title = "Cumulative Returns (Growth of $1 Invested)",
        x = "Date", y = "Cumulative Return",
        color = "Stock"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  # --- Performance Summary Table ---
  output$performanceSummary <- renderTable({
    df <- stock_data()
    validate_data(df, "performance data")
    
    summary_stats <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      summarise(
        `Total Return` = sprintf("%.2f%%", (last(adjusted) / first(adjusted) - 1) * 100),
        `Volatility (Annual)` = sprintf("%.2f%%", sd(daily_return) * sqrt(252) * 100),
        `Sharpe Ratio` = sprintf("%.2f", mean(daily_return) / sd(daily_return) * sqrt(252)),
        `Best Day` = sprintf("%.2f%%", max(daily_return) * 100),
        `Worst Day` = sprintf("%.2f%%", min(daily_return) * 100),
        `Current Price` = sprintf("$%.2f", last(adjusted))
      ) %>%
      rename(Stock = ticker)
    
    summary_stats
  }, align = 'c')
  
  # --- Forecast Plot ---
  output$forecastPlot <- renderPlot({
    df <- forecast_data()
    
    if (nrow(df) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "No data to display")
      text(1, 1, "Please select a ticker for forecasting", cex = 1.5, col = "gray40")
      return()
    }
    
    validate_data(df, "forecast data")
    
    if (input$forecast_mode == "conservative") {
      min_required <- max(50, input$horizon * 2)  # Conservative mode
    } else {
      min_required <- max(30, input$horizon * 0.5)  # Aggressive mode
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
    
    withProgress(message = 'Generating forecast...', value = 0.5, {
      ts_data <- ts(df$adjusted, frequency = 252)
      
      tryCatch({
        model <- auto.arima(ts_data)
        forecasted <- forecast(model, h = input$horizon)
        
        autoplot(forecasted) +
          labs(
            title = paste(input$forecast_ticker, "Price Forecast (Next", input$horizon, "Days)"),
            x = "Time (Trading Days)", y = "Adjusted Price ($)"
          ) +
          theme_minimal(base_size = 12)
      }, error = function(e) {
        showNotification(paste("Forecast error:", e$message), type = "error")
        validate("Unable to generate forecast. Please try a different ticker or horizon.")
      })
    })
  })
  
  # --- Forecast Evaluation ---
  output$forecastMetrics <- renderPrint({
    df <- forecast_data()
    
    if (nrow(df) == 0) {
      cat("No data available for evaluation\n")
      return()
    }
    
    horizon <- input$horizon
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
    
    withProgress(message = 'Evaluating forecast accuracy...', value = 0.5, {
      df <- df %>% arrange(date)
      train <- head(df$adjusted, -horizon)
      test <- tail(df$adjusted, horizon)
      
      if (length(train) < 30) {
        cat("Insufficient training data for evaluation\n")
        return()
      }
      
      ts_train <- ts(train, frequency = 252)
      
      tryCatch({
        model <- auto.arima(ts_train)
        fc <- forecast(model, h = horizon)
        
        rmse <- sqrt(mean((fc$mean - test)^2, na.rm = TRUE))
        mae <- mean(abs(fc$mean - test), na.rm = TRUE)
        mape <- mean(abs((fc$mean - test) / test), na.rm = TRUE) * 100
        
        cat("Forecast Model: ARIMA", paste0("(", paste(model$arma[c(1,6,2)], collapse = ","), ")\n"))
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
  
  # --- Forecast Accuracy Plot ---
  output$forecastAccuracyPlot <- renderPlot({
    df <- forecast_data()
    
    if (nrow(df) == 0) {
      return()
    }
    
    horizon <- input$horizon
    min_required <- horizon + 60
    
    if (nrow(df) <= min_required) {
      plot(1, type = "n", xlab = "", ylab = "", 
           main = "Cannot Generate Accuracy Plot")
      text(1, 1, paste("Need", min_required - nrow(df), "more days of data\n",
                       "to show forecast vs actual comparison"), 
           cex = 1.2, col = "gray40")
      return()
    }
    
    df <- df %>% arrange(date)
    train <- head(df$adjusted, -horizon)
    test <- tail(df$adjusted, horizon)
    test_dates <- tail(df$date, horizon)
    
    if (length(train) < 30) {
      validate("Insufficient training data for accuracy plot")
    }
    
    withProgress(message = 'Creating accuracy plot...', value = 0.5, {
      ts_train <- ts(train, frequency = 252)
      
      tryCatch({
        model <- auto.arima(ts_train)
        fc <- forecast(model, h = horizon)
        
        forecast_df <- data.frame(
          date = test_dates,
          actual = as.numeric(test),
          forecast = as.numeric(fc$mean),
          lower_80 = fc$lower[, 1],
          upper_80 = fc$upper[, 1],
          lower_95 = fc$lower[, 2],
          upper_95 = fc$upper[, 2]
        )
        
        forecast_df$residual <- forecast_df$actual - forecast_df$forecast
        
        ggplot(forecast_df, aes(x = date)) +
          geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "lightblue", alpha = 0.3) +
          geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "lightblue", alpha = 0.5) +
          geom_line(aes(y = actual), color = "red", linewidth = 1.2) +
          geom_line(aes(y = forecast), color = "blue", linetype = "dashed", linewidth = 1) +
          geom_point(aes(y = actual), color = "red", size = 2, alpha = 0.5) +
          geom_point(aes(y = forecast), color = "blue", size = 2, alpha = 0.5) +
          labs(
            title = paste(input$forecast_ticker, "Forecast vs. Actual (Last", horizon, "Days)"),
            x = "Date", y = "Adjusted Price ($)",
            caption = "Red = Actual | Blue = Forecast | Shaded = Confidence Intervals"
          ) +
          scale_y_continuous(labels = scales::dollar_format()) +
          theme_minimal(base_size = 12)
      }, error = function(e) {
        showNotification(paste("Error creating accuracy plot:", e$message), type = "error")
        validate("Unable to create accuracy plot")
      })
    })
  })
  
  # --- Risk Analysis Functions ---
  
  risk_data <- reactive({
    req(input$risk_tickers)
    
    if (length(input$risk_tickers) == 0) {
      showNotification("Please select at least one ticker for risk analysis", type = "error")
      return(tibble())
    }
    
    withProgress(message = 'Loading risk analysis data...', value = 0, {
      data_list <- lapply(seq_along(input$risk_tickers), function(i) {
        ticker <- input$risk_tickers[i]
        incProgress(1/length(input$risk_tickers), detail = paste("Loading", ticker))
        
        tryCatch({
          df <- getSymbols(ticker, src = "yahoo", 
                           from = input$risk_date_range[1], 
                           to = input$risk_date_range[2],
                           auto.assign = FALSE)
          
          if (is.null(df) || nrow(df) == 0) {
            return(NULL)
          }
          
          df <- fortify.zoo(df) %>% as_tibble()
          colnames(df)[1] <- "date"
          
          if (ncol(df) >= 6) {
            names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
          }
          
          df$ticker <- ticker
          df
        }, error = function(e) {
          NULL
        })
      })
    })
    
    data_list <- data_list[!sapply(data_list, is.null)]
    if (length(data_list) == 0) return(tibble())
    
    bind_rows(data_list)
  })
  
  output$riskMetricsTable <- renderTable({
    df <- risk_data()
    validate_data(df, "risk analysis")
    
    metrics <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(
        daily_return = (adjusted / dplyr::lag(adjusted)) - 1,
        cumulative_return = cumprod(1 + replace_na(daily_return, 0)) - 1
      ) %>%
      filter(!is.na(daily_return)) %>%
      summarise(
        `Annual Return` = mean(daily_return, na.rm = TRUE) * 252,
        `Volatility` = sd(daily_return, na.rm = TRUE) * sqrt(252),
        `Sharpe Ratio` = `Annual Return` / `Volatility`,
        `Max Drawdown` = min(cumulative_return, na.rm = TRUE),
        `VaR (95%)` = quantile(daily_return, 0.05, na.rm = TRUE),
        `Best Day` = max(daily_return, na.rm = TRUE),
        `Worst Day` = min(daily_return, na.rm = TRUE),
        `Positive Days` = sum(daily_return > 0, na.rm = TRUE) / n()
      ) %>%
      mutate(
        across(c(`Annual Return`, `Volatility`, `Max Drawdown`, `VaR (95%)`, 
                 `Best Day`, `Worst Day`, `Positive Days`), 
               ~ sprintf("%.2f%%", . * 100)),
        `Sharpe Ratio` = sprintf("%.2f", `Sharpe Ratio`)
      ) %>%
      rename(Stock = ticker)
    
    metrics
  }, align = 'c', striped = TRUE, hover = TRUE)
  
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
      text(1, 1, "Select at least 2 stocks for correlation analysis", cex = 1.2, col = "gray40")
      return()
    }
    
    cor_matrix <- cor(returns_wide[,-1], use = "complete.obs")
    melted_cor <- melt(cor_matrix)
    
    ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +
      scale_fill_gradient2(low = "#3498db", high = "#e74c3c", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name = "Correlation") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title = element_blank(),
            panel.grid = element_blank()) +
      labs(title = "Stock Return Correlation Matrix") +
      coord_fixed()
  })
  
  # --- Risk-Return Plot ---
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
        volatility = sd(daily_return, na.rm = TRUE) * sqrt(252)
      )
    
    ggplot(risk_return, aes(x = volatility, y = annual_return)) +
      geom_point(aes(color = ticker), size = 5, alpha = 0.7) +
      geom_text(aes(label = ticker), vjust = -1.5, size = 4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      labs(
        title = "Risk-Return Profile",
        x = "Annual Volatility (Risk)",
        y = "Annual Return",
        color = "Stock"
      ) +
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

# Run the application 
shinyApp(ui = ui, server = server)
