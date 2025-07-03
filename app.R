
library(shiny)
library(quantmod)
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)
library(tidyr)

tickers <- c("AAPL", "MSFT", "GOOGL")

# --- UI ---

ui <- navbarPage("Investment Performance Tracker",
                 tabPanel("Price Trend",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("tickers", "Select Stocks to Compare:", choices = tickers,
                                          selected = c("AAPL", "MSFT"),
                                          multiple = TRUE),
                              sliderInput("ma_window",
                                          "Moving Average Window (days):",
                                          min = 5,
                                          max = 100,
                                          value = 20)
                              ),
                            mainPanel(
                              plotOutput("pricePlot")
                            )
                          )
                 ),
                 tabPanel("Returns",
                          mainPanel(
                            plotOutput("returnsPlot")
                          )
                 ),
                 tabPanel("Forecast",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("forecast_ticker", "Select Ticker to Forecast:",
                                          choices = tickers, selected = "AAPL"),
                              sliderInput("horizon", "Forecast Horizon (days):", min = 5, max = 90, value = 30)
                            ),
                            mainPanel(
                              plotOutput("forecastPlot"),
                              verbatimTextOutput("forecastMetrics"),
                              plotOutput("forecastAccuracyPlot")
                            )
                          )
                          ),
                 tabPanel("Cumulative Returns",
                          mainPanel(
                            plotOutput("cumulativePlot")
                          )
                 )
)

# --- Server ---

server <- function(input, output) {
  
  # Pull and combine data for selected tickers

  stock_data <- reactive({
    req(input$tickers)
    
    if (length(input$tickers) == 0) {
      return(tibble())
    }
    
    data_list <- lapply(input$tickers, function(ticker) {
      tryCatch({
        df <- getSymbols(ticker, src = "yahoo", from = Sys.Date() - 365, auto.assign = FALSE)
        df <- fortify.zoo(df) %>% as_tibble()
        colnames(df)[1] <- "date"
        
        if (ncol(df) == 7) {
          names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
        }
        
        df$ticker <- ticker
        df
      }, error = function(e) {
        message(paste("Failed to load", ticker, ":", e$message))
        NULL
      })
    })
    
    bind_rows(data_list)
  })
  
  # Separate forecast dataset (single ticker)
  
  forecast_data <- reactive({
    req(input$forecast_ticker)
    
    tryCatch({
      df <- getSymbols(input$forecast_ticker, src = "yahoo", from = Sys.Date() - 365, auto.assign = FALSE)
      df <- fortify.zoo(df) %>% as_tibble()
      colnames(df)[1] <- "date"
      
      if (ncol(df) == 7) {
        names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
      }
      
      df <- df %>%
        arrange(date)
      
      return(df)
    }, error = function(e) {
      message(paste("Failed to load data for forecast:", input$forecast_ticker, e$message))
      return(tibble())
    })
  })
    
  # --- Price Plot ---
  
    output$pricePlot <- renderPlot({
      df <- stock_data() %>%
        arrange(date) %>%
        group_by(ticker) %>%
        mutate(ma = rollmean(adjusted, k = input$ma_window, fill = NA, align = "right")) %>%
        ungroup()
      
      ggplot(df, aes(x = date, y = adjusted, color = ticker)) +
        geom_line(alpha = 0.5) +
        geom_line(aes(y = ma), linetype = "dashed") +
        labs(title = paste("Adjusted Price with", input$ma_window, "Day MA"),
             x = "Date", y = "Adjusted Price") +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme_minimal()
    })
    
    # --- Daily Returns ---
    
    output$returnsPlot <- renderPlot({
      df <- stock_data() %>%
        arrange(ticker, date) %>%
        group_by(ticker) %>%
        mutate(daily_return = (adjusted / lag(adjusted)) - 1) %>%
        ungroup()
      
      ggplot(df, aes(x = date, y = daily_return, color = ticker)) +
        geom_line(alpha = 0.6) +
        labs(title = "Daily Returns by Ticker",
             x = "Date", y = "Daily Return") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        theme_minimal()
    })
    
    # --- Cumulative Returns ---
    
    output$cumulativePlot <- renderPlot({
      df <- stock_data()
      req(nrow(df) > 0)
      
      df <- df %>%
        arrange(ticker, date) %>%
        group_by(ticker) %>%
        mutate(
          daily_return = adjusted / lag(adjusted) - 1,
          cumulative_return = cumprod(1 + replace_na(daily_return, 0))
        ) %>%
        ungroup()
      
      p <- ggplot(df, aes(x = date, y = cumulative_return, color = ticker)) +
        geom_line(size = 1, alpha = 0.8) +
        labs(
          title = "Normalized Cumulative Returns",
          x = "Date", y = "Growth of $1 Invested"
        ) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme_minimal(base_size = 14)
      
      return(p)
    })
    
    # --- Forecast Plot ---
    
    output$forecastPlot <- renderPlot({
      df <- forecast_data()
      req(nrow(df) > 2)
      
      
      if (!"adjusted" %in% colnames(df)) {
        validate("No 'adjusted' price data found.")
        return(NULL)
      }
      
      ts_data <- ts(df$adjusted, frequency = 252)
      
      model <- auto.arima(ts_data)
      forecasted <- forecast(model, h = input$horizon)
      
      autoplot(forecasted) +
        labs(
          title = paste(input$forecast_ticker, "Forecast (Next", input$horizon, "Days)"),
          x = "Time", y = "Adjusted Price"
        ) +
        theme_minimal()
    })
    
    # --- Forecast Evaluation ---
    
    output$forecastMetrics <- renderPrint({
      df <- forecast_data()
      req(nrow(df) > input$horizon + 10)
      
      horizon <- input$horizon
      
      df <- df %>% arrange(date)
      train <- head(df$adjusted, -horizon)
      test <- tail(df$adjusted, horizon)

      ts_train <- ts(train, frequency = 252)
      model <- auto.arima(ts_train)
      fc <- forecast(model, h = horizon)

      rmse <- sqrt(mean((fc$mean - test)^2, na.rm = TRUE))
      mae <- mean(abs(fc$mean - test), na.rm = TRUE)

      cat("Model Evaluation\n")
      cat("----------------\n")
      cat("  Forecast Horizon:", horizon, "days\n")
      cat("  RMSE:", round(rmse, 3), "\n")
      cat("  MAE :", round(mae, 3), "\n")
    })
    
    # --- Forecast Accuracy ---
    
    output$forecastAccuracyPlot <- renderPlot({
      df <- forecast_data()
      req(nrow(df) > input$horizon + 10)
      
      df <- df %>% arrange(date)
      horizon <- input$horizon
      train <- head(df$adjusted, -horizon)
      test <- tail(df$adjusted, horizon)
      test_dates <- tail(df$date, horizon)
      
      ts_train <- ts(train, frequency = 252)
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
      
      ggplot(forecast_df, aes(x = date)) +
        geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "lightblue", alpha = 0.3) +
        geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "lightblue", alpha = 0.5) +
        geom_line(aes(y = actual), color = "red", size = 1.1) +
        geom_line(aes(y = forecast), color = "blue", linetype = "dashed", size = 1) +
        labs(
          title = paste(input$forecast_ticker, "Forecast vs. Actual (Last", horizon, "Days)"),
          x = "Date", y = "Adjusted Price",
          caption = "Red = Actual | Blue Dashed = Forecast | Shaded = Confidence Interval"
        ) +
        theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
