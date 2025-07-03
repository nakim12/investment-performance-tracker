#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(quantmod)
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)
library(tidyr)

tickers <- c("AAPL", "MSFT", "GOOGL")

# Define UI for application that draws a histogram
ui <- navbarPage("Investment Performance Tracker",
                 tabPanel("Price Trend",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("tickers", "Select Stocks:", choices = tickers,
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
                              verbatimTextOutput("forecastMetrics")
                            )
                          )
                          ),
                 tabPanel("Cumulative Returns",
                          mainPanel(
                            plotOutput("cumulativePlot")
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  stock_data <- reactive({
    req(input$tickers)
    
    data_list <- lapply(input$tickers, function(ticker) {
      tryCatch({
        df <- getSymbols(ticker, src = "yahoo", from = Sys.Date() - 365, auto.assign = FALSE)
        df <- fortify.zoo(df) %>% as_tibble()
        colnames(df)[1] <- "date"
        
        # Rename the price columns safely if they exist
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
    
    output$pricePlot <- renderPlot({
      df <- stock_data() %>%
        arrange(date) %>%
        group_by(ticker) %>%
        mutate(ma = rollmean(adjusted, k = input$ma_window, fill = NA, align = "right")) %>%
        ungroup()
      
      ggplot(df, aes(x = date, y = adjusted, color = ticker)) +
        geom_line(alpha = 0.5) +
        geom_line(aes(y = ma), linetype = "dashed") +
        labs(title = "Adjusted Price with Moving Average",
             x = "Date", y = "Adjusted Price") +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme_minimal()
    })
    
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
      
      # Assign and return the plot
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
    
    output$forecastPlot <- renderPlot({
      df <- forecast_data()
      req(nrow(df) > 2)  # need at least 2 points to fit a time series
      
      # Debug: print structure
      print("Forecast data columns:")
      print(colnames(df))
      print(head(df))
      
      # Ensure adjusted column is numeric
      if (!"adjusted" %in% colnames(df)) {
        validate("No 'adjusted' price data found.")
        return(NULL)
      }
      
      ts_data <- ts(df$adjusted, frequency = 252)  # ~252 trading days/year
      
      model <- auto.arima(ts_data)
      forecasted <- forecast(model, h = input$horizon)
      
      autoplot(forecasted) +
        labs(
          title = paste(input$forecast_ticker, "Forecast (Next", input$horizon, "Days)"),
          x = "Time", y = "Adjusted Price"
        ) +
        theme_minimal()
    })
    
    output$forecastMetrics <- renderPrint({
      df <- forecast_data()
      req(nrow(df) > 30)
      
      # Parameters
      test_size <- 30
      horizon <- input$horizon
      
      # Split into training/test sets
      df <- df %>% arrange(date)
      train <- head(df$adjusted, -test_size)
      test <- tail(df$adjusted, test_size)
      
      # Fit model and forecast
      ts_train <- ts(train, frequency = 252)
      model <- auto.arima(ts_train)
      fc <- forecast(model, h = length(test))
      
      # Compute error metrics
      rmse <- sqrt(mean((fc$mean - test)^2, na.rm = TRUE))
      mae <- mean(abs(fc$mean - test), na.rm = TRUE)
      
      # Print results
      cat("Model Evaluation (on last", test_size, "days):\n")
      cat("  RMSE:", round(rmse, 3), "\n")
      cat("  MAE :", round(mae, 3), "\n")
    })
    
    observe({
      print(paste("Loaded tickers:", paste(input$tickers, collapse = ", ")))
      print(head(stock_data()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
