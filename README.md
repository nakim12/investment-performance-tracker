# Investment Performance Tracker

This R project analyzes and forecasts stock price trends using real-time 
financial data. Built with RMarkdown, it includes return analysis, trend 
visualization, and ARIMA-based forecasting.

## Features
- Real-time data pulled from Yahoo Finance (`quantmod`)
- Daily and cumulative return calculations
- 20-day and 50-day moving average trend visualizations
- Short-term price forecasting using ARIMA
- Forecast error evaluation using RMSE and MAE

## Technologies Used
- R, RMarkdown, `ggplot2`, `dplyr`, `quantmod`, `forecast`, `zoo`

## Next Steps
- Convert to a Shiny app
- Add support for ETFs or multi-stock portfolios
- Automate updates via `cronR` or GitHub Actions

## How to Run
Clone the repository, open `Investment Performance Tracker.Rmd` in RStudio, and 
knit to HTML to view the report.
