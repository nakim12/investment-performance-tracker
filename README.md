# Investment Performance Tracker

An interactive R-based application for analyzing, comparing, and forecasting investment performance using real-time financial data.

---

## Overview

This project started as an RMarkdown-based analysis and evolved into a full-featured [Shiny](https://shiny.posit.co/) web app. Users can analyze real-time stock data, visualize trends, compare returns, evaluate forecasting models, and perform risk analysis by sector.

---

## Features

### RMarkdown Report (Static Analysis)
- Pulled and cleaned financial data using `quantmod` and `dplyr`
- Calculated and visualized moving averages
- Built and evaluated ARIMA models for price forecasting
- Reported forecast accuracy using RMSE and MAE

### Shiny App (Interactive Dashboard)
- **Sector Filtering**: Choose from Technology, Financial, Healthcare, Consumer, and Energy sectors.
- **Multi-Ticker Comparison**: Select and compare multiple tickers simultaneously.
- **Price Trend Visualization**: View adjusted prices with customizable moving averages.
- **Daily & Cumulative Returns**: Visualize short- and long-term return patterns.
- **Return Distribution**: Explore histograms of daily return volatility for each stock.
- **ARIMA Forecasting**:
  - Conservative vs. Aggressive forecast modes
  - Model backtesting with evaluation metrics (RMSE, MAE, MAPE)
  - Forecast accuracy plots comparing predicted vs. actual prices
- **Risk Analysis Tab**:
  - Metrics: Volatility, Sharpe Ratio, Max Drawdown, Value at Risk (VaR)
  - Correlation Heatmap: Analyze how selected tickers move together
  - Risk-Return Scatter: Visualize tradeoffs between volatility and performance

---

## Technologies Used

- **R** / **RStudio**
- `quantmod`, `forecast`, `tseries`, `zoo`, `reshape2`
- `dplyr`, `ggplot2`, `lubridate`, `scales`, `tidyr`
- **Shiny** + `shinycssloaders` for interactivity
- **RMarkdown** for reproducible reporting

---

## Live Demo

Try the app: 
[▶️ Investment Performance Tracker (Shiny App)](https://nathankim.shinyapps.io/investment-performance-tracker/)

---

## Repository Structure

```
Investment-Performance-Tracker/
├── app.R                          # Shiny app (interactive dashboard)
├── Investment Performance Tracker.Rmd   # RMarkdown report (static analysis)
├── Investment Performance Tracker.Rproj # RStudio project file
├── README.md                      # Project description and usage
├── .gitignore                     # Git exclusion rules
```

---

## Author

**Nathan Kim**
[GitHub](https://github.com/nakim12) • [LinkedIn](https://linkedin.com/in/kim-a-nathan)

---
