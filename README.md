# Investment Performance Tracker

An interactive R-based application for analyzing, comparing, and forecasting investment performance using real-time financial data.

---

## Overview

This project started as an RMarkdown-based analysis and evolved into a full-featured [Shiny](https://shiny.posit.co/) web app. Users can explore real-time stock performance, view technical indicators, compare historical returns, and generate price forecasts with error metrics.

---

## Features

### RMarkdown Report (Static Analysis)
- Pulled and cleaned financial data using `quantmod` and `dplyr`
- Calculated and visualized moving averages
- Built and evaluated ARIMA models for price forecasting
- Reported forecast accuracy using RMSE and MAE

### Shiny App (Interactive Dashboard)
- Select and compare multiple tickers (AAPL, MSFT, GOOGL)
- View adjusted prices with user-defined moving averages
- Visualize daily and cumulative returns
- Forecast future prices using ARIMA models
- View model error metrics (RMSE, MAE) and compare forecasts to actuals

---

## Technologies Used

- **R** / **RStudio**
- `quantmod`, `forecast`, `tseries`, `zoo`
- `dplyr`, `ggplot2`, `lubridate`, `scales`, `tidyr`
- **Shiny** for interactivity
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
