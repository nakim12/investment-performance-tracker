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
- Dynamically report forecast error metrics (RMSE, MAE)

---

## Technologies Used

- **R** / **RStudio**
- [`quantmod`](https://cran.r-project.org/web/packages/quantmod/index.html)
- [`forecast`](https://cran.r-project.org/web/packages/forecast/index.html)
- `dplyr`, `ggplot2`, `lubridate`, `zoo`, `tidyr`, `scales`, `tseries`
- **Shiny** (for the web app)
- **RMarkdown** (for the static report)

---

## Live Demo

Try the app: 
[▶️ Investment Performance Tracker](https://nathankim.shinyapps.io/investment-performance-tracker/)

This interactive Shiny app allows users to:
- Select multiple stocks (e.g., AAPL, MSFT, GOOGL)
- Visualize price trends and moving averages
- Compare cumulative returns
- Forecast future prices using ARIMA models
- Monitor investment performance over time

---

## 📂 Repository Structure

Investment-Performance-Tracker/
├── Investment Performance Tracker # RMarkdown Project
├── shinyapp/
│ └── app.r # Shiny app (interactive)
├── README.md # This file

---


## Screenshots

> _(Optional: add plots or UI screenshots here to showcase your app)_

---

## Future Improvements

- Add CSV download for filtered data
- Include additional tickers or ETF categories
- Add user authentication and bookmarking
- Improve layout with `bslib` theming

---

## Author

Nathan Kim  
[GitHub](https://github.com/nakim12) • [LinkedIn](https://linkedin.com/in/kim-a-nathan)

---
