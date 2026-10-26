# BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase

**Name:** Balwinder
**Course:** BDA400 - Data Science Tools and Techniques
**Assignment title:** Assignment 6 - Technical Analysis using R, Visualization Phase (15%)
**Repository:** https://github.com/YOUR-USERNAME/BDA400_Assignment_6  <!-- replace with your real link -->

## Project description

An interactive portfolio dashboard built with R Shiny. It downloads historical stock
data (Yahoo Finance via `quantmod`, with a Stooq backup), plots it as a candlestick,
line or area chart with `ggplot2`, overlays technical indicators (Moving Averages,
Bollinger Bands, RSI, MACD) that can be toggled on and off, and applies a
customizable trading rule (MA crossover, RSI, or MACD crossover) to annotate the
chart with Buy / Sell / Hold signals. A second tab compares several stocks in a portfolio.

## Files

| File | Purpose |
|------|---------|
| `Balwinder_BDA400_A06.R` | Complete R script (Shiny app), organized by Task 1-4 |
| `README.md` | This cover page |

## How to run

```r
# Packages are installed automatically if missing: shiny, ggplot2, quantmod, patchwork
shiny::runApp("Balwinder_BDA400_A06.R")
```

## Mapping to the assignment tasks

| Task | Where in the script |
|------|---------------------|
| Task 1 - Data collection and setup | Source choice, package install/load, `fetch_stock_data()` with error handling |
| Task 2 - Visualizing stock data | `ui` skeleton, widgets (date range, time frame, chart type), `build_price_plot()` |
| Task 3 - Overlay technical indicators | `add_indicators()`, indicator checkboxes, RSI / MACD panels |
| Task 4 - Trading rules and annotations | `generate_signals()`, Buy / Sell / Hold labels and markers on the chart |
