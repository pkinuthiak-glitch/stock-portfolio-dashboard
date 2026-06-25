#  Dynamic Stock Portfolio Dashboard
## Assignment Title
**Technical Analysis using R: Visualization Phase**
## Student
**Paul Kinuthia Kuguru**
## Project Description
This interactive R Shiny dashboard provides comprehensive stock analysis capabilities including:
- **Real-time stock data** fetching from Yahoo Finance
- **Interactive visualization** with multiple chart types
- **Technical indicators** (Moving Averages, RSI, MACD, Bollinger Bands)
- **Trading signal generation** based on Moving Average crossover strategy
- **Signal annotations** with buy/sell indicators
- **Data export** functionality for further analysis
## Repository Links
- **GitHub Repository**: [https://github.com/YOUR_USERNAME/stock-portfolio-dashboard](https://github.com/YOUR_USERNAME/stock-portfolio-dashboard)
## Features
### 1. Data Collection
- Yahoo Finance integration using `quantmod`
- Historical stock data retrieval
- Date range selection
### 2. Visualization
- Multiple chart types: Line, Candlestick, Area
- Time frame selection (Daily, Weekly, Monthly)
- Interactive ggplot2 visualizations

### 3. Technical Indicators
- **Moving Averages** (SMA with customizable periods)
- **RSI** (Relative Strength Index)
- **MACD** (Moving Average Convergence Divergence)
- **Bollinger Bands** (Volatility indicator)
### 4. Trading Strategy
- **Golden Cross**: BUY signal when short MA crosses above long MA
- **Death Cross**: SELL signal when short MA crosses below long MA
- Visual annotations on price chart
- Comprehensive signal table
### 5. Data Management
- Download data as CSV
- Interactive data table with filtering
- Summary statistics
## Technologies Used
- **R** (v4.6.0)
- **Shiny** - Interactive web framework
- **ggplot2** - Visualization
- **quantmod** - Financial data
- **DT** - Interactive tables
- **gridExtra** - Multi-panel plots
## Installation & Setup
### Prerequisites
```r
# Install required packages
install.packages(c(
  "shiny", "ggplot2", "quantmod", "scales", 
  "gridExtra", "DT", "shinythemes"
))
# stock-portfolio-dashboard
Interactive R Shiny dashboard for stock analysis with technical indicators and trading signals
