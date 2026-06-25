# ============================================================
#  COMPLETE STOCK DASHBOARD - SAVE AS "portfolio dashboard.R"
# ============================================================

# ── 1. Load Libraries ──────────────────────────────────────
library(shiny)
library(ggplot2)
library(quantmod)
library(scales)
library(gridExtra)
library(DT)
library(shinythemes)

# ── 2. UI ──────────────────────────────────────────────────
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("📈 Dynamic Stock Portfolio Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("⚙️ Settings"),
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      
      dateRangeInput("date_range", "Date Range:",
                     start = "2023-01-01",
                     end = "2023-12-31"),
      
      hr(),
      
      h4("📊 Chart Options"),
      selectInput("chart_type", "Chart Type:",
                  choices = c("Line Chart" = "line",
                              "Candlestick" = "candlestick",
                              "Area Chart" = "area")),
      
      hr(),
      
      h4("📈 Technical Indicators"),
      checkboxGroupInput("indicators", "Select Indicators:",
                         choices = c("Moving Averages" = "ma",
                                     "RSI" = "rsi",
                                     "MACD" = "macd",
                                     "Bollinger Bands" = "bb"),
                         selected = c("ma", "rsi")),
      
      conditionalPanel(
        condition = "input.indicators.indexOf('ma') > -1",
        numericInput("short_ma", "Short MA:", value = 20, min = 5, max = 100),
        numericInput("long_ma", "Long MA:", value = 50, min = 10, max = 200)
      ),
      
      hr(),
      
      h4("🚦 Trading Signals"),
      checkboxInput("show_signals", "Show Trading Signals", value = TRUE),
      
      hr(),
      
      actionButton("update", "🔄 Update Chart", 
                   class = "btn-primary",
                   style = "width: 100%;"),
      
      br(), br(),
      
      downloadButton("download_data", "📥 Download Data", 
                     class = "btn-success",
                     style = "width: 100%;")
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(12,
               h3(textOutput("stock_title")),
               p(textOutput("stock_info"))
        )
      ),
      
      fluidRow(
        column(12,
               plotOutput("stock_chart", height = "500px")
        )
      ),
      
      fluidRow(
        column(12,
               hr(),
               h4("🚦 Trading Signals"),
               tableOutput("signal_table")
        )
      ),
      
      fluidRow(
        column(12,
               hr(),
               h4("📊 Data Preview"),
               DTOutput("data_table")
        )
      )
    )
  )
)

# ── 3. Server ──────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Reactive data ────────────────────────────────────────
  stock_data <- eventReactive(input$update, {
    symbol <- toupper(input$symbol)
    
    withProgress(message = paste('Fetching', symbol, 'data...'), value = 0.5, {
      
      tryCatch({
        data <- getSymbols(symbol, 
                           from = input$date_range[1], 
                           to = input$date_range[2], 
                           auto.assign = FALSE)
        
        df <- data.frame(
          Date = index(data),
          Open = as.numeric(Op(data)),
          High = as.numeric(Hi(data)),
          Low = as.numeric(Lo(data)),
          Close = as.numeric(Cl(data)),
          Volume = as.numeric(Vo(data))
        )
        
        # Add indicators
        df$SMA_short <- SMA(df$Close, n = input$short_ma)
        df$SMA_long <- SMA(df$Close, n = input$long_ma)
        df$RSI <- RSI(df$Close, n = 14)
        
        macd <- MACD(df$Close)
        df$MACD <- as.numeric(macd[, "macd"])
        df$MACD_signal <- as.numeric(macd[, "signal"])
        df$MACD_hist <- df$MACD - df$MACD_signal
        
        bb <- BBands(df$Close, n = 20, sd = 2)
        df$BB_upper <- as.numeric(bb[, "up"])
        df$BB_middle <- as.numeric(bb[, "mavg"])
        df$BB_lower <- as.numeric(bb[, "dn"])
        
        df$Returns <- c(NA, diff(log(df$Close)) * 100)
        
        # Generate trading signals
        df$Signal <- "HOLD"
        for (i in 2:nrow(df)) {
          if (!is.na(df$SMA_short[i-1]) && !is.na(df$SMA_long[i-1]) &&
              !is.na(df$SMA_short[i]) && !is.na(df$SMA_long[i])) {
            
            prev_diff <- df$SMA_short[i-1] - df$SMA_long[i-1]
            curr_diff <- df$SMA_short[i] - df$SMA_long[i]
            
            if (prev_diff < 0 && curr_diff > 0) {
              df$Signal[i] <- "BUY"
            } else if (prev_diff > 0 && curr_diff < 0) {
              df$Signal[i] <- "SELL"
            }
          }
        }
        
        incProgress(1)
        return(df)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
    })
  })
  
  # ── Outputs ──────────────────────────────────────────────
  output$stock_title <- renderText({
    paste(toupper(input$symbol), "Stock Analysis")
  })
  
  output$stock_info <- renderText({
    df <- stock_data()
    req(df)
    paste(nrow(df), "observations from", 
          format(min(df$Date), "%Y-%m-%d"), 
          "to", format(max(df$Date), "%Y-%m-%d"))
  })
  
  # ── Main Chart ──────────────────────────────────────────
  output$stock_chart <- renderPlot({
    df <- stock_data()
    req(df)
    
    # Base plot
    if (input$chart_type == "candlestick") {
      p <- ggplot(df, aes(x = Date)) +
        geom_rect(aes(
          xmin = Date - 0.4,
          xmax = Date + 0.4,
          ymin = pmin(Open, Close),
          ymax = pmax(Open, Close),
          fill = ifelse(Close >= Open, "Up", "Down")
        ), alpha = 0.9) +
        geom_segment(aes(x = Date, xend = Date, y = Low, yend = High),
                     color = "gray40", size = 0.3) +
        scale_fill_manual(values = c("Up" = "#27ae60", "Down" = "#e74c3c"),
                          guide = "none")
      
    } else if (input$chart_type == "area") {
      p <- ggplot(df, aes(x = Date, y = Close)) +
        geom_area(fill = "#3498db", alpha = 0.6) +
        geom_line(color = "#2c3e50", size = 1)
      
    } else {
      p <- ggplot(df, aes(x = Date, y = Close)) +
        geom_line(color = "#2c3e50", size = 1.2)
    }
    
    # Add indicators
    if ("ma" %in% input$indicators) {
      p <- p +
        geom_line(aes(y = SMA_short, color = "SMA Short"), 
                  size = 0.8, linetype = "dashed", na.rm = TRUE) +
        geom_line(aes(y = SMA_long, color = "SMA Long"), 
                  size = 0.8, linetype = "dashed", na.rm = TRUE)
    }
    
    if ("bb" %in% input$indicators) {
      p <- p +
        geom_ribbon(aes(ymin = BB_lower, ymax = BB_upper),
                    fill = "#3498db", alpha = 0.1, na.rm = TRUE) +
        geom_line(aes(y = BB_upper), color = "#3498db", 
                  size = 0.5, linetype = "dotted", na.rm = TRUE) +
        geom_line(aes(y = BB_lower), color = "#3498db", 
                  size = 0.5, linetype = "dotted", na.rm = TRUE)
    }
    
    # Add trading signals
    if (input$show_signals) {
      signals <- df[df$Signal != "HOLD", ]
      if (nrow(signals) > 0) {
        p <- p +
          geom_point(data = signals[signals$Signal == "BUY", ],
                     aes(x = Date, y = Close), 
                     color = "green", size = 4, shape = 24, fill = "green") +
          geom_point(data = signals[signals$Signal == "SELL", ],
                     aes(x = Date, y = Close), 
                     color = "red", size = 4, shape = 25, fill = "red") +
          geom_text(data = signals,
                    aes(x = Date, y = Close * 1.02, label = Signal),
                    size = 3.5, fontface = "bold", vjust = -0.5)
      }
    }
    
    # Labels and theme
    p <- p +
      labs(title = paste(toupper(input$symbol), "Stock Price"),
           x = "Date", y = "Price ($)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "top",
        legend.title = element_blank()
      )
    
    # Add RSI subplot
    if ("rsi" %in% input$indicators) {
      rsi_plot <- ggplot(df, aes(x = Date, y = RSI)) +
        geom_line(color = "#8e44ad", size = 0.8, na.rm = TRUE) +
        geom_hline(yintercept = 70, linetype = "dashed", color = "red", alpha = 0.7) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "green", alpha = 0.7) +
        labs(title = "RSI (14-day)", x = "", y = "RSI") +
        theme_minimal() +
        ylim(0, 100) +
        theme(plot.title = element_text(size = 10, face = "bold"))
      
      grid.arrange(p, rsi_plot, nrow = 2, heights = c(2, 1))
      
    } else {
      print(p)
    }
  })
  
  # ── Signal Table ─────────────────────────────────────────
  output$signal_table <- renderTable({
    df <- stock_data()
    req(df)
    
    signals <- df[df$Signal != "HOLD", 
                  c("Date", "Signal", "Close", "SMA_short", "SMA_long", "RSI")]
    
    if (nrow(signals) == 0) {
      return(data.frame(Message = "No trading signals generated"))
    }
    
    signals$Date <- format(signals$Date, "%Y-%m-%d")
    signals$Close <- round(signals$Close, 2)
    signals$SMA_short <- round(signals$SMA_short, 2)
    signals$SMA_long <- round(signals$SMA_long, 2)
    signals$RSI <- round(signals$RSI, 1)
    
    colnames(signals) <- c("Date", "Signal", "Close ($)", 
                           "Short MA ($)", "Long MA ($)", "RSI")
    
    signals[order(signals$Date, decreasing = TRUE), ]
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ── Data Table ───────────────────────────────────────────
  output$data_table <- renderDT({
    df <- stock_data()
    req(df)
    
    datatable(df[, c("Date", "Open", "High", "Low", "Close", "Volume", 
                     "SMA_short", "SMA_long", "RSI", "Signal")],
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                dom = 'Bfrtip'
              ),
              rownames = FALSE) %>%
      formatRound(columns = c("Open", "High", "Low", "Close", 
                              "SMA_short", "SMA_long"), digits = 2) %>%
      formatRound(columns = "RSI", digits = 1)
  })
  
  # ── Download Handler ─────────────────────────────────────
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(toupper(input$symbol), "_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- stock_data()
      req(df)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# ── 4. Run the App ──────────────────────────────────────────
shinyApp(ui = ui, server = server)
