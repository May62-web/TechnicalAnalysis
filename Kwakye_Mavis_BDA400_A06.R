# BDA400 Assignment 6
# Technical Analysis using R - Visualization Phase

library(shiny)
library(ggplot2)
library(quantmod)

# Stock data
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)
# Shiny User Interface

ui <- fluidPage(
  
  titlePanel("AAPL Technical Analysis Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      
      selectInput(
        "chart_type",
        "Chart Type:",
        choices = c("Line", "Area"),
        selected = "Line"
      ),
      
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c("Moving Average", "RSI", "MACD"),
        selected = "Moving Average"
      )
    ),
    
    mainPanel(
      verbatimTextOutput("signal"),
      plotOutput("stock_chart", height = "500px"),
      plotOutput("rsi_chart", height = "250px"),
      plotOutput("macd_chart", height = "250px")
    )
  )
)
# Shiny Server

server <- function(input, output) {
  
  filtered_data <- reactive({
    stock_data[
      index(stock_data) >= input$date_range[1] &
        index(stock_data) <= input$date_range[2]
    ]
  })
  
  output$signal <- renderText({
    
    close_prices <- as.numeric(Cl(filtered_data()))
    
    ma20 <- SMA(close_prices, n = 20)
    ma50 <- SMA(close_prices, n = 50)
    
    latest_signal <- ifelse(
      tail(ma20, 1) > tail(ma50, 1),
      "Buy",
      ifelse(tail(ma20, 1) < tail(ma50, 1), "Sell", "Hold")
    )
    
    paste("Current Signal:", latest_signal)
  })
  output$stock_chart <- renderPlot({
    
    # Filter stock data by selected date range
   
    
    # Convert stock data to a data frame
    chart_data <- data.frame(
      Date = index(filtered_data()),
      Close = as.numeric(Cl(filtered_data()))
    )
    
    # Calculate Moving Averages BEFORE creating the plot
    chart_data$MA20 <- as.numeric(SMA(chart_data$Close, n = 20))
    chart_data$MA50 <- as.numeric (SMA(chart_data$Close, n = 50)) 
    
    chart_data$RSI <- as.numeric(RSI(chart_data$Close, n = 14))
    
    chart_data$MACD <- as.numeric(MACD(chart_data$Close)[, "macd"])
    
    # Generate Buy, Sell, or Hold signals
    chart_data$Signal <- ifelse(
      chart_data$MA20 > chart_data$MA50,
      "Buy",
      ifelse(chart_data$MA20 < chart_data$MA50, "Sell", "Hold")
    )
    
    
    
    # Create the stock price chart
    p <- ggplot(chart_data, aes(x = Date, y = Close)) +
      geom_line() +
      labs(
        title = paste(stock_symbol, "Stock Price"),
        x = "Date",
        y = "Closing Price (USD)"
      ) +
      theme_minimal()
    
    # Add Moving Averages when selected
    if ("Moving Average" %in% input$technical_indicators) {
      
      p <- p +
        geom_line(aes(y = MA20), linetype = "dashed") +
        geom_line(aes(y = MA50), linetype = "dotted")
    }
    
    
    
    print(p)
  })
  output$rsi_chart <- renderPlot({
    
    if ("RSI" %in% input$technical_indicators) {
      
      rsi_data <- data.frame(
        Date = index(filtered_data()),
        RSI = as.numeric(RSI(as.numeric(Cl(filtered_data())), n = 14))
      )
      
      ggplot(rsi_data, aes(x = Date, y = RSI)) +
        geom_line() +
        geom_hline(yintercept = 70, linetype = "dashed") +
        geom_hline(yintercept = 30, linetype = "dashed") +
        labs(
          title = "RSI (14)",
          x = "Date",
          y = "RSI"
        ) +
        theme_minimal()
      
    }
    
  })
  output$macd_chart <- renderPlot({
    
    if ("MACD" %in% input$technical_indicators) {
      
      
      macd_data <- data.frame(
        Date = index(filtered_data()),
        MACD = as.numeric(
          MACD(as.numeric(Cl(filtered_data())),
               nFast = 12, nSlow = 26, nSig = 9)[, 1]
          
        )
      )
      
      
      ggplot(macd_data, aes(x = Date, y = MACD)) +
        geom_line() +
        labs(
          title = "MACD",
          x = "Date",
          y = "MACD"
        ) +
        theme_minimal()
    }
  })
  }

# Run the Shiny application
shinyApp(ui, server)