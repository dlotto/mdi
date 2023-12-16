require(dygraphs)

#UI Output/Namespace Section----
timeseries2UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("timeseries2_ui"))
}

#Module Section----
timeseries2 <- function(input, output, session) {
  
  #UI Section----
  
  output$timeseries2_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("timeseries2_header"), class = 'shuttle-box-2'))
      ),
      
      div(
        fluidRow(
          box(
            dygraphOutput(session$ns("timeseries_graph"))
          ),
          box(width = 2,
              h4("User Inputs"),
              tags$i("Enter a stock symbol to display."),
              textInput(session$ns("symbol"), "Stock Symbol", value = "AMZN", placeholder = "Enter a Stock Symbol"),
              dateInput(session$ns("start_date"), "Start Date", value = "2017-01-01"),
              dateInput(session$ns("end_date"), "End Date", value = Sys.Date()),
              actionButton(session$ns("update"), "Update")
          )
        )
      )
    )
  })
  
  #Server Section----
  
  stockdata <- eventReactive(input$update, {
    
    tidyquant::tq_get(input$symbol, get = "stock.prices", from = input$start_date, to = input$end_date)
    
  }, ignoreNULL = FALSE)
  
  # Creating the candlestick graph using the dygraphs package
  # Great place to learn more about dygraphs https://rstudio.github.io/dygraphs/
  output$timeseries_graph <- renderDygraph({
    req(stockdata()>0)
    
    stockdata() %>%
      select(date,open,high,low,close) %>%
      column_to_rownames(var = "date") %>%
      xts::as.xts() %>%
      dygraph() %>%
      dyAxis("y", label = "Stock Price") %>%
      dyCandlestick() %>% # can add compress = TRUE here if you want quarterly/monthly candles
      dyRangeSelector()
  })

  # Project description document
  output$timeseries2_header <- renderUI({includeMarkdown(paste0("base/gemini/timeseries/timeseries2.md"))})  
}