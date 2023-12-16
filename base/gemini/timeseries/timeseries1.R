#UI Output/Namespace Section----
timeseries1UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("timeseries1_ui"))
}

#Module Section----
timeseries1 <- function(input, output, session) {
  
  #UI Section----
  
  output$timeseries1_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("timeseries1_header"), class = 'shuttle-box-2'))
      ),
      
      div(
        fluidRow(
          box(
            title = "GDP Data from Federal Reserve Economic Database",
            dygraphs::dygraphOutput(session$ns("timeseries"))
          )
        )
      )
    )
  })
  
  #Server Section----
  
  output$timeseries <- dygraphs::renderDygraph({
    
    tidyquant::tq_get("GDPC1", get = "economic.data", from = "1960-01-01", to = "2019-01-01") %>%
      tibble::column_to_rownames(var = "date") %>%
      xts::as.xts() %>%
      dygraphs::dygraph() %>%
      dygraphs::dyAxis("y", label = "US GDP") %>%
      dygraphs::dyRangeSelector()
  })
  
  # Project description document
  output$timeseries1_header <- renderUI({includeMarkdown(paste0("base/gemini/timeseries/timeseries1.md"))})  
}