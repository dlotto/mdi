#UI Output/Namespace Section----
datatable1UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("datatable1_ui"))
}

#Module Section----
datatable1 <- function(input, output, session) {
  
  #UI Section----
  
  output$datatable1_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("datatable1_header"), class = 'shuttle-box-2'))
      ),
      
      div(
        fluidRow(
          box(
            DT::dataTableOutput(session$ns("standard_table"))
          )
        )
      )
    )
    
  })
  
  #Server Section----
  
  output$standard_table <- DT::renderDataTable({
    
    iris %>%
      DT::datatable(.)
  })
  
  # Project description document
  output$datatable1_header <- renderUI({includeMarkdown(paste0("base/gemini/datatable/datatable1.md"))})
}