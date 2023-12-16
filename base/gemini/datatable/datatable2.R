#UI Output/Namespace Section----
datatable2UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("datatable2_ui"))
}

#Module Section----
datatable2 <- function(input, output, session) {
  
  #UI Section----
  
  output$datatable2_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("datatable2_header"), class = 'shuttle-box-2'))
      ),
      
      div(
        fluidRow(
          box(
            DT::dataTableOutput(session$ns("standard_table"))
          ),
          box(width = 2,
            h4("Cell Clicked Info"),
            tags$i("Click on any cell in the datatable."),
            verbatimTextOutput(session$ns("cell_info"))
          )
        )
      )
    )
    
  })
  
  #Server Section----
  
  output$standard_table <- DT::renderDataTable({
                
    iris %>%
      DT::datatable(.,
                class = 'cell-border stripe',
                selection = 'none',
                rownames=FALSE)
  })
  
  output$cell_info <- renderPrint({
    input$standard_table_cell_clicked
  })
  
  # Project description document
  output$datatable2_header <- renderUI({includeMarkdown(paste0("base/gemini/datatable/datatable2.md"))})
}