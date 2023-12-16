#UI Output/Namespace Section----
yourtabUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("yourtab_ui"))
}

#Module Section----
yourtab <- function(input, output, session) {
  
  #UI Section----

  output$yourtab_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("yourtab_header"), class = 'shuttle-box-2'))
      )
    )
  })
  
  output$yourtab_header <- renderUI({includeMarkdown(paste0("base/apollo/yourtab.md"))})
}