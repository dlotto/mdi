#UI Output/Namespace Section----
geyser1UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("geyser1_ui"))
}

#Module Section----
geyser1 <- function(input, output, session) {
  
  #UI Section----

  output$geyser1_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("geyser1_header"), class = 'shuttle-box-2'))
      ),
      
      div(
        fluidRow(
            
            # Sidebar with a slider input for number of bins 
              box(
                title = "Old Faithful Geyser Data",
                
                sliderInput(session$ns("bins"),
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
              ),
              
              # Show a plot of the generated distribution
              box(
                plotOutput(session$ns("distPlot"))
              )
            ))
      )
         
  })

  #Server Section----
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from the UI Section
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  # Project description document
  output$geyser1_header <- renderUI({includeMarkdown(paste0("base/gemini/plots/geyser1.md"))})
}