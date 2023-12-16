#UI Output/Namespace Section----
geyser2UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("geyser2_ui"))
}

# Example found at https://rmarkdown.rstudio.com/flexdashboard/shiny.html

#Module Section----
geyser2 <- function(input, output, session) {
  
  #UI Section----
  
  output$geyser2_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("geyser2_header"), class = 'shuttle-box-2'))
      ),
      
      div(
        fluidRow(
          
          # Box with a information and controls for the plot
          box(width = 3,
              title = "Old Faithful Geyser Data",
              
              strong("Description"),
              textOutput(session$ns("description")),
              hr(),
              
              h4("Basic Info"),
              verbatimTextOutput(session$ns("observations")),
              verbatimTextOutput(session$ns("mean")),
              hr(),
              
              h4("Plot Controls"),
              sliderInput(session$ns("bins"),
                          "Number of bins:",
                          min = 1,
                          max = 50,
                          value = 30)
          ),
          
          # Show a plot of the generated distribution
          box(width = 6,
              plotOutput(session$ns("distPlot"))
          )
        ))
    )
    
  })
  
  #Server Section----
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from the UI Section
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, probability = TRUE, breaks = bins, col = 'darkgray', border = 'white',
         xlab = "Waiting Time (minutes)", main = "Old Faithful Geyser Eruption")
  })
  
  output$description <- renderText(
    "Waiting time between eruptions for the Old Faithful geyser in Yellowstone National Park, Wyoming, USA."
  )
  
  output$observations <- renderText(
    paste("Number of Obvervations:", nrow(faithful))
  )
  
  output$mean <- renderText(
    paste("Mean of Waiting Time:", mean(faithful$waiting) %>% round(digits = 3), "minutes")
  )
  
  # Project description document
  output$geyser2_header <- renderUI({includeMarkdown(paste0("base/gemini/plots/geyser2.md"))})
}