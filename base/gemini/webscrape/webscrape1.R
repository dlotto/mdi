#UI Output/Namespace Section----
webscrape1UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("webscrape1_ui"))
}

#Module Section----
webscrape1 <- function(input, output, session) {
  
  #UI Section----
  
  output$webscrape1_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("webscrape1_header"), class = 'shuttle-box-2'))
      ),
      
      div(
        fluidRow(
          box(title = "Latest International News Headlines from CNN World Markets/Asia",
              DT::dataTableOutput(session$ns("webscrape_table"))
          )
        )
      )
    )
  })
  
  #Server Section----
  
  # Scraping the data
  webscrape_data <- reactive({
    
    url <- 'https://money.cnn.com/data/world_markets/asia/' # the website that we are scraping
    
    url %>%
      read_html() %>%
      html_nodes(.,'#section_latestnews li') %>% # we figure out which element to target by using the SelectorGadget plugin
      html_text() %>%
      trimws() %>%
      tibble()
  })
  
  # Rendering the scraped data into a datatable
  output$webscrape_table <- DT::renderDataTable({
    req(webscrape_data())
    
    webscrape_data() %>%
      DT::datatable(.)
  })
  
  # Project description document
  output$webscrape1_header <- renderUI({includeMarkdown(paste0("base/gemini/webscrape/webscrape1.md"))})
}