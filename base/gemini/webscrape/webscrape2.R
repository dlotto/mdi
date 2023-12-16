#UI Output/Namespace Section----
webscrape2UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("webscrape2_ui"))
}

#Module Section----
webscrape2 <- function(input, output, session) {
  
  #UI Section----
  
  output$webscrape2_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               htmlOutput(session$ns("webscrape2_header"), class = 'shuttle-box-2'))
      ),
      
      div(
        fluidRow(
          box(width = 10, title = "Latest International News Headlines from CNN World Markets/Asia",
              DT::dataTableOutput(session$ns("webscrape_table"))
          )
        )
      )
    )
  })
  
  #Server Section----
  
  # Scraping the data
  webscrape_data <- reactive({
    
    url <- 'https://money.cnn.com/data/world_markets/asia/' %>% # the website that we are scraping
      read_html()
    
    tibble( # creating these variables in a tibble
      item_title = html_nodes(url, '#section_latestnews li') %>% # scraping the 'list item' (li) text
        html_text() %>%
        trimws(),
      item_link = html_nodes(url, '#section_latestnews a') %>% # scraping the link associated with the li text
        html_attr('href') %>% #href is the attribute inside of the node that contains the hyperlink
        trimws() %>%
        .[-c(1,length(.))]
    )
  })
  
  # Rendering the scraped data into a datatable
  output$webscrape_table <- DT::renderDataTable({
    req(webscrape_data())
    
    webscrape_data() %>%
      DT::datatable(.)
  })
  
  # Project description document
  output$webscrape2_header <- renderUI({includeMarkdown(paste0("base/gemini/webscrape/webscrape2.md"))})
}