require(shinyBS)
require(shinyWidgets)

loginUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("login_ui"))
}

login <- function(input, output, session) {
  
  credentials <- reactiveValues(access = FALSE,
                                name = NULL)
  
  output$login_ui <- renderUI({
    
    if(credentials$access == TRUE) return(NULL)
    
    fluidPage(
      # Login by pressing enter
      tags$head(
        tags$script(
          HTML(paste0('
      $(document).keyup(function(event) {
        if ($("#login-user_name,#login-user_zip").is(":focus") && (event.key == "Enter")) {
          $("#login-login_button").click();
        }
      });
')))),
      div(style = 'width: 800px; margin: 0 auto; padding-top: 20px;',
          column(4, style = 'width: 400px; margin: 0 auto;',
                 htmlOutput(session$ns("login_welcome"))
          ),
          column(6,
                 div(style = 'width: 300px; margin: 0 auto;', # max-width: 100%; width: 300px; margin: 0 auto; padding: 20px; 
                     wellPanel(style = "padding-top: 0;",
                               
                               div(style = 'text-align: center; padding-top: 15px;',
                                   htmlOutput(session$ns("github_img")),
                                   div(style = 'padding-top: 5px; font-size: 20px;',
                                       htmlOutput(session$ns("zip_location")))
                               ),
                               
                               hr(),
                               
                               div(style = 'padding-top: 0;',
                                   fluidRow(
                                     column(9, style = 'padding-right: 0;',
                                            shinyWidgets::textInputIcon(session$ns("user_name"), label = NULL, placeholder = "Choose a Username", icon = icon("user-astronaut"))),
                                     column(3, shinyBS::bsButton(session$ns("q_name"), label = "", icon = icon("question-circle", class = 'question-mark'), style = "default", size = "extra-small")),
                                     bsTooltip(id = session$ns("q_name"), "Enter your GitHub username to use that avatar.", placement = "right", trigger = "click", options = NULL)
                                   ),
                                   
                                   fluidRow(
                                     column(9, style = 'padding-right: 0;',
                                            shinyWidgets::textInputIcon(session$ns("user_zip"), label = NULL, placeholder = "Zip Code", icon = icon("globe-americas"))),
                                     column(3, shinyBS::bsButton(session$ns("q_zip"), label = "", icon = icon("question-circle", class = 'question-mark'), style = "default", size = "extra-small")),
                                     bsTooltip(id = session$ns("q_zip"), "Only used to show a weather forecast and display your city.", placement = "right", trigger = "click", options = NULL)
                                   )
                               ),
                               
                               div(style = 'text-align: center;',
                                   fluidRow(
                                     column(6,
                                            actionButton(session$ns("login_button"), "Launch", class = 'button-login', width = "100%", style = 'color: white;') # class = 'btn-primary'
                                     ),
                                     column(6, style = 'color:#1c1e21; font-size: 16px; text-align: center; padding-top: 5px;',
                                            htmlOutput(session$ns("github_signup"))
                                     ))
                               ),
                               
                               uiOutput(session$ns("login_error"))
                     )
                 ))
      ))
  })
  
  output$login_welcome <- renderUI({includeMarkdown(paste0("base/login/login_welcome.md"))})
  
  # Prevent warnings and errors from user inputs
  InputCheck <- function(input, check) {
    
    tryCatch({
      if(check == "git") {
        url <- paste0("https://github.com/",input,".png")
      }
      else if(check == "zip") {
        url <- paste0('http://www.geonames.org/postalcode-search.html?q=',input,'&country=US')
      }

      suppressWarnings(con <- url(url, "rb"))
      
      if(length(con)>0) {
        close(con)
        rm(con)
        TRUE
      } else {FALSE}
    }, error = function(e) FALSE)
  }

  img_data <- eventReactive(input$user_name, {
    
    if (!InputCheck(input$user_name, "git")) {
      'github_pic.png'
    } else {
      paste0('https://github.com/',input$user_name,'.png')
    }
  })
  
  
  output$github_img <- renderText({
    req(img_data())
    
    paste0('<img src="',img_data(),'"/ class="github-img">')
  })
  
  output$github_signup <- renderText({
    
    paste0('<a href="https://github.com/join?source=header-home" target="_blank">Join GitHub</a>')
  })
  
  # Gets city name and lat/long for zip
  zip_data <- eventReactive(input$user_zip, {
    
    if (!InputCheck(input$user_zip, "zip")) {
      return(NULL)
    } else {
      input$user_zip %>%
        paste0('http://www.geonames.org/postalcode-search.html?q=',.,'&country=US') %>%
        read_html() %>%
        html_nodes(.,'tr:nth-child(2) td:nth-child(2) , tr:nth-child(3) small') %>%
        html_text() %>%
        trimws()
    }
  })
  
  # If user does not input a zip then a random one is selected
  no_zip_data <- eventReactive(input$user_zip, {
    
    read_html('https://phaster.com/zip_code.html') %>%
        html_nodes(.,'tr+ tr td~ td+ td font') %>%
        html_text() %>%
        gsub("thru.*","",.) %>%
        gsub("-.*","",.) %>%
        trimws() %>%
        .[! . %in% c("02101","87500")] %>% #returns 'no rows' from geonames
        sample(.,1) %>%
        paste0('http://www.geonames.org/postalcode-search.html?q=',.,'&country=US') %>%
        read_html() %>%
        html_nodes(.,'tr:nth-child(2) td:nth-child(2) , tr:nth-child(3) small') %>%
        html_text() %>%
        trimws()
  })
  
  login_zip_data <- eventReactive(input$user_zip, {
    
    if(input$user_zip == ""){
      no_zip_data()
    } else {
      zip_data()
    }
  })
  
  
  observeEvent(input$user_zip, {
    
    output$zip_location <- renderText({
      req(zip_data())
      shiny::validate(
        need(input$user_zip != "", "Enter Zip Code")
      )
      paste0(zip_data()[1])
    })
  })
  

  observeEvent(input$login_button, {
    
    if (trimws(input$user_name) == "") {
      output$login_error <- renderUI({
        fluidRow(
          hr(),
          div(style = 'text-align: center; padding-top: 5px;',
              p("Please Choose a Username", style = 'color: red;')
          )
        )
      })
    } else {
      credentials$access <- TRUE
      credentials$name <- input$user_name
      credentials$loc <- if(input$user_zip == "") {no_zip_data()[1]} else {zip_data()[1]}
      credentials$lat <- if(input$user_zip == "") {gsub("/.*","",no_zip_data()[2])} else {gsub("/.*","",zip_data()[2])}
      credentials$long <- if(input$user_zip == "") {gsub(".*/","",no_zip_data()[2])} else {gsub(".*/","",zip_data()[2])}
      credentials$img <- img_data()
    }
  })

  return(reactive(reactiveValuesToList(credentials)))

}