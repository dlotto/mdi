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
        if ($("#login-user_name").is(":focus") && (event.key == "Enter")) {
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
                                   # div(style = 'padding-top: 5px; font-size: 20px;',
                                   #     htmlOutput(session$ns("zip_location")))
                               ),
                               
                               hr(),
                               
                               div(style = 'padding-top: 0;',
                                   fluidRow(
                                     column(9, style = 'padding-right: 0;',
                                            shinyWidgets::textInputIcon(session$ns("user_name"), label = NULL, placeholder = "Choose a Username", icon = icon("user-astronaut"))),
                                     column(3, shinyBS::bsButton(session$ns("q_name"), label = "", icon = icon("question-circle", class = 'question-mark'), style = "default", size = "extra-small")),
                                     bsTooltip(id = session$ns("q_name"), "Enter your GitHub username to use that avatar.", placement = "right", trigger = "click", options = NULL)
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
    
    'https://images.squarespace-cdn.com/content/v1/63026b41ce69c14fc69dbf89/1672977362237-N0ZXEZJAEH3AXZENLC48/Mae+Davis+%2812%29.png'
    
  })
  
  
  output$github_img <- renderText({
    req(img_data())
    
    paste0('<img src="',img_data(),'"/ class="github-img">')
  })
  
  output$github_signup <- renderText({
    
    paste0('<a href="https://github.com/join?source=header-home" target="_blank">Join GitHub</a>')
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
      credentials$img <- img_data()
    }
  })

  return(reactive(reactiveValuesToList(credentials)))

}