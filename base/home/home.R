require(tidyquant)

homeUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("home_ui"))
}

home <- function(input, output, session, login_info) {

  output$home_ui <- renderUI({
    
    tagList(
      div(
        fluidRow(
          column(8, div(style = 'padding-left: 15px; padding-right: 15px;',
                        fluidRow(
                          column(9, 
                                 fluidRow(
                                   column(12, style = 'padding-bottom: 5px;',
                                          class = 'shuttle-box-1',
                                          htmlOutput(session$ns("weather"))
                                   )),
                                 div(style = 'padding-top: 15px;',
                                     fluidRow(
                                       column(12, class = 'shuttle-box-1',
                                              fluidRow(
                                                column(5,
                                                       div(class='home-selectize-input', style = 'text-align: center;',
                                                           uiOutput(session$ns("stock_search"))
                                                       ),
                                                       tags$style(HTML('.datepicker {z-index:99999 !important;}')),
                                                       uiOutput(session$ns("stock_dates")),
                                                       fluidRow(
                                                         column(6,
                                                                actionButton(session$ns("random_ticker"), label = "Random Stock", icon = icon("dice"))
                                                                
                                                         ),
                                                         column(6, style = 'margin: 0 auto;',
                                                                selectizeInput(inputId = session$ns("date_preset"), 
                                                                               label = NULL,
                                                                               width = '60%',
                                                                               selected = "365",
                                                                               choices = c("1M" = "30",
                                                                                           "3M" = "90",
                                                                                           "6M" = "120",
                                                                                           "1Y" = "365",
                                                                                           "2Y" = "730",
                                                                                           "5Y" = "1825",
                                                                                           "10Y" = "3650"))
                                                         )
                                                       )
                                                ),
                                                column(7,
                                                       fluidRow(
                                                         column(8,
                                                                fluidRow(class = 'header-underline',
                                                                         htmlOutput(session$ns("company_info_header"), class = 'stock-info-label')
                                                                ),
                                                                fluidRow(style = 'padding-top: 5px;',
                                                                         htmlOutput(session$ns("basic_info"))
                                                                )
                                                         ),
                                                         column(4,
                                                                fluidRow(class = 'header-underline',
                                                                         htmlOutput(session$ns("stock_info_header"), class = 'stock-info-label')
                                                                ),
                                                                fluidRow(style = 'padding-top: 5px;',
                                                                         htmlOutput(session$ns("ticker_percent")),
                                                                         htmlOutput(session$ns("ticker_highlow"))
                                                                )
                                                         )
                                                       )
                                                )
                                              ))))
                          ),
                          column(3, div(style = 'padding-left: 15px;',
                                        fluidRow(class = 'shuttle-box-1',
                                                 div(style = 'font-size: 18px; text-align: center; padding-bottom: 5px;',
                                                     htmlOutput(session$ns("header2")), class = 'header-underline'),
                                                 div(style = 'padding: 0 15px 0 15px;',
                                                     uiOutput(session$ns("gnews2"))
                                                 ))
                          ))
                        ),
                        
                        div(style = 'padding-top: 15px;', #adds 15px padding for the stock ticker row
                            fluidRow(class = 'shuttle-box-1', style = 'padding: 20px 5px 20px 5px;',
                                     column(12,
                                            plotOutput(session$ns("ticker_plot")) %>% shinycssloaders::withSpinner(color = "#FE01B2"))))
          )),
          column(4, div(style = 'padding-right: 15px;', # Only need padding-right bc the box to the left has padding already
                        fluidRow(
                          column(6, div(style = 'padding-right: 7px;',
                                        fluidRow(class = 'shuttle-box-1', style = 'padding-bottom: 20px;',
                                                 div(style = 'font-size: 18px; text-align: center; padding-bottom: 5px;',
                                                     htmlOutput(session$ns("header1")), class = 'header-underline'),
                                                 
                                                 div(class='home-selectize-input',
                                                     selectizeInput(
                                                       inputId = session$ns("control_gnews1"),
                                                       label = NULL,
                                                       selected = "topnews",
                                                       width = "100%",
                                                       choices = c("Top News" = "topnews",
                                                                   "Sports" = "sports",
                                                                   "Technology" = "technology",
                                                                   "Business" = "business",
                                                                   "Science" = "science",
                                                                   "Health" = "health",
                                                                   "Entertainment" = "entertainment")
                                                     )
                                                 ),
                                                 div(class = 'gnews1-box',
                                                     uiOutput(session$ns("gnews1")))))
                          ),
                          column(6,
                                 div(style = 'padding-left: 8px;',
                                     fluidRow(class = 'shuttle-box-1',
                                              div(style = 'font-size: 18px; text-align: center; padding-bottom: 5px;',
                                                  htmlOutput(session$ns("so_header")), class = 'header-underline'),
                                              div(class='home-selectize-input',
                                                  selectizeInput(inputId = session$ns("so_search"), 
                                                                 label = NULL,
                                                                 selected = c("r","shiny","shinydashboard","javascript","css","html","xml",
                                                                              "tidyverse","rvest","tidyquant","quantmod","shinyjs") %>% sample(., 1),
                                                                 width = "100%",
                                                                 choices = c("r","shiny","shinydashboard","javascript","css","html","xml",
                                                                             "tidyverse","rvest","tidyquant","quantmod","shinyjs"))
                                              ),
                                              div(class = 'so-box',
                                                  uiOutput(session$ns("so_questions"))),
                                              div(style = 'padding-top: 5px;',
                                                  uiOutput(session$ns("so_header_type")))
                                     ),
                                     div(style = 'padding-top: 15px;',
                                         fluidRow(class = 'shuttle-box-1',
                                                  div(class = 'header-underline-scroll',
                                                      style = 'font-size: 18px; text-align: center; padding-bottom: 5px;',
                                                      div(htmlOutput(session$ns("arrow_left")), style = 'display: inline-block;'),
                                                      div(htmlOutput(session$ns("dsfeed_header")), style = 'display: inline-block;'),
                                                      div(htmlOutput(session$ns("arrow_right")), style = 'display: inline-block;')
                                                      ),
                                                  div(style = 'padding: 10px 15px 0 15px;',
                                                      uiOutput(session$ns("dsfeeds"))
                                                  )
                                         ))
                                     
                                 ))
                        )
          ))
        )))
  })
  
  # Weather ----
  
  output$weather <- renderUI({
    # dark.base1 <- "https://forecast.io/embed/#"
    dark.base1 <- "https://merrysky.net/"
    # call.dark1 <- paste(dark.base1, "lat=", login_info()$lat, "&lon=", login_info()$long, "&name=", login_info()$loc, sep="")
    tags$iframe(src="https://images.squarespace-cdn.com/content/v1/63026b41ce69c14fc69dbf89/f53b46b7-57ad-4846-a62f-611b85a3ea03/Mae+Davis+%2810%29.jpg", width= "100%", height= 230, frameborder= 0)
  })
  
  # Stack Overflow ----
  
  url_tag <- reactiveVal()
  
  url_sort <- reactiveVal("active")
  
  url_count <- reactiveVal()
  
  observeEvent(input$so_search, {
    url_tag(input[["so_search"]])
  })
  
  observeEvent(input$so_search_type, {
    url_sort(input[["so_search_type"]])
  })
  
  output$so_header_type <- renderUI({
    
    tags$div(class='home-selectize-input-nobox',
             selectizeInput(inputId = session$ns("so_search_type"),
                            label = NULL,
                            selected = "active",
                            choices = c("Newest" = "newest",
                                        "Active" = "active",
                                        "Unanswered" = "unaswered",
                                        "Votes" = "votes",
                                        "Frequent" = "frequent")
             )
    )
  })
  
  output$so_header <- renderText({
    shiny::validate(
      need(url_tag() != "", "Select a Tag")
    )
    
    url <- paste0('https://stackoverflow.com/questions/tagged/?tagnames=',url_tag(),'&sort=',url_sort())
    paste0('<a href=','"',url,'"',' target="_blank">Stack Overflow</a>')
    
  })
  
  observe({
    req(input$so_search>0 & url_tag()>0)

    tb <- atomFeed(paste0('https://stackoverflow.com/feeds/tag?tagnames=',url_tag(),'&sort=',url_sort()))
    
    url_count(length(tb$item_title))
    
    lapply(seq_len(length(tb$item_title)), function(i) {
      output[[paste0("so_questions", i)]] <- renderUI({
        paste0('<a href=','"',tb$item_link[i],'"',' target="_blank">',tb$item_title[i],'</a>') %>%
          HTML()
      })
    })
  })
  
  output$so_questions <- renderUI({
    lapply(as.list(seq_len(url_count())), function(i) {
      fluidRow(
        column(12,
               htmlOutput(session$ns(paste0("so_questions", i))),
               hr()
        )
      )
    })
  })
  
  random_selected <- eventReactive(input$random_ticker, {
    
    ticker_list() %>% sample(., 1)
    
  }, ignoreNULL = FALSE)
  
  output$stock_search <- renderUI({
    
    selectizeInput(
      inputId = session$ns("ticker_search"),
      label = "S&P500 Stock Search",
      selected = random_selected(),
      choices = ticker_list(),
      multiple = FALSE,
      options = list(
        searchField = c("label","value")
      )
    )
  })
  
  output$stock_dates <- renderUI({
    
    dateRangeInput(session$ns("date_range"), label = NULL, 
                   start = Sys.Date() - as.numeric(input$date_preset),
                   end = Sys.Date())
  })
  
  # Google News Feed ----
  refresh <- reactiveTimer(30000)
  
  gfeed1 <- reactiveValues(choice = "?hl=en-US&gl=US&ceid=US:en")
  
  gfeed2 <- reactiveValues(choice = NULL)
  
  observeEvent(input$control_gnews1, {

    if(input$control_gnews1 == "sports"){
      gfeed1$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRFp1ZEdvU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header1 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRFp1ZEdvU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Sports</a>')
    }
    else if(input$control_gnews1 == "technology"){
      gfeed1$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGRqTVhZU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header1 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGRqTVhZU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Technology</a>')
    }
    else if(input$control_gnews1 == "business"){
      gfeed1$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGx6TVdZU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header1 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGx6TVdZU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Business</a>')
    }
    else if(input$control_gnews1 == "entertainment"){
      gfeed1$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNREpxYW5RU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header1 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNREpxYW5RU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Entertainment</a>')
    }
    else if(input$control_gnews1 == "science"){
      gfeed1$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRFp0Y1RjU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header1 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRFp0Y1RjU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Science</a>')
    }
    else if(input$control_gnews1 == "health"){
      gfeed1$choice <- "/topics/CAAqIQgKIhtDQkFTRGdvSUwyMHZNR3QwTlRFU0FtVnVLQUFQAQ?hl=en-US&gl=US&ceid=US%3Aen"
      output$header1 <- renderText('<a href="https://news.google.com/topics/CAAqIQgKIhtDQkFTRGdvSUwyMHZNR3QwTlRFU0FtVnVLQUFQAQ?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Health</a>')
    }
    else{
      gfeed1$choice <- "?hl=en-US&gl=US&ceid=US:en"
      output$header1 <- renderText('<a href="https://news.google.com/?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Top News</a>')
    }


  })
  
  observeEvent(refresh(), {

    news <- c("sports","technology","business","entertainment","science","health") %>% sample(.,1)
    
    if(news == "sports"){
      gfeed2$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRFp1ZEdvU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header2 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRFp1ZEdvU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Sports</a>')
    }
    else if(news == "technology"){
      gfeed2$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGRqTVhZU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header2 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGRqTVhZU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Technology</a>')
    }
    else if(news == "business"){
      gfeed2$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGx6TVdZU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header2 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGx6TVdZU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Business</a>')
    }
    else if(news == "entertainment"){
      gfeed2$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNREpxYW5RU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header2 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNREpxYW5RU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Entertainment</a>')
    }
    else if(news == "science"){
      gfeed2$choice <- "/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRFp0Y1RjU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen"
      output$header2 <- renderText('<a href="https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRFp0Y1RjU0FtVnVHZ0pWVXlnQVAB?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Science</a>')
    }
    else if(news == "health"){
      gfeed2$choice <- "/topics/CAAqIQgKIhtDQkFTRGdvSUwyMHZNR3QwTlRFU0FtVnVLQUFQAQ?hl=en-US&gl=US&ceid=US%3Aen"
      output$header2 <- renderText('<a href="https://news.google.com/topics/CAAqIQgKIhtDQkFTRGdvSUwyMHZNR3QwTlRFU0FtVnVLQUFQAQ?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Health</a>')
    }
    else{
      gfeed2$choice <- "?hl=en-US&gl=US&ceid=US:en"
      output$header2 <- renderText('<a href="https://news.google.com/?hl=en-US&gl=US&ceid=US%3Aen" target="_blank">Top News</a>')
    }
  })
  
  num_articles <- reactiveVal(NULL)
  
  observe({
    refresh() #refreshes the extract
    
    tb <- xmlFeed(paste0("https://news.google.com/rss",gfeed1$choice))

    headline <- gsub(" -.*","",tb$item_title)
    outlet <- gsub(".*- ","",tb$item_title)
    num_articles(length(headline))
    
    lapply(seq_len(num_articles()), function(i) {
      output[[paste0("gnews1", i)]] <- renderUI({
        paste0('<h5>',outlet[i],'</h5>','<a href=','"',tb$item_link[i],'"',' target="_blank">',headline[i],'</a>') %>%
          HTML()
      })
    })
  })
  
  output$gnews1 <- renderUI({
    lapply(as.list(seq_len(num_articles())), function(i) {
      fluidRow(
        column(12,
               htmlOutput(session$ns(paste0("gnews1", i))),
               hr()
        )
      )
    })
  })
  
  observe({

    tb <- xmlFeed(paste0("https://news.google.com/rss",gfeed2$choice))

    headline <- gsub(" -.*","",tb$item_title)
    outlet <- gsub(".*- ","",tb$item_title)
    
    lapply(seq_len(3), function(i) {
      output[[paste0("gnews2", i)]] <- renderUI({
        paste0('<h5>',outlet[i],'</h5>','<a href=','"',tb$item_link[i],'"',' target="_blank">',headline[i],'</a>') %>%
          HTML()
      })
    })
  })
  
  output$gnews2 <- renderUI({
    lapply(as.list(seq_len(3)), function(i) {
      fluidRow(
        column(12,
               htmlOutput(session$ns(paste0("gnews2", i))),
               hr()
        )
      )
    })
  })
  
  # Data Science Feeds ----
  
  dsfeed_list <- reactiveVal(c("reddit","kdnuggets","kaggle","rbloggers"))
  dsfeed <- reactiveVal()
  
  observe({
    dsfeed(dsfeed_list() %>% sample(.,1))
  })

  # Scrolling through the feeds
  output$arrow_left <- renderText({
    ('<a id="home-scroll_left" href="" class="action-button shiny-bound-input"><i class="fas fa-chevron-left"></i></a>')
  })
  
  output$arrow_right <- renderText({
    '<a id="home-scroll_right" href="" class="action-button shiny-bound-input"><i class="fas fa-chevron-right"></i></a>'
  })
  
  observeEvent(input$scroll_left, {
    i = which(dsfeed_list() == dsfeed())
    if(i > 1) {
      i = i - 1
    } else if(i == 1) {
      i = length(dsfeed_list())
    }
    dsfeed(dsfeed_list()[i])
  })
  
  observeEvent(input$scroll_right, {
    i = which(dsfeed_list() == dsfeed())
    if(i < length(dsfeed_list())) {
      i = i + 1
    } else if(i == length(dsfeed_list())) {
      i = 1
    }
    dsfeed(dsfeed_list()[i])
  })

  
  observeEvent(dsfeed(), {
    
    if(dsfeed() == "reddit") {
      output$dsfeed_header <- renderText('<a href="https://www.reddit.com/r/datascience/" target="_blank">r/DataScience</a>')
      tb <- atomFeed('https://www.reddit.com/r/datascience/.rss')
      
    } else if (dsfeed() == "kdnuggets") {
      output$dsfeed_header <- renderText('<a href="https://www.kdnuggets.com/news/index.html" target="_blank">KDnuggets</a>')
      tb <- xmlFeed("https://www.kdnuggets.com/feed")
      
    } else if(dsfeed() == "kaggle") {
      output$dsfeed_header <- renderText('<a href="https://medium.com/kaggle-blog" target="_blank">Kaggle Blog</a>')
      tb <- xmlFeed("https://medium.com/feed/kaggle-blog")
      
    } else if(dsfeed() == "R_MLlist") {
      output$dsfeed_header <- renderText('<a href="https://github.com/josephmisiti/awesome-machine-learning#general-purpose-machine-learning-24" target="_blank">R ML Packages</a>')
      tb <- listScrape("https://github.com/josephmisiti/awesome-machine-learning#general-purpose-machine-learning-24","ul:nth-child(270) li", "ul:nth-child(270) a") %>%
        sample_n(.,4)
      
    } else if (dsfeed() == "rbloggers") {
      output$dsfeed_header <- renderText('<a href="https://www.r-bloggers.com/" target="_blank">R-bloggers</a>')
      tb <- xmlFeed("https://feeds.feedburner.com/RBloggers")
    }

    if(dsfeed() == "R_MLlist") {
      headline <- gsub(".*- ","",tb$item_title)
    } else { 
      headline <- tb$item_title
    }
    
    lapply(seq_len(4), function(i) {
      output[[paste0("dsfeeds", i)]] <- renderUI({
        paste0('<a href=','"',tb$item_link[i],'"',' target="_blank">',headline[i],'</a>') %>%
          HTML()
      })
    })
  }, ignoreNULL = FALSE)
  
  output$dsfeeds <- renderUI({
    lapply(as.list(seq_len(4)), function(i) {
      fluidRow(
        column(12,
               htmlOutput(session$ns(paste0("dsfeeds", i))),
               hr()
        )
      )
    })
  })

  
  # S&P500 Ticker Search and Charts ----
  
ticker_data <- reactive({

  tq_get(input$ticker_search, curl.options = list(ssl_verifypeer = 0), get = "stock.prices", from = input$date_range[1], to = input$date_range[2])
  
})


output$ticker_plot <- renderPlot({
  req(input$ticker_search > 0 & input$date_range[1] > 0 & input$date_range[2] > 0)
  
  ticker_data() %>%
    select("date","open","high","low","close","volume") %>%
    tibble::column_to_rownames(., var = "date") %>%
    xts::as.xts() %>%
    chartSeries(.,
                name = toupper(input$ticker_search),
                type = "candlesticks",
                col.vol = FALSE,
                multi.col = FALSE,
                TA=list(addVo()),
                theme = chartTheme('white',bg.col='#FFFFFF',fg.col="#555555"
                                   ,up.border='#0449CB',up.col='#0449CB'
                                   ,dn.border='#C12626',dn.col='#C12626'
                                   ,area="#FFFFFF")
                
    )
})

# Stock price summary

output$company_info_header <- renderText({
  "Company Information"
})

output$stock_info_header <- renderText({
  "Stock Information"
})

output$ticker_percent <- renderText({
  req(input$ticker_search > 0 & input$date_range[1] > 0 & input$date_range[2] > 0)
  
  percent <- round((ticker_data()$close[nrow(ticker_data())]-ticker_data()$close[1])/ticker_data()$close[1]*100, 2)
  
  if(percent < 0){
    return(paste("<p>","Change:","<span style=\"color:red\">",percent,"%</span>","</p>"))
    
  }else{
    return(paste("<p>","Change:","<span style=\"color:blue\">",percent,"%</span>","</p>"))
  }
})

output$ticker_highlow <- renderText({
  req(input$ticker_search > 0 & input$date_range[1] > 0 & input$date_range[2] > 0)

  high <- round(max(ticker_data()$close, na.rm = TRUE), 2)
  low <- round(min(ticker_data()$close, na.rm = TRUE), 2)

  paste("<p>","High:",high,"</p>","<p>","Low:",low,"</p>")
})

output$basic_info <- renderText({
  req(input$ticker_search > 0 & input$date_range[1] > 0 & input$date_range[2] > 0)

  n <- which(grepl(paste0("\\<",input$ticker_search,"\\>"), ticker_list()))
  
  paste("<p>","Symbol:",company_info()$symbol[n],"</p>",
        "<p>","Industry:",company_info()$gics_sector[n],"</p>",
        "<p>","Subsector:",company_info()$gics_subsector[n],"</p>"
        )
})

company_info <- reactive({
  
  url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
  
  table_data <- read_html(url) %>%
    html_nodes(.,'#constituents td') %>%
    html_text() %>%
    trimws()

  names <- c("symbol","security",
             "gics_sector","gics_subsector","headquarters",
             "dateadded","cik","founded")
  tickers <- tibble()
  for (k in names) tickers[[k]] <- as.character()
  
  nrows <- (length(table_data)/length(names))-1
  for(i in 0:nrows) {
    for (j in 1:length(names)){
      n <- as.integer(i*length(names) + j)
      tickers[i+1,j] <- as.character(table_data[n])
    }
  }
  
  return(tickers)
})


ticker_list <- reactive({
  req(company_info())
  
  tickers_vec <- gsub("[.]","-",company_info()$symbol)
  names(tickers_vec) <- company_info()$security
  
  return(tickers_vec)
})

}