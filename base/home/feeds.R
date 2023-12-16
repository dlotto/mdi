require(xml2)
require(httr)
# Thanks to the tidyRSS GitHub (https://github.com/RobertMyles/tidyRSS)
# for some guidance on how to get started here.

safe_xml_find_first <- safely(xml_find_first)
safe_xml_find_all <- safely(xml_find_all)
safe_GET <- safely(GET)

GET_result <- function(x) {
  doc = safe_GET(x)
  res = doc$result
  res
}

GET_content <- function(res) {
  res$content
}

GET_type <- function(res) {
  res$headers$`content-type`
}


type_check <- function(res) {
content_type <- GET_type(res)
  case_when(
    grepl(x = content_type, pattern = "application/xml") ~ "xml_grab",
    grepl(x = content_type, pattern = "text/xml") ~ "xml_grab",
    grepl(x = content_type, pattern = "application/rss\\+xml") ~ "xml_grab",
    grepl(x = content_type, pattern = "application/atom\\+xml") ~ "html_grab",
    TRUE ~ as.character("html_grab")
  )
}


clean_tibble <- function(tb) {
  tb %>%
    lapply(., function(x) gsub("[^\u0009\u000a\u000d\u0020-\uD7FF\uE000-\uFFFD]", "", x)) %>%
    lapply(., function(x) gsub("[[:cntrl:]]", "", x)) %>%
    lapply(., function(x) gsub("\u00E2", "'", x)) %>%
    as_tibble()
}


xml_grab <- function(url, con){
  doc = NULL
  while(is.null(doc)) {
      doc <- try(
        con %>% read_xml()
      )}
  return(doc)
}


html_grab <- function(url){
  doc = NULL
  while(is.null(doc)) {
    doc <- try(
      read_html(url)
    )}
  return(doc)
}

# Applies correct read_x function
feeder <- function(url) {
  res = GET_result(url)
  con = GET_content(res)
  type = type_check(res)
  
  if(type == "xml_grab") {
    doc <- xml_grab(url, con)
    }
  else if(type == "html_grab") {
    doc <- html_grab(url)
  }
  return(doc)
}

# Parses XML-'ready' Feeds
xmlFeed <- function(feed) {
  
  doc = feeder(feed)
  
  items <- xml_find_all(doc, "channel") %>%
    xml_find_all(., "item")
  
  tibble(
    item_title = safe_xml_find_first(items, "title") %>%
      .$result %>%
      xml_text(),
    item_link = safe_xml_find_first(items, "link") %>%
      .$result %>%
      xml_text()
  ) %>%
    clean_tibble()
}

# Parses Atom Feeds
atomFeed <- function(feed) {
  
  doc = feeder(feed)
  header = xml_find_first(doc, "//*[name()='title']") %>% xml_text() %>% unique()
  
  tibble(
    item_title = html_nodes(doc, "title") %>%
      html_text() %>%
      .[!. %in% header],
    item_link = html_nodes(doc, "link") %>%
      map(xml_attrs) %>%
      map_df(~as.list(.)) %>%
      filter(is.na(type)) %>%
      .$href
  ) %>%
    clean_tibble()
}


listScrape <- function(url, li, a) {
  
  doc = feeder(url)
  
  tibble(
    item_title = html_nodes(doc, li) %>%
      html_text() %>%
      trimws(),
    item_link = html_nodes(doc, a) %>%
      html_attr('href') %>%
      trimws()
  ) %>%
    clean_tibble()
}
