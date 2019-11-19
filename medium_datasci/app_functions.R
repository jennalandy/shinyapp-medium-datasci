library(dplyr)

# CONSTANTS
tag_options <- c('tag_ai','tag_big_data','tag_data',
         'tag_data_science','tag_data_visualization',
         'tag_deep_learning','tag_machine_learning')

add_css <- "
  table.dataTable tbody tr{
    color: grey;
  }
  table.dataTable tbody tr a{
    color: grey;
  }
  table.dataTable tbody tr:hover {
    color: black;
  }
  table.dataTable tbody tr:hover a {
    color: black;
  }
  table.dataTable_length label {
    color: white;
  }
"

# FUNCTIONS
open_topics <- function() {
  read.csv('article_topics.csv')
}

open_data <- function() {
  data = read.csv('medium_datasci.csv')
  data$date = as.Date(paste(data$month, data$day, data$year, sep = '/'),'%m/%d/%Y')
  data$order = as.numeric(data$month)/12 + as.numeric(data$year)
  return(data)
}

get_tag_data <- function(data) {
  maxorder = max(data$order)
  
  first = T
  for (tag in tag_options){
    this_tag_data = data %>% 
      filter(
        !!rlang::sym(tag) == 1 &
          order < maxorder
      ) %>% 
      group_by(order) %>% 
      summarize(
        med_claps = median(claps),
        num = n()
      ) %>% 
      mutate(tag = tag)
    if (first) {
      tag_data = this_tag_data
      first = F
    } else {
      tag_data = rbind(tag_data, this_tag_data)
    }
  }
  return(tag_data)
}

# UI COMPONENTS
category_selection <- checkboxGroupInput(
  inputId = "category", label = strong("Article Tags:"),
  choices = c(
    'Artificial Intelligence' = 'tag_ai',
    'Big Data' = 'tag_big_data',
    'Data' = 'tag_data',
    'Data Science' = 'tag_data_science',
    'Data Visualization' = 'tag_data_visualization',
    'Deep Learning' = 'tag_deep_learning',
    'Machine Learning' = 'tag_machine_learning'
  ),
  selected = tag_options
)

response_selection <- radioButtons(
  inputId = 'measure', label = strong('Measure:'),
  choices = c(
    'Median Claps' = 'med_claps',
    'Number of Articles' = 'num'
  ),
  selected = 'num'
                                   
)

interest_choices <- c(
  'Video, Animation, and Games' = '1',
  'Work, Jobs, and Industry' = '2',
  'Business' = '3',
  'App and Game Development' = '4',
  'Devices and Technology' = '5',
  'Neural Networks, Deep Learning, and Image Recognition' = '6',
  'Chatbots and Human Computer Interaction' = '7',
  'Teamwork and Management' = '8',
  'Robot Design, User Experience, Healthcare' = '9'
)

topic_selection <- checkboxGroupInput(
  inputId = "interests", label = "",
  choices = interest_choices,
  selected = c('1')
)

display_article <- function(articles, i) {
  article = articles[i,]
  url = a(
    paste('"', toTitleCase(toString(article$title)), '", by ', toTitleCase(toString(article$author)), sep = ''),
    href = article$url, target = "_bank"
  )
  renderUI({
    tagList(i, ". ", url, " (", article$claps, " claps)")
  })
}

display_author <- function(authors, i) {
  author = authors[i,]
  url = a(
    toTitleCase(toString(author$author)),
    href = author$author_url, target = "_bank"
  )
  renderUI({
    tagList(i, ". ", url, " (", author$total_claps, " total claps)")
  })
}

sort_articles_by <- selectInput(
  "sort_articles_by", label = p("Sort By", style = "color: 'red"),
  choices = list(
    "Most Popular" = "claps",
    "Most Obscure" = "neg_claps"
  ),
  selected = "claps",
  width = '20%'
)

articles <- fluidRow(
  h4("Recommended Articles:"),
  # sort_articles_by,
  uiOutput("article1"),
  uiOutput("article2"),
  uiOutput("article3"),
  uiOutput("article4"),
  uiOutput("article5")
)

authors <- fluidRow(
  h4("Recommended Authors:"),
  uiOutput("author1"),
  uiOutput("author2"),
  uiOutput("author3"),
  uiOutput("author4"),
  uiOutput("author5")
)