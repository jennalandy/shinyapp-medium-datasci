library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tools)
library(rsconnect)

source("app_functions.R")

data = open_data()
tag_data = get_tag_data(data)
topics = open_topics()

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  tags$head(tags$style(add_css)),
  
  titlePanel("☆ Exploring Medium Data Science Articles ☆"),
  tabsetPanel(
    tabPanel(
      'Article Recommendations',
      sidebarLayout(
        sidebarPanel(
          h6("What are you interested in?"),
          actionLink("selectall", "Select All"),
          topic_selection,
          width = 3
        ),
        mainPanel(
          conditionalPanel(
            condition = "
              (typeof input.interests !== 'undefined' && 
              input.interests.length > 0)
            ",
            articles, authors,
            hr(),
            fluidRow(
              includeCSS('main.css'),
              h6("All Related Articles"),
              DT::dataTableOutput("recommendation_table")
            )
          ),
          conditionalPanel(
            condition = "
              (typeof input.interests == 'undefined' ||
              input.interests.length == 0)
            ",
            h4("Select a topic of interest to see article recommendations!")
          )
        )
      )
    ),
    tabPanel(
      'Statistics',
      sidebarLayout(
        sidebarPanel(
          category_selection,
          response_selection
        ),
        
        # Output: Description, lineplot, and reference
        mainPanel(
          plotOutput(outputId = "tagPlot", height = "300px")
        )
      )
    )
  )
)

# Define server function
server <- function(input, output, session) {
  # Topics panel
  observe({
    if (input$selectall == 0) return (NULL)
    else if (input$selectall%%2 == 0) {
      updateCheckboxGroupInput(
        session, "interests", 
        "", 
        choices=interest_choices
      )
    } else {
      updateCheckboxGroupInput(
        session, "interests", 
        "", 
        choices=interest_choices,
        selected=interest_choices
      )
    }
  })
  
  get_recommendations <- reactive({
    print(input$interests)
    print(!is.null(input$interests))
    
    all_data = merge(data, topics, by = 'url')
    
    condition = rep(0, length(all_data))
    
    for (interest in input$interests) {
      condition = condition|(all_data$topic == interest)
    }
    subsetted_data <- all_data[condition,]
      
    url_recommendations <- subsetted_data[
      order(-subsetted_data$claps),
    ]  
    
    grouped_by_author <- subsetted_data %>% 
      group_by(.dots = c("author_url","author")) %>% 
      summarize(total_claps = sum(claps))
    
    auth_recommendations = grouped_by_author[
      order(-grouped_by_author$total_claps),
    ]
    
    output$article1 = display_article(url_recommendations, 1)
    output$article2 = display_article(url_recommendations, 2)
    output$article3 = display_article(url_recommendations, 3)
    output$article4 = display_article(url_recommendations, 4)
    output$article5 = display_article(url_recommendations, 5)
    
    output$author1 = display_author(auth_recommendations, 1)
    output$author2 = display_author(auth_recommendations, 2)
    output$author3 = display_author(auth_recommendations, 3)
    output$author4 = display_author(auth_recommendations, 4)
    output$author5 = display_author(auth_recommendations, 5)
    
    return(list(
      'urls' = url_recommendations,
      'auth_urls' = auth_recommendations
    ))
    
  })

  # Table
  output$recommendation_table <- DT::renderDataTable({
    recommendations = get_recommendations()
    articles = recommendations['urls'][[1]]
    articles$link_title = paste(
      "<a href ='", articles$url, "'>", articles$title, "</a>", sep = ''
    )
    
    print(articles$link_title[1])
    
    authors = recommendations['auth_urls'][[1]]
    return(DT::datatable(
      articles[c('claps','link_title','author')], rownames= FALSE,
      filter = list('claps', 'desc'), escape = FALSE
    ))
  })

  # Stats panel
  subset_data <- reactive({
    condition = rep(0, length(data))
    for (tag in input$category) {
      condition = condition + data[[tag]]
    }
    
    data[condition > 0,]
  })
  
  subset_tag_data <- reactive({
    tag_data %>% filter(tag %in% input$category)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$tagPlot <- renderPlot({
    sub = subset_tag_data()
    ggplot(data = sub, aes(x = order, y = !!rlang::sym(input$measure), col = tag)) + 
      geom_line()
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
