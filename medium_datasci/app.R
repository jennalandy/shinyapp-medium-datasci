# TODO: about this app/author tab
# TODO: order authors by avg claps/article
# TODO: display numbers with commas (445614 as 445,614)
# TODO: column headers for table

library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tools)
library(rsconnect)

source("app_functions.R")

# data = open_data()
# topics = open_topics()
all_data = open_all_data()

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  tags$head(tags$style(add_css)),
  
  titlePanel("Exploring Medium Data Science Articles"),
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
      sidebarPanel(
        h6("What are you interested in?"),
        actionLink("plot_selectall", "Select All"),
        plot_topic_selection,
        width = 4
      ),
      mainPanel(
        conditionalPanel(
          condition = "
          (typeof input.plot_interests !== 'undefined' && 
          input.plot_interests.length > 0)
          ",
          hr(),
          plotOutput(outputId = "topicPlot", height = "300px"),
          fluidRow(
            column(4, range_selection),
            column(4, var_selection),
            conditionalPanel(
              condition = "input.var != 'num'",
              column(4, aggregation_selection)
            )
          )
        ),
        conditionalPanel(
          condition = "
          (typeof input.plot_interests == 'undefined' ||
          input.plot_interests.length == 0)
          ",
          h4("Select a topic of interest to see article visualization!")
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
    sub = subset_data(all_data, input$interests)
    url_recommendations <- sub[order(-sub$claps),]  

    author_options = sub$author_url
    grouped_by_author <- all_data %>% 
      filter(author_url %in% author_options) %>%
      group_by(.dots = c("author_url","author")) %>% 
      summarize(total_claps = sum(claps))
    author_recommendations = grouped_by_author[
      order(-grouped_by_author$total_claps),
    ]
    
    output$article1 = display_article(url_recommendations, 1)
    output$article2 = display_article(url_recommendations, 2)
    output$article3 = display_article(url_recommendations, 3)
    output$article4 = display_article(url_recommendations, 4)
    output$article5 = display_article(url_recommendations, 5)
    
    output$author1 = display_author(author_recommendations, 1)
    output$author2 = display_author(author_recommendations, 2)
    output$author3 = display_author(author_recommendations, 3)
    output$author4 = display_author(author_recommendations, 4)
    output$author5 = display_author(author_recommendations, 5)
    
    return(list(
      'urls' = url_recommendations,
      'auth_urls' = author_recommendations
    ))
    
  })

  # Table panel
  observe({
    if (input$plot_selectall == 0) return (NULL)
    else if (input$plot_selectall%%2 == 0) {
      updateCheckboxGroupInput(
        session, "plot_interests", 
        "", 
        choices=interest_choices
      )
    } else {
      updateCheckboxGroupInput(
        session, "plot_interests", 
        "", 
        choices=interest_choices,
        selected=interest_choices
      )
    }
  })
  output$recommendation_table <- DT::renderDataTable({
    recommendations = get_recommendations()
    articles = recommendations['urls'][[1]]
    articles$link_title = paste(
      "<a href ='", articles$url, "'>", articles$title, "</a>", sep = ''
    )
    
    authors = recommendations['auth_urls'][[1]]
    return(DT::datatable(
      articles[c('claps','link_title','author')], rownames= FALSE,
      filter = list('claps', 'desc'), escape = FALSE
    ))
  })
  
  # Plot
  output$topicPlot <- renderPlot({
    plot_topic_data(
      all_data, as.vector(input$plot_interests), 
      response = input$var, agg = input$agg, range = input$range
    )
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
