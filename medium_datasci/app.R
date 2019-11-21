# TODO: prettier colors
# TODO: most popular OR most obscure maybe

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
          width = 4
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
      'Topic Popularity',
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
        ),
        class = 'plot_panel'
      )
    ),
    tabPanel(
      "About",
      sidebarLayout(
        sidebarPanel(
          fluidRow(align="center",
             plotOutput("me", width = '100px',height='100px'),
             fluidRow(h6("Jenna Landy")),
             fluidRow(a("Github", href = 'https://github.com/jennalandy/shinyapp-medium-datasci', target='_blank')),
             fluidRow(a("LinkedIn", href = 'https://www.linkedin.com/in/jenna-landy/', target='_blank')),
             HTML('<a href="mailto:jlandy@calpoly.edu" target="blank">jlandy@calpoly.edu</a>'),
            class = 'aboutme'
          )
        ),
        about_app_panel
      )
    )
  ),
  column(12, align = 'center', 
      hr(),
      a('Created by Jenna Landy ● See Source Code', 
        href = 'https://github.com/jennalandy/shinyapp-medium-datasci',
        class = 'footer',
        target = '_blank')
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
      summarize(mean_claps = mean(claps))
    author_recommendations = grouped_by_author[
      order(-grouped_by_author$mean_claps),
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
      "<a href ='", articles$url, "' target='_blank'>", articles$title, "</a>", sep = ''
    )
    articles$claps = format(articles$claps, big.mark=",",scientific=FALSE)
    
    authors = recommendations['auth_urls'][[1]]
    show = articles[c('claps','link_title','author')]
    names(show) = c("Claps", "Article", "Author")
    return(DT::datatable(
      show, rownames= FALSE,
      filter = list('Claps', 'desc'), escape = FALSE
    ))
  })
  
  # Plot
  output$topicPlot <- renderPlot({
    plot_topic_data(
      all_data, as.vector(input$plot_interests), 
      response = input$var, agg = input$agg, range = input$range
    )
  })
  
  # PNGs
  output$png <- renderImage({
    filename = normalizePath(file.path("./", "topics_plot.png"))
    list(src=filename,width = 500,height=500*6.24/7)
  }, deleteFile = FALSE)
  
  output$me <- renderImage({
    filename = normalizePath(file.path("./", "me.png"))
    list(src=filename,width = 100,height=100)
  }, deleteFile = FALSE)
}

# Create Shiny object
shinyApp(ui = ui, server = server)
