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
  #topicPlot {
    margin-bottom: 30px;
  }
  .plot_panel {
    margin-bottom: 30px;
  }
  .aboutme {
    background-color: 'black';
  }
"

agg_names = list(
  'avg' = 'Average',
  'med' = 'Median'
)

var_names = list(
  'claps' = 'Number of Claps',
  'time' = 'Reading Time'
)

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

open_all_data <- function() {
  read.csv('data_topics.csv')
}

subset_data <- function(data_all, input_topics) {
  condition = rep(0, nrow(data_all))
  
  for (topic in input_topics) {
    condition = condition + data_all[topic]
  }
  
  unique(data_all[condition > 0,c(
    'title','author','year',
    'month','day','reading_time',
    'claps','url','author_url','week',
    "topic_dl_nn", "topic_ml_algo",
    "topic_app_bot", "topic_ai_hci",
    "topic_ind", "topic_vis"
  )])
}

# UI COMPONENTS
var_selection <- radioButtons(
  inputId = 'var', label = strong('Measure:'),
  choices = c(
    'Claps' = 'claps',
    'Reading Time' = 'time',
    'Number of Articles' = 'num'
  ),
  selected = 'claps'
)

aggregation_selection <- radioButtons(
  inputId = 'agg', label = strong('Aggregation:'),
  choices = c(
    'Average' = 'avg',
    'Median' = 'med'
  ),
  selected = 'avg'
)

range_selection <- radioButtons(
  inputId = 'range', label = strong('Time Range:'),
  choices = c(
    'Day' = 'day',
    'Week' = 'week',
    'Month' = 'month'
  ),
  selected = 'month'
)

# Topics
# 1. Deep Learning, Neural Networks                        + tag_deep_learning, tag_big_data
# 2. Machine Learning, Algorithms                          + tag_machine_learning
# 3. App and Software Development, Chat Bots               
# 4. Artificial Intelligence, Human Computer Interaction   + tag_ai, tag_artificial_intelligence
# 5. Industry, Business
# (6). Data Visualization, just tag_data_visualization

interest_choices <- c(
  'Deep Learning, Neural Networks' = 'topic_dl_nn',
  'Machine Learning, Algorithms' = 'topic_ml_algo',
  'App and Software Development, Chat Bots' = 'topic_app_bot',
  'Artificial Intelligence, Human Computer Interaction' = 'topic_ai_hci',
  'Industry, Business' = 'topic_ind',
  'Data Visualization' = 'topic_vis'
)


topic_selection <- checkboxGroupInput(
  inputId = "interests", label = "",
  choices = interest_choices,
  selected = c('topic_dl_nn')
)

plot_topic_selection <- checkboxGroupInput(
  inputId = "plot_interests", label = "",
  choices = interest_choices,
  selected = c('topic_dl_nn')
)

display_article <- function(articles, i) {
  article = articles[i,]
  url = a(
    paste('"', toTitleCase(toString(article$title)), '", by ', toTitleCase(toString(article$author)), sep = ''),
    href = article$url, target = "_bank"
  )
  renderUI({
    tagList(i, ". ", url, " (", format(article$claps, big.mark=",",scientific=FALSE), " claps)")
  })
}

display_author <- function(authors, i) {
  author = authors[i,]
  url = a(
    toTitleCase(toString(author$author)),
    href = author$author_url, target = "_bank"
  )
  renderUI({
    tagList(i, ". ", url, " (", format(round(author$mean_claps), big.mark=",",scientific=FALSE), " claps per article)")
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

# About panel

about_app_panel <- mainPanel(
  hr(),
  h4("Purpose: Article Recommendation and Inspection of Topic Popularity"),
  p(paste(
    "The first panel of this app acts as a data science article recommendation platform,",
    "including a selection panel of topics a user may be interested in.",
    "Depending on what is selected, the five most popular related articles and authors will be displayed,",
    "as well as a table of all related articles that can be searched.",
    "All articles and authors are linked for easy access to the medium articles themselves."
  )),
  p(paste(
    "The second panel is to inspect topic popularities over time. Again, the user can",
    "select topics of interest. Here, they can also select what popularity metric to inspect:",
    "claps reflects popularity among readers, while number of articles represents popularity among writers",
    "(i.e. how many authors want to write on a given topic?).",
    "Time range and aggregation function are also user-defined."
  )),
  h4("Process: Web Scraping and Topic Modeling"),
  p(paste(
    "The tags provided (ai, big data, data science,",
    "data visualization, deep learning, machine learning, and just 'data')",
    "are not very specific and I hoped to get some more interesting categories.",
    "To do this, I performed topic modeling on the full text of each article.",
    "I first used the urls provided in the dataset to scrape the full text of each article",
    "using the rvest package. Topics of interest were then determined with latent dirichlet allocation ",
    "topic modeling using the topicmodels package. LDA was only fit on a random 3,000 articles for the",
    "sake of time. For the full dataset of 78,388 articles, I recorded its topic distribution (gamma values for each topic."
  )),
  p("Code for web scraping, lda modeling, and lda fitting can be found in topic_modeling/scraping.R"),
  p(paste(
    "Below is a plot of the 20 most common words in each topic of the LDA",
    "topic modeling results. I experimented with different numbers of topics,",
    "and five seemed to be a happy medium where groups didn't overlap too much",
    "and there weren't duplicate topics."
  )),
  fluidRow(align="center",
          plotOutput("png", width = '500px',height=paste(500*6.24/7,'px',sep = ''))
  ),
  p(paste(
    "Putting these topics into context and combining them with the tag information,",
    "I decided on the following topics of interest:"
  )),
  tags$ol(
    tags$li("Deep Learning and Neural Networks: topic 1, tag_deep_learning, tag_big_data"),
    tags$li("Machine Learning and Algorithms: topic 2, tag_macine_learning"),
    tags$li("App, Software, and Chat Bot Development: topic 3"),
    tags$li("Artificial Intelligence, Human Computer Interaction: topic 4, tag_ai, tag_artificial_intelligence"),
    tags$li("Industry and Business: topic 5"),
    tags$li("Data Visualization: tag_data_visualization")
  ),
  p(paste(
    "I define an article as fitting one of these topics of interest if (a) it",
    "is tagged appropriately (not applicable for topics 3 and 5) or (b) the gamma",
    "value associated with its topic is over 0.5."
  )),
  p("Code for preprocessing of data into topics can be found in process_data.R."),
  h4("Reflection: Interesting Discoveries"),
  p(paste(
    "Below are a few intersting discoveries I made in the process of creating",
    "and playing around with this application."
  )),
  p(paste(
    "Looking at the number of articles across topics, it is clear that machine learning is an all",
    "time favorite among authors. On the other end of the spectrum, deep learning and neural networks",
    "are rarely written about. The number of articles on app, software, and chat bot development has gone down in popularity over",
    "the past two years while all other topics have increased."
  )),
  p(paste(
    "On the other hand, when we inspect the median number of claps across topics, machine learning",
    "is relatively unpopular (typically 5 claps per article), while neural networks were significntly more popular",
    "(up to 25 claps per article) up until recently, when neural networks lowered down to the level of ML.",
    "Though it changes throughout the dataset, most recent data indicates that data visualization",
    "articles are slighlty more popular than the rest, with around 7 claps per article. Articles about industry",
    "and business are the least popular, between 1 and 2 claps per article."
  )),
  p(paste(
    "Articles about artificial intelligence, human computer interaction, and data",
    "visualization are, on average, about 1-2 minutes longer to read than articles on other topics.",
    "Articles about business and industry are the shortest at around 3.5 minutes to read.",
    "However, all median reading times are very similar, between 3 and 4 minutes."
  ))
)


# PLOT!!!
topic_map <- list(
  'topic_dl_nn' = 'Deep Learning, Neural Networks',
  'topic_ml_algo' = 'Machine Learning, Algorithms',
  'topic_app_bot' = 'App and Software Development, Chat Bots',
  'topic_ai_hci' = 'Artificial Intelligence, Human Computer Interaction',
  'topic_ind' = 'Industry, Business',
  'topic_vis' = 'Data Visualization'
)

topic_color_map <- list(
  'Deep Learning, Neural Networks' = 'red',
  'Machine Learning, Algorithms' = 'orange',
  'App and Software Development, Chat Bots' = 'green',
  'Artificial Intelligence, Human Computer Interaction' = 'blue',
  'Industry, Business' = 'purple',
  'Data Visualization' = 'black'
)

topic_color_key = c('topic_dl_nn','topic_ml_algo','topic_app_bot',
                    'topic_ai_hci','topic_ind','topic_vis')
topic_color_value = c('red','orange','green','blue','purple','black')

plot_topic_data <- function(data_all, input_topics, response, agg = 'avg', range = 'month') {
  sub = subset_data(data_all, input_topics)
  first = TRUE
  for (topic in input_topics) {
    topic_sub = sub[sub[topic] == 1,]
    topic_sub['topic'] = topic
    if (first) {
      topic_subs = topic_sub[c('title','author','year',
                               'month','day','reading_time',
                               'claps','url','author_url','week','topic'
      )]
      first = FALSE
    } else {
      topic_subs = rbind(topic_subs, topic_sub[c(
        'title','author','year',
        'month','day','reading_time',
        'claps','url','author_url','week','topic'
      )])
    }
  }
  
  
  if (range == 'month') {
    range_dat <- topic_subs %>% group_by(year,month,topic) %>% summarize(
      avg_claps = mean(claps), 
      med_claps = median(claps),
      avg_time = mean(reading_time), 
      med_time = median(reading_time),
      num = sum(!is.na(url))
    )
    range_dat <- range_dat[order(range_dat$year, range_dat$month),]
    range_dat$time <- paste(range_dat$year, range_dat$month)
    unique_times = unique(range_dat$time)
    range_dat$order = rep(1:length(unique_times), each = length(input_topics))
    xticks_breaks = c(2, 5, 8, 11)
    xticks_labels = c("September 2017", "December 2017", 
                      "March 2018", 'June 2018'
    )
  } else if (range == 'day') {
    topic_subs$time <- paste(topic_subs$year, topic_subs$month, topic_subs$day)
    range_dat <- topic_subs %>% group_by(time, topic) %>% 
      summarize(
        avg_claps = mean(claps), 
        med_claps = median(claps),
        avg_time = mean(reading_time), 
        med_time = median(reading_time),
        num = sum(!is.na(url))
      ) %>% ungroup() %>% complete(
        time, topic, fill = list(
          avg_claps = 0,
          med_claps = 0,
          avg_time = 0,
          med_time = 0,
          num = 0
        )
      )
    
    range_dat = merge(range_dat, unique(topic_subs[c('time','day','month','year')]), by = 'time')
    
    range_dat <- range_dat[order(range_dat$year, range_dat$month, range_dat$day),]
    unique_times = unique(range_dat$time)
    range_dat$order = rep(1:length(unique_times), each = length(input_topics))
    xticks_breaks = c(32, 123, 213, 305)
    xticks_labels = c("September 2017", "December 2017", 
                      "March 2018", 'June 2018'
    )
  } else if (range == 'week') {
    range_dat <- topic_subs %>% group_by(year,week,topic) %>% summarize(
      avg_claps = mean(claps), 
      med_claps = median(claps),
      avg_time = mean(reading_time), 
      med_time = median(reading_time),
      num = sum(!is.na(url))
    )
    range_dat <- range_dat[order(range_dat$year, range_dat$week),]
    range_dat$time <- paste(range_dat$year, range_dat$week)
    unique_times = unique(range_dat$time)
    range_dat$order = rep(1:length(unique_times), each = length(input_topics))
    xticks_breaks = c(6, 19, 34, 47)
    xticks_labels = c("September 2017", "December 2017", 
      "March 2018", 'June 2018'
    )
  }
  
  if (response == 'num') {
    name = 'num'
    ylab = 'Number of Articles'
    title = paste('Number of Articles by',toTitleCase(range))
  } else {
    agg_name = agg_names[[agg]]
    var_name = var_names[[response]]
    name = paste(agg, '_', response, sep = '')
    ylab = paste(agg_name, var_name)
    title = paste(agg_name, var_name, "by", toTitleCase(range))
  }
  
  range_dat$topic_label = unlist(topic_map[range_dat$topic], use.names=FALSE)
  
  # drop last time interval because it was not complete
  # (i.e. don't have the full last month of articles)
  range_dat = range_dat[range_dat$order != max(range_dat$order),]
  l = unlist(topic_map[input_topics], use.names=FALSE)
  c = unlist(topic_color_map[l], use.names=FALSE)
  ggplot(data = range_dat, aes(x = order, y = !!rlang::sym(name), col = topic_label)) + 
    geom_line() + 
    xlab(paste(toTitleCase(range), "s", sep = '')) +
    ylab(ylab) +
    ggtitle(title) + 
    labs(color = "Topic") +
    scale_color_manual(
      labels = l, 
      values = c
    ) + 
    scale_x_continuous(breaks = xticks_breaks, labels=xticks_labels)
}
