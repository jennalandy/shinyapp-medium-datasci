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
  } else if (range == 'day') {
    range_dat <- topic_subs %>% group_by(year,month,day,topic) %>% summarize(
      avg_claps = mean(claps), 
      med_claps = median(claps),
      avg_time = mean(reading_time), 
      med_time = median(reading_time),
      num = sum(!is.na(url))
    )
    range_dat <- range_dat[order(range_dat$year, range_dat$month, range_dat$day),]
  } else if (range == 'week') {
    range_dat <- topic_subs %>% group_by(year,week,topic) %>% summarize(
      avg_claps = mean(claps), 
      med_claps = median(claps),
      avg_time = mean(reading_time), 
      med_time = median(reading_time),
      num = sum(!is.na(url))
    )
    range_dat <- range_dat[order(range_dat$year, range_dat$week),]
  }
  
  range_dat['order'] <- 1:nrow(range_dat)
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
  
  print(range_dat[c('topic')])
  range_dat$topic_label = unlist(topic_map[range_dat$topic], use.names=FALSE)
  print(range_dat$topic)
  print(range_dat$topic_label)
  # drop last time interval because it was not complete
  # (i.e. don't have the full last month of articles)
  range_dat = range_dat[1:(nrow(range_dat)-1), ]
  l = unlist(topic_map[input_topics], use.names=FALSE)
  c = unlist(topic_color_map[l], use.names=FALSE)
  
  ggplot(data = range_dat, aes(x = order, y = !!rlang::sym(name), col = topic_label)) + 
    geom_line() + 
    xlab(paste(toTitleCase(range), "s since August 1, 2017", sep = '')) + 
    ylab(ylab) +
    ggtitle(title) + 
    labs(color = "Topic") +
    scale_color_manual(
      labels = l, 
      values = c
    )
}
