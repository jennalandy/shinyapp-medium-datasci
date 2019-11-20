library(dplyr)
library(ggplot2)

source("app_functions.R")

data = read.csv('medium_datasci.csv')
drops = c("x1")
data = data[ , !(names(data) %in% drops)]

data$week = format(
  as.Date(paste(data$month,'/',data$day,'/',data$year, sep = ''), '%m/%d/%Y'),
  format="%U"
)

topics = read.csv('article_topics.csv')
data$url = as.character(data$url)
topics$url = as.character(topics$url)
data_all = left_join(data, topics, by = 'url')
data_all[is.na(data_all$topic),]$topic = -1
data_all[is.na(data_all$gamma),]$gamma = 0
names(data_all)

# Add Topics To Data
# Topics
# 1. Deep Learning, Neural Networks                        + tag_deep_learning, tag_big_data
topic_dl_nn = (
  data_all['tag_deep_learning'] + 
    data_all['tag_big_data'] + 
    (data_all$topic == 1)&(data_all$gamma > 0.5)
)
data_all['topic_dl_nn'] = rep(0, nrow(data_all))
data_all[topic_dl_nn,]$topic_dl_nn = 1

# 2. Machine Learning, Algorithms                          + tag_machine_learning
topic_ml_algo = (
  data_all['tag_machine_learning'] + 
    (data_all$topic == 2)&(data_all$gamma > 0.5)
)
data_all['topic_ml_algo'] = rep(0, nrow(data_all))
data_all[topic_ml_algo,]$topic_ml_algo = 1

# 3. App and Software Development, Chat Bots  
topic_app_bot = (
  (data_all$topic == 3)&(data_all$gamma > 0.5)
)
data_all['topic_app_bot'] = rep(0, nrow(data_all))
data_all[topic_app_bot,]$topic_app_bot = 1

# 4. Artificial Intelligence, Human Computer Interaction   + tag_ai, tag_artificial_intelligence
topic_ai_hci = (
  data_all['tag_ai'] + 
    data_all['tag_artificial_intelligence'] +
    (data_all$topic == 4)&(data_all$gamma > 0.5)
)
data_all['topic_ai_hci'] = rep(0, nrow(data_all))
data_all[topic_ai_hci,]$topic_ai_hci = 1

# 5. Industry, Business
topic_ind = (
  (data_all$topic == 5)&(data_all$gamma > 0.5)
)
data_all['topic_ind'] = rep(0, nrow(data_all))
data_all[topic_ind,]$topic_ind = 1

# (6). Data Visualization, just tag_data_visualization
topic_vis = as.vector(
  data_all['tag_data_visualization'] == 1
)
data_all['topic_vis'] = rep(0, nrow(data_all))
data_all[topic_vis,]$topic_vis = 1

names(data_all)[24:29]

write.csv(
  data_all, 
  'data_topics.csv',
  row.names=FALSE
)
