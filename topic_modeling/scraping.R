library(rvest)
library(tm)
library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(textcat)

`%notin%` <- Negate(`%in%`)

data = read.csv(paste(path, 'medium_datasci.csv', sep = ''))

scrape = function(url) {
  url = toString(url)
  webpage = read_html(url)
  webpage_text = html_nodes(webpage, '.ac') %>% html_text()
  text_lengths = nchar(webpage_text)
  text = webpage_text[text_lengths == max(text_lengths)]
  text = gsub("’s", " is", text)
  my_stop = c(
    'is', 'this', 'and', 'are', 'also', 'data'
  )
  for (stop in my_stop) {
    text = gsub(paste(' ', stop, ' ', sep = ''), ' ', text)
  }
  return(text)
}

# https://stackoverflow.com/questions/25905144/
#removing-overly-common-words-occur-in-more-than-
#80-of-the-documents-in-r
removeCommonTerms <- function (x, pct) {
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
            is.numeric(pct), pct > 0, pct < 1)
  m <- if (inherits(x, "DocumentTermMatrix")) 
    t(x)
  else x
  t <- table(m$i) < m$ncol * (pct)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix")) 
    x[, termIndex]
  else x[termIndex, ]
}

# shuffle dataset
set.seed(123)
rows <- sample(nrow(data))
data <- data[rows, ]

corpus <- list()
save_urls <- list()

# 100 takes 2 minutes
# 1,000 in 23 minutes
# 10,000 in 226 minutes = 3.76 hours
# 78,388 in total, expect 784*2 = 1568 minutes = 26 hours, uh oh
  # - 8 done Monday at 6am
  # - Should be done on Monday around midnight 
# Train DTM on a random sample? 
#---- GET CORPUS ---
start = as.numeric(Sys.time())
if ("with_progress.RData" %in% list.files()){
  load("with_progress.RData")
} else {
  savei = 0
  urls = data$url
  count = 1
}
for (i in savei:length(urls)){
  try(
    if(toString(urls[i]) != "NA"){
      text = scrape(urls[i])
      if (textcat(text) == "english"){
        corpus[count] = text
        save_urls[count] = toString(data$url[i])
        if (count%%10 == 0){
          now = as.numeric(Sys.time())
          x = as.character(round(now - start, digits = 2)/60)
          print(paste(count, '(', i, '/', length(urls),')', ":", x, "min"))
        }
        count = count + 1
      }
      if (count%%1000 == 0) {
        save.image("with_progress.RData")
      }
    }
  )
}
savei = i

process_corpus <- function(corpus) {
  real_corpus = Corpus(VectorSource(as.vector(corpus)))
  real_corpus = tm_map(real_corpus, removeWords, stopwords('english'))
  real_corpus = tm_map(real_corpus, content_transformer(removePunctuation))
  real_corpus = tm_map(real_corpus, content_transformer(removeNumbers))
  real_corpus = tm_map(real_corpus,  content_transformer(tolower))
  real_corpus = tm_map(real_corpus, content_transformer(stripWhitespace))
  real_corpus = tm_map(real_corpus, content_transformer(stemDocument), language = "english")
  dtm = DocumentTermMatrix(real_corpus, control = list(wordLengths = c(2, Inf)))
  dtm = removeCommonTerms(dtm, 0.5)
  return(dtm)
}

dtm = process_corpus(corpus)

resp = list(
  'urls' = save_urls, 
  'corpus' = corpus, 
  'dtm' = dtm
)

#------------


corpus = resp['corpus'][[1]]
dtm = resp['dtm'][[1]]
urls = resp['urls'][[1]]

save.image("with_corpus.RData")

# --- Test time to fit LDA on different numbers of documents
# n = 1000, t = 1.141 min
# n = 2000, t = 3.165 min
# n = 3000, t = 5.327 min

n = 3000
dtm_n = process_corpus(corpus[1:n])
start = as.numeric(Sys.time())
lda = LDA(dtm_n, k = 6, control = list(seed = 123))
end = as.numeric(Sys.time())
print(paste('n =', toString(n)))
print(round(end - start, digits = 2)/60)
# -------------------------------------------

# save.image("~/github/jennalandy/shiny-apps/medium_datasci/with_lda.RData")

topics = tidy(lda, matrix = 'beta')

dont_show = c(
  "go",
  "’s",
  "don't",
  "thing",
  "want",
  "take",
  "’re"
)

top_terms <- topics %>%
  filter(term %notin% dont_show) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

gamma_matrix = tidy(lda, matrix = 'gamma')

get_topics = function(doc) {
  doc_matrix = gamma_matrix %>% filter(document == doc)
  doc_topics = doc_matrix[doc_matrix$gamma > 0.8,]
  if (dim(doc_topics)[1] == 0){
    doc_topics = doc_matrix[
      doc_matrix$gamma == max(doc_matrix$gamma),
      ]
    if (dim(doc_topics)[1] == 0){
      return(-1)
    }
  }
  return(doc_topics$topic)
}

doc_topics = list()
for (i in 1:length(corpus)){
  doc_topics[i] = get_topics(i)
}

df = data.frame('url' = unlist(urls,use.names=FALSE), 
                'text' = unlist(corpus,use.names=FALSE),
                'topic' = unlist(doc_topics,use.names=FALSE))

save.image("with_df.RData")

print(head(df[c('url','topic')]))

write.csv(df[c('url','topic')], 'article_topics.csv')

# url = 'https://medium.com/@otherside914/futures-of-ai-friendly-ai-a65a1f7c7a31'
# url = 'https://medium.com/@sanparithmarukatat/a-i-%E0%B8%AA%E0%B8%A3%E0%B9%89%E0%B8%B2%E0%B8%87%E0%B8%A0%E0%B8%B2%E0%B8%A9%E0%B8%B2%E0%B9%80%E0%B8%AD%E0%B8%87-e85ebe71891a'
# url = 'https://millennials-times.com/creativeflower-eng-3ed56c6bc8b7'
# url = 'https://medium.com/neworder/os-melhores-links-de-julho-cameras-vintage-google-glass-star-wars-hote-4574f25c9d3f'
# url = 'https://towardsdatascience.com/the-future-of-ai-redefining-how-we-imagine-596d5747da2e'
# url = 'https://medium.com/@annanaveed/digital-hr-for-healthcare-83100c0767f4'
