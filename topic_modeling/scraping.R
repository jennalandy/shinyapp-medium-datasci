library(rvest)
library(tm)
library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(textcat)
library(tidyr)

`%notin%` <- Negate(`%in%`)

data = read.csv('medium_datasci/medium_datasci.csv')

# -----------------------------
scrape = function(url) {
  url = toString(url)
  webpage = read_html(url)
  webpage_text = html_nodes(webpage, '.ac') %>% html_text()
  text_lengths = nchar(webpage_text)
  text = webpage_text[text_lengths == max(text_lengths)]
  return(text)
}

# Solution for removing common terms found at:
# https://stackoverflow.com/questions/25905144/removing-overly-common-words-occur-in-more-than-80-of-the-documents-in-r
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
if ("with_progress.RData" %in% list.files('topic_modeling')){
  load("topic_modeling/with_progress.RData")
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
        save.image("topic_modeling/with_progress.RData")
      }
    }
  )
}
savei = i
save.image("topic_modeling/with_progress.RData")

my_stopwords = c(
  "go",
  "’s", "'s", # with regular and with fonted apostrophe
  "don’t", "don't",
  "thing",
  "want",
  "take",
  "’re", "'re",
  'is', 'this', 
  'and', 'are', 'also'
)

process_corpus <- function(corpus) {
  # Transform to a corpus
  real_corpus = Corpus(VectorSource(as.vector(corpus)))
  
  # Remove Punctuation
  real_corpus = tm_map(real_corpus, content_transformer(removePunctuation))
  
  # Remove Numbers
  real_corpus = tm_map(real_corpus, content_transformer(removeNumbers))
  
  # Move to Lower Case
  real_corpus = tm_map(real_corpus,  content_transformer(tolower))
  
  # Get rid of Whitespace
  real_corpus = tm_map(real_corpus, content_transformer(stripWhitespace))
  
  # Remove Stopwords
  real_corpus = tm_map(real_corpus, removeWords, c(stopwords("english")))
  
  # Stem Words
  real_corpus = tm_map(real_corpus, content_transformer(stemDocument), language = "english")
  
  # Transform to a document term matrix and remove terms that are in over 50% of articles
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

save.image("topic_modeling/with_corpus.RData")

# --- Test time to fit LDA on different numbers of documents
# n = 1000, t = 1.141 min
# n = 2000, t = 3.165 min
# n = 3000, t = 5.327 min

n = 3000
dtm_n = process_corpus(corpus[1:n])
start = as.numeric(Sys.time())
lda = LDA(dtm_n, k = 5, control = list(seed = 123))
end = as.numeric(Sys.time())
print(paste('n =', toString(n)))
print(round(end - start, digits = 2)/60)
# -------------------------------------------

# save.image("topic_modeling/with_lda.RData")
N = 5000
dtm_N = process_corpus(corpus[1:N])
all_topics = data.frame(posterior(lda, dtm_N)[[2]])
all_topics$document = 1:nrow(all_topics)
all_topics = gather(all_topics, topic, gamma, X1:X5)
all_topics$topic = gsub('X', '', all_topics$topic)

topics = tidy(lda, matrix = 'beta')

top_terms <- topics %>%
  filter(term %notin% dont_show) %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

doc_topics = data.frame(posterior(lda, dtm)[[2]])
doc_topics$document = 1:nrow(doc_topics)
doc_topics$url = unlist(urls[1:nrow(doc_topics)], use.names=FALSE)
doc_topics = gather(doc_topics, topic, gamma, X1:X5)
doc_topics$topic = gsub('X', '', doc_topics$topic)
write.csv(
  doc_topics[c('url','topic','gamma')], 
  'article_topics.csv',
  row.names=FALSE
)