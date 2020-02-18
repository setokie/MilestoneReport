library(quanteda)
library(dplyr)
library(ggplot2)
library(readtext)

#load data set
twitter <- readtext("en_US.twitter.txt")
news <- readtext("en_US.news.txt")
blogs <- readtext("en_US.blogs.txt")

# creating corpus
corpusTwitter <- corpus(twitter)
corpusNews <- corpus(news)
corpusBlogs <- corpus(blogs)
ContentCorpus <- corpusTwitter + corpusNews + corpusBlogs
docnames(ContentCorpus) <- c("Twitter", "Blog", "News")
ContentCorpus <- iconv(ContentCorpus, from = 'UTF-8', to = 'ASCII//TRANSLIT')
summary(ContentCorpus)

# constructing tokens
ContentTokens <- tokens(ContentCorpus, remove_numbers = TRUE,
                        remove_punct = TRUE, remove_symbols = TRUE,
                        remove_twitter = TRUE,
                        remove_hyphens = TRUE, remove_url = TRUE)
ContentTokens <- tokens_select(ContentTokens, names(data_int_syllables))

# document feature matrix
dfm_content <- dfm(ContentTokens, remove = stopwords("english"))
dfm_content <- 
dfm_content <- dfm_trim(dfm_content, min_docfreq = 2)
topfeatures(dfm_content)

word_ts <- kwic(ContentCorpus, pattern = c('ts'))
