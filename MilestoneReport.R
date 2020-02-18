library(quanteda)
library(stopwords)
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
summary(ContentCorpus)

# constructing tokens
ContentTokens <- tokens(ContentCorpus, remove_numbers = TRUE,
                        remove_punct = TRUE, remove_symbols = TRUE,
                        remove_twitter = TRUE,
                        remove_hyphens = TRUE, remove_url = TRUE)
ContentTokens <- tokens_select(ContentTokens, names(data_int_syllables), selection = 'keep')
ContentTokens <- tokens_select(ContentTokens, stopwords('en'), selection = 'remove', min_nchar = 2)
ContentTokens <- tokens_wordstem(ContentTokens)
ContentTokens <- tokens_tolower(ContentTokens)
summary(ContentTokens)


# document feature matrix
dfm_content <- dfm(ContentTokens)
dfm_content <- dfm_trim(dfm_content, min_docfreq = 2)
topfeatures(dfm_content)
