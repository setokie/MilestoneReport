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

# remove data
rm(corpusTwitter)
rm(corpusNews)
rm(corpusBlogs)

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
unigram <- saveRDS(dfm_content, file = "unigram.RDS")
dfm_content <- readRDS("unigram.RDS")

# exploratory data analysis
dfm_content %>%
     textstat_frequency(n=10) %>%
     ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
                geom_point() +
                coord_flip() +
                labs(x = NULL, y = "Frequency") +
                theme_minimal()

twogram <- tokens_ngrams(ContentTokens, n = 2)
dfm_twogram <-dfm(twogram)
rm(twogram)
bigram <- saveRDS(dfm_twogram, file = "bigram.RDS")
dfm_twogram <- readRDS("bigram.RDS")
dfm_twogram %>%
    textstat_frequency(n=10) %>%
    ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
    geom_point() +
    coord_flip() +
    labs(x = NULL, y = "Frequency") +
    theme_minimal()
topfeatures(dfm_twogram)

threegram <- tokens_ngrams(ContentTokens, n = 3)
dfm_threegram <- dfm(threegram)
rm(threegram)
dfm_threegram %>%
    textstat_frequency(n=10) %>%
    ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
    geom_point() +
    coord_flip() +
    labs(x = NULL, y = "Frequency") +
    theme_minimal()
trigram <- saveRDS(dfm_threegram, file = "trigram.RDS")
dfm_trigram <- readRDS("trigram.RDS")

tf_sorted <- sort(colSums(as.matrix(dfm_content)), decreasing = TRUE)
count_sample_tokens <- sum(colSums(dfm_content))
stopifnot(sum(tf_sorted) == count_sample_tokens)

count_singular_features <- length(tf_sorted[tf_sorted == 1])
count_seldom_features <- length(tf_sorted[tf_sorted > 0 & tf_sorted <= 10])

# same as tf of a dfm with all sources grouped into one document
rel_freq <- tf_sorted / count_sample_tokens * 100
coverage <- cumsum(rel_freq)
idx_cover_50 <- Position(function(x) x >= 50, coverage)
idx_cover_80 <- Position(function(x) x >= 80, coverage)
idx_cover_90 <- Position(function(x) x >= 90, coverage)

freq_cover_50 <- tf_sorted[idx_cover_50]
freq_cover_80 <- tf_sorted[idx_cover_80]
freq_cover_90 <- tf_sorted[idx_cover_90]

special_points <- c(idx_cover_50, idx_cover_80, idx_cover_90)
plot(coverage[1:(idx_cover_90 + 100)], type = "p", cex = .4, xaxt = "n",
     xlab = "Number of Features",
     ylab = "Coverage of Corpus (in percent)",
     main = "Coverage of Corpus by Number of Features")
abline(v = special_points,
       lwd = 2, lty = 3, col = "red")
axis(1, special_points,
     labels = special_points)

filenames <- list.files(pattern = "*.txt", full.names = TRUE)
sapply(filenames, function(x) length(readLines(x)))

for (thing in ls()) {
    message(thing)
    print(object.size(get(thing)), units='auto')
}
