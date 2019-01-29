#####-----my_text_processing.R-----#####

#Source: https://quanteda.io/

tokenization <- function(text){
  library(quanteda)
  tokens <- tokens(text, what = "word", 
                   remove_numbers = TRUE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_hyphens = TRUE)
  tokens <- tokens_tolower(tokens) # low casing of tokens
  tokens <- tokens_select(tokens, stopwords(), selection = "remove") # remove stop words
  tokens <- tokens_wordstem(tokens, language = "english") # stemming
  return(tokens)
}

# Term frequency (TF)
tf <- function(row) {
  row / sum(row)
}

# Inverse document frequency (IDF)
idf <- function(col) {
  texts_count <- length(which(col > 0))
  log10(length(col) / texts_count)
}

# TF-IDF
tf_idf <- function(x, idf) {
  x * idf
}

# DFM
dfm_matrix <- function(tokens){
  dfm <- dfm(tokens, tolower = FALSE)
  return(as.matrix(dfm))
}

# Normalize DFM with TF-IDF
tfidf <- function(dfm_matrix){
  tokens_df <- apply(dfm_matrix, 1, tf)
  tokens_idf <- apply(dfm_matrix, 2, idf)
  tokens_tfidf <-  apply(tokens_df, 2, tf_idf, idf = tokens_idf)
  tokens_tfidf <- t(tokens_tfidf)
  tokens_tfidf[is.na(tokens_tfidf)] <- 0.0 #NA to 0
  return(tokens_tfidf)
}

# Texts preprocessing
get_tfidf <- function(texts){
  tokens <- tokenization(texts)
  tokens_matrix <- dfm_matrix(tokens)
  tokens_tfidf <- tfidf(tokens_matrix)
  return(tokens_tfidf)
}

#####-----End of my_text_processing.R-----#####