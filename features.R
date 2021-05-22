### this script builds features 

library(dplyr)
library(stringr)
library(tokenizers)

fls <- list.files(sprintf("%s/Documents/data/kaggle_reading_levels", getwd()), recursive = TRUE, full.names = TRUE)
training <- read.csv(grep("train", fls, value = TRUE))

set.seed(1979)
split_vector <- sample(c(TRUE, FALSE), nrow(training), replace = TRUE, prob = c(0.7, 0.3))
mytrain <- training[split_vector, ]
mytest <- training[!split_vector, ]

processText <- function(txt){
  ## change hyphenated words to a single word so that the ngram doesn't get erased when tokenizing
  gsub("-", "", txt)
}

## features
createFeatures <- function(xerpt){
  nwords <- count_words(xerpt)
  nsentence <- count_sentences(xerpt)
  semicolon <- grepl(";", xerpt)
  commas <- sum(grepl("\\,", xerpt))
  sent_lengths <- tokenize_sentences(xerpt)[[1]] %>%
    count_words()
  word_lengths <- tokenize_words(xerpt)[[1]] %>%
    nchar()

  data.frame(
    nwords = nwords,
    nsentence = nsentence,
    semicolon = semicolon,
    commas = commas,
    max_sent_length = max(sent_lengths),
    ave_sent_length = mean(sent_lengths),
    max_word_length = max(word_lengths),
    ave_word_length = mean(word_lengths)
  )
}

featurized_train <- lapply(mytrain$excerpt, createFeatures) %>%
  bind_rows() %>%
  mutate(target = mytrain$target)
rownames(featurized_train) <- mytrain$id

featurized_test<- lapply(mytest$excerpt, createFeatures) %>%
  bind_rows() %>%
  mutate(target = mytest$target)
rownames(featurized_test) <- mytest$id
