library(dplyr)
library(highcharter)
# library(tidytext) ## everything must be a tibble because pull is incorporated
library(stringr)
library(tokenizers)

fls <- list.files(sprintf("%s/Documents/data/kaggle_reading_levels", getwd()), recursive = TRUE, full.names = TRUE)

example_sub <- read.csv(grep("sample", fls, value = TRUE))
### id | target ### 7 rows

training <- read.csv(grep("train", fls, value = TRUE))

### let's take a look at some of the excerpts
training$excerpt[sample(1:nrow(training), 1, replace = F)]

### cleaning...
# convert hypens to underscores


enhanced <- training %>%
  mutate(wordct = count_words(excerpt),
    sentct = count_sentences(excerpt))

hist(enhanced$wordct, 100)
hist(enhanced$sentct, 100)

### I want to find the average length of sentences. In order to do so,
### I need to tokenize the sentences and then count the words in each sentence.
### tokenize_sentences returns a list (1 item for each paragraph, I presume)
### So I need to know how many entries include more than one paragraph

table(unlist(lapply(enhanced$excerpt, function(s) length(tokenize_sentences(s))))) ## they all only have one list element, odd but we'll worry about that later if necessary

excerpts <- enhanced$excerpt
names(excerpts) <- enhanced$id

sent_len <- lapply(excerpts, function(x){
  tmp <- tokenize_sentences(x)[[1]]
  data.frame(
    sent_length = mean(count_words(tmp))
  )
}) %>%
bind_rows(.id = "id")

hist(sent_len$sent_length, 100)

### lets look at vocabulary:
## how many unique words are there across the corpus, with and without standard stopwords
total_unique_corp <- data.frame(table(unlist(tokenize_words(excerpts))))
unique_corp_nonstop <- data.frame(table(unlist(tokenize_words(excerpts, stopwords = stopwords::stopwords("en")))))

unique_wd_ct <- lapply(excerpts, function(x){
  length(unique(tokenize_words(x)[[1]]))
})
hist(unlist(unique_wrds))

### presence of a semicolon
table(unlist(lapply(excerpts, function(x) grepl(";", x))))

### average word length
summary(unlist(lapply(excerpts, function(x){
  tmp <- tokenize_words(x)[[1]]
  mean(nchar(tmp))
})))

### max word length
summary(unlist(lapply(excerpts, function(x){
  tmp <- tokenize_words(x)[[1]]
  max(nchar(tmp))
})))

## tfidf-inverse word freq 
