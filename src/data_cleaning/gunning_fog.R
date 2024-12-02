library(quanteda)
library(stringr)
library(nsyllable)
library(data.table)
library(tidyverse)

review_dates <- fread("../../../Data/Giveaways/cleaned/review_dates.csv")
similar_review_dates <- fread("../../../Data/Giveaways/cleaned/similar_review_dates.csv")

calculate_fog_index <- function(text) {
  
  # Tokenize sentences to count them
  sentences <- str_count(text, "\\.|\\!|\\?")
  if (sentences == 0) sentences <- 1 # Prevent division by zero
  
  # Tokenize words
  words <- quanteda::tokens(text, what = "word")
  word_count <- ntoken(words)
  
  
  # Check for empty text
  if (word_count == 0) {
    return(list(fog_index = NA, word_count = 0))
  }
  # Count complex words (words with three or more syllables)
  #complex_words <- tokens_select(words, pattern = "[a-zA-Z]{3,}", valuetype = "regex", min_nchar = 3)
  complex_word_count <- sum(nsyllable(words[[1]]) >= 3, na.rm = TRUE)
  
  # Apply the Gunning Fog Index formula
  fog_index <- 0.4 * ((word_count / sentences) + (complex_word_count / word_count) * 100)
  
  return(list(fog_index = fog_index, word_count = word_count))
}

calculate_fog_index("u0026lt;3 u0026lt;3 u0026lt;3 u0026lt;3")

# Add gunning-fog to reviews
write.csv(review_dates, "../../Data/Giveaways/cleaned/review_dates.csv")
write.csv(similar_review_dates, "../../Data/Giveaways/cleaned/similar_review_dates.csv")

review_dates_text <- review_dates %>% 
  filter(!is.na(text)) %>% 
  rowwise() %>% 
  mutate(
    metrics = list(calculate_fog_index(text)),
                   FOG = metrics$fog_index,
                   word_count = metrics$word_count) %>% 
  select(-metrics) %>% 
  ungroup()

similar_reviews_dates_text <- similar_review_dates %>% 
  filter(!is.na(text) & text != "")

reviews_test <- reviews %>% 
  head()

reviews <- fread("../../../Data/Giveaways/cleaned/all_reviews.csv")

reviews <- reviews %>% 
  rowwise() %>% 
  mutate(
    metrics = list(calculate_fog_index(text)),
    FOG = metrics$fog_index,
    word_count = metrics$word_count) %>% 
  select(-metrics) %>% 
  ungroup()

write.csv(reviews, "../../../Data/Giveaways/cleaned/all_reviews_fog.csv")

write.csv(review_dates_text, "../../../Data/Giveaways/cleaned/review_dates_text.csv")
write.csv(similar_reviews_dates_text, "../../../Data/Giveaways/cleaned/similar_review_dates_text.csv")

reviews_no_inf <- reviews %>% 
  filter(FOG != Inf)
