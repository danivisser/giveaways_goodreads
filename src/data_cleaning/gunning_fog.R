library(quanteda)
library(stringr)
library(nsyllable)

calculate_fog_index <- function(text) {
  
  # Tokenize sentences to count them
  sentences <- str_count(text, "\\.|\\!|\\?")
  if (sentences == 0) sentences <- 1 # Prevent division by zero
  
  # Tokenize words
  words <- quanteda::tokens(text, what = "word")
  word_count <- ntoken(words)
  
  # Count complex words (words with three or more syllables)
  complex_words <- tokens_select(words, pattern = "[a-zA-Z]{3,}", valuetype = "regex", min_nchar = 3)
  complex_word_count <- sum(nsyllable(words[[1]]) >= 3, na.rm = TRUE)
  
  # Apply the Gunning Fog Index formula
  fog_index <- 0.4 * ((word_count / sentences) + (complex_word_count / word_count) * 100)
  
  return(fog_index)
}

# Add gunning-fog to reviews
review_dates_text <- review_dates %>% 
  filter(!is.na(text)) %>% 
  rowwise() %>% 
  mutate(FOG = calculate_fog_index(text)) %>% 
  ungroup()

similar_reviews_dates_text <- similar_review_dates %>% 
  filter(!is.na(text)) %>% 
  rowwise() %>% 
  mutate(FOG = calculate_fog_index(text)) %>% 
  ungroup()

word_count <- function(text) {
  words <- quanteda::tokens(text, what = "word")
  word_count <- ntoken(words)
  
  return(word_count)
}
