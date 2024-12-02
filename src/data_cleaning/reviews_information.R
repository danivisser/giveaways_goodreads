library(lubridate)
library(tidyverse)
library(data.table)

review_dates <- fread("../../../Data/Giveaways/preparation/review_dates.csv")
similar_review_dates <- fread("../../../Data/Giveaways/preparation/review_dates_similar.csv")
reviews <- fread("../../../Data/Giveaways/cleaned/reviews_thesis.csv")
similar_reviews <- fread("../../../Data/Giveaways/similar_rev_thesis")

# Keep necessary rows
similar_reviews <- similar_reviews %>% 
  mutate(treatment = 0) %>% 
  rename(book_id = similar_book_id) %>% 
  select(book_id, new_review_id, ratings, time, text, treatment)

reviews$treatment <- 1

similar_review_dates <- similar_review_dates %>% 
  rename(book_id = similar_book_id)
# Join review data with review information
similar_review_dates <- inner_join(similar_review_dates, similar_reviews, by = "book_id")
review_dates <- inner_join(review_dates, reviews, by = "book_id")

# Set review date correctly
similar_review_dates <- similar_review_dates %>% 
  mutate(time = mdy(time))

review_dates <- review_dates %>% 
  mutate(time = mdy(time))

# Combine reviews
reviews <- rbind(review_dates, similar_review_dates)

reviews <- reviews %>% 
  filter(!is.na(text) & text != "")

write.csv(reviews, "../../../Data/Giveaways/cleaned/all_reviews.csv")

# Set POST and delete other reviews
similar_review_dates <- similar_review_dates %>% 
  mutate(POST = ifelse(time >= pre_period_start & time <= pre_period_end, 0,
                       ifelse(time >= post_period_start & time <= post_period_end, 1, NA))) %>% 
  filter(!is.na(POST))

review_dates <- review_dates %>% 
  mutate(POST = ifelse(time >= pre_period_start & time <= pre_period_end, 0,
                       ifelse(time >= post_period_start & time <= post_period_end, 1, NA))) %>% 
  filter(!is.na(POST))

similar_reviews_dates_test <- similar_review_dates %>% 
  filter(!is.na(text))

did_complete
