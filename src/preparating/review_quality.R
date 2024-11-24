# Load packages
library(text)
library(textTinyR)
library(tidyverse)
library(data.table)
library(stringr)

similar_reviews <- fread("../../../Data/Giveaways/similar_rev_thesis")
filtered_reviews <- fread("../../../Data/Giveaways/preparation/filtered_reviews.csv")

filtered_similar_reviews <- similar_reviews %>% 
  filter(focal_book_id %in% book_ids)

similar_reviews_text <- similar_reviews %>% 
  filter(!is.na(text) & text != "")

similar_reviews_text <- similar_reviews %>% 
  select(-order_filter, -rating_filter, -review_id, -reviewer_name, -ratings, -time, -focal_book_id, -title, -link)

write_csv(similar_reviews_text, "../../../Data/Giveaways/similar_reviews_text.csv")
