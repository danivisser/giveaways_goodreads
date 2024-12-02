# Load necessary packages
library(tidyverse)
library(data.table)

## Load in genre data
book_genres <- fread("../../../Data/Giveaways/book_genre.csv")

## Filter data to have one genre per book
book_genre <- book_genres %>% 
  group_by(book_id) %>% 
  filter(count == max(count)) %>% 
  select(-count) %>% 
  ungroup()

## Keep only fiction and non fiction
book_genre <- book_genre %>% 
  filter(shelf == "non-fiction" | shelf == "fiction")

book_genre <- book_genre %>% 
  group_by(book_id) %>% 
  filter(n() == 1) %>% 
  ungroup()

## Export data
write_csv(book_genre, "../../Data/Giveaways/cleaned/book_genre_filtered.csv")
