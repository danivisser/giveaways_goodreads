# Load necessary packages
library(tidyverse)

## Load in genre data
book_genres <- fread("../../../Data/Giveaways/book_genre.csv")

## Filter data to have one genre per book
book_genre <- book_genres %>% 
  group_by(book_id) %>% 
  filter(count == max(count)) %>% 
  ungroup()

## Export data
write_csv(book_genre, "../../../Data/Giveaways/cleaned/book_genre_filtered.csv")
