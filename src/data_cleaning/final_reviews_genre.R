library(tidyverse)
library(data.table)

book_genre <- fread("../../../Data/Giveaways/cleaned/book_genre_filtered.csv")
giveaways_thesis <- fread("../../../Data/Giveaways/cleaned/giveaways_thesis.csv")
similar_map <- fread("../../../Data/Giveaways/similar_map_thesis.csv")
similar_meta <- fread("../../../Data/Giveaways/similar_meta_thesis.csv")

giveaway_genre <- inner_join(giveaways_thesis, book_genre, by = "book_id")

book_ids <- giveaway_genre_books$book_id

similar_books <- similar_map %>% 
  filter(focal_book_id %in% book_ids)

similar_books <- similar_books %>% 
  mutate(book_id = focal_book_id)

similar_books_genre <- inner_join(similar_books, book_genre, by = "book_id")

similar_books <- similar_books_genre %>% 
  select(-focal_book_id, -link)

giveaway_genre <- giveaway_genre %>% 
  select(-format, -copy_n, -request_n, -giveaway_end_date)

similar_reviews <- fread("../../../Data/Giveaways/similar_rev_thesis")

similar_review_dates <- inner_join(similar_books, similar_reviews, by = "similar_book_id")

similar_review_dates <- similar_review_dates %>% 
  select(similar_book_id, shelf, new_review_id, ratings, time, text) %>% 
  mutate(treatment = 0,
         giveaway_start_date = NA) %>% 
  rename(book_id = similar_book_id)

giveaway_reviews <- fread("../../../Data/Giveaways/cleaned/reviews_thesis.csv")

giveaway_reviews_dates <- inner_join(giveaway_genre, giveaway_reviews, by = "book_id")

giveaway_reviews_dates$treatment <- 1

reviews_genre <- rbind(similar_review_dates, giveaway_reviews_dates)

# Alle reviews in fictie en non fictie
write.csv(reviews_genre, "../../../Data/Giveaways/cleaned/reviews_genre.csv")

# Even alle tekst reviews pakken eruit
reviews_genre_text <- reviews_genre %>% 
  filter(!is.na(text) & text != "")

all_reviews_fog <- all_reviews_fog %>% 
  select(-1, -2)

# Kijken voor welke reviews al berekeningen zijn gemaakt en welke nog moeten
reviews_text <- left_join(reviews_genre_text, all_reviews_fog, by = "book_id")
