library(tidyverse)
library(data.table)

book_genre <- fread("../../../Data/Giveaways/cleaned/book_genre_filtered.csv")
giveaways_thesis <- fread("../../../Data/Giveaways/cleaned/giveaways_thesis.csv")
book_df <- fread("../../../Data/Giveaways/cleaned/book_df.csv")
similar_map <- fread("../../../Data/Giveaways/similar_map_thesis.csv")
similar_meta <- fread("../../../Data/Giveaways/similar_meta_thesis.csv")

# Giveaways that are mainly fiction or nonfiction
giveaway_genre <- inner_join(giveaways_thesis, book_genre, by = "book_id")

# Supplying supplementan information
giveaway_genre_books <- inner_join(giveaway_genre, book_df, by = "book_id")

book_ids <- giveaway_genre_books$book_id

similar_books <- similar_map %>% 
  filter(focal_book_id %in% book_ids)

# Determine pre and post periods
giveaway_books <- giveaway_genre_books %>%
  mutate(giveaway_start_date = as.Date(giveaway_start_date),
         giveaway_end_date = as.Date(giveaway_end_date)) %>%
  mutate(pre_period_start = giveaway_start_date - months(6),
         pre_period_end = giveaway_start_date - days(1),
         post_period_start = giveaway_end_date + days(1),
         post_period_end = giveaway_end_date + months(6))

# Combine information for similar books
similar_books <- left_join(similar_books, similar_meta, by = "similar_book_id")

# Set dates accordingly. Keep out publication_year as it has less NAs
similar_books <- similar_books %>%
  mutate(
    book_publication_date = as.Date(publication_date, format = "%Y-%m-%d"),
    publication_month = month(book_publication_date),
    publication_day = day(book_publication_date)
  )

# Set book_id to add genre information
similar_books <- similar_books %>% 
  mutate(book_id = focal_book_id)

# Supplement book genre information to similar book information
similar_books_genre <- inner_join(similar_books, book_genre, by = "book_id")

# Set other columns and drop unnecessary ones
similar_books <- similar_books_genre %>% 
  mutate(num_pages = as.numeric(gsub(" pages", "", numberOfPages))) %>% 
  select(-title.x, -link, -title.y, -author, -reviews, -rating, -`average rating`, - `5 star`, - `4 star`, - `3 star`, - `2 star`, - `1 star`, -published, -Original_Title, -ISBN, -Edition_Language, -focal_book_id, -numberOfPages)

# Add review information to corresponding books
POST_data <- giveaway_books %>% 
  select(book_id, pre_period_start, pre_period_end, post_period_start, post_period_end)

similar_books <- left_join(similar_books, POST_data, by = "book_id")

# Export to do calculations on review datasets, to add in the next steps
## For giveaway books
write_csv(POST_data, "../../Data/Giveaways/preparation/review_dates.csv")

similar_periods <- similar_books %>% 
  select(similar_book_id, pre_period_start, pre_period_end, post_period_start, post_period_end)

write_csv(similar_periods, "../../Data/Giveaways/preparation/review_dates_similar.csv")
# Add review calculations for reviews (quantity, quality calculations) for both similar and normal reviews before merging those datasets
similar_books <- similar_books %>% 
  #select(-book_id) %>% 
 # rename(book_id = similar_book_id) %>% 
  rename(format.y = bookFormat)

similar_books <- similar_books %>% 
  select(-book_publication_date)

giveaway_genre_books <- giveaway_genre_books %>% 
  rename(publication_date = book_publication_date) %>% 
  select(book_id, format.y, publication_year, publication_month, publication_day, publication_date, shelf, num_pages)

giveaway_information <- rbind(giveaway_genre_books, similar_books)

# Giveaway and non- giveaway books without review information
write.csv(giveaway_information, "../../../Data/Giveaways/cleaned/giveaway_information.csv")

did_complete <- left_join(reviews_2, giveaway_information, by = "book_id")

write.csv(did_complete, "../../../Data/Giveaways/cleaned/did_complete.csv")

write_csv(filtered_reviews, "../../../Data/Giveaways/preparation/filtered_reviews.csv")
write_csv(giveaway_genre_books_reviews, "../../../Data/Giveaways/cleaned/before-after.csv")

review_summary <- did_complete %>% 
  group_by(treatment, POST) %>% 
  summarize(
    total_reviews = n(),
    avg_reviews = total_reviews / n_distinct(book_id),
    sd_reviews = sd(table(book_id))
  )

review_summary <- did_complete %>% 
  group_by(treatment, POST) %>% 
  summarize(
    unique_books = n_distinct(book_id)
  )
print(review_summary)

review_summary <- did_complete %>% 
  filter(FOG != Inf) %>% 
  group_by(treatment, POST) %>% 
  summarize(
    avg_length = mean(word_count, na.rm = TRUE),
    sd_review = sd(word_count, na.rm = TRUE),
    number_length = sum(!is.na(word_count)),
    Gf_index = mean(FOG, na.rm = TRUE),
    sd_gf = sd(FOG, na.rm = TRUE),
    N_gf = sum(!is.na(FOG))
  )



