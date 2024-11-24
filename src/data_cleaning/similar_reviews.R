# Load necessary packages
library(tidyverse)
library(data.table)
library(MatchIt)

similar_map <- fread("../../../Data/Giveaways/similar_map_thesis.csv")
similar_meta <- fread("../../../Data/Giveaways/similar_meta_thesis.csv")
similar_reviews <- fread("../../../Data/Giveaways/similar_rev_thesis")
before_after <- fread("../../../Data/Giveaways/cleaned/before-after.csv")
books_df <- fread("../../../Data/Giveaways/book_df_cleanPublisher.csv")
books_genre <- fread("../../../Data/Giveaways/book_genre.csv")

before_after <- before_after %>%
  mutate(
    book_publication_date = as.Date(book_publication_date, format = "%Y-%m-%d"),
    publication_year = year(book_publication_date),
    publication_month = month(book_publication_date),
    publication_day = day(book_publication_date)
  )

before_after$treatment <- 1
similar_meta$treatment <- 0

similar_meta <- similar_meta %>%
  rename(
    book_id = similar_book_id,
    format.y = bookFormat
  )

similar_meta$publication_month <- match(similar_meta$publication_month, tolower(month.name))
similar_meta$num_pages <- as.numeric(gsub(" pages", "", similar_meta$numberOfPages))

similar_genre <- inner_join(similar_meta, book_genre, by = "book_id")

similar_test <- left_join(similar_meta, book_genre, by = "book_id")

common_columns <- c("book_id", "format.y", "shelf", "publication_year", "publication_month", "publication_day", 
                    "num_pages", "treatment")

before_after_filtered <- before_after %>% select(all_of(common_columns))
similar_genre_filtered <- similar_genre %>% select(all_of(common_columns))
similar_test_filtered <- similar_test %>% select(all_of(common_columns))

summary(similar_genre_filtered)
summary(before_after_filtered)

combined_data <- bind_rows(before_after_filtered, similar_genre_filtered)
combined_data <- bind_rows(before_after_filtered, similar_test_filtered)


# PSM Matching
set.seed(123)  

combined_data <- combined_data %>%
  filter(!is.na(publication_year) & !is.na(num_pages) & !is.na(shelf))

matchit_out <- matchit(treatment ~ publication_year +
                         num_pages,
                       data = combined_data,
                       method = "nearest",
                       replace = TRUE)

matchit_out <- matchit(treatment ~ publication_year +
                         num_pages,
                       data = combined_data,
                       method = "nearest",
                       caliper = 0.2)

matchit_out <- matchit(treatment ~ publication_year +
                         num_pages,
                       data = combined_data,
                       method = "full")

matchit_out <- matchit(treatment ~ publication_year +
                         num_pages,
                       data = combined_data,
                       method = "optimal")

summary(matchit_out)
matched_data <- match.data(matchit_out)
