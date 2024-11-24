# Load packages
library(tidyverse)
library(lubridate)
library(data.table)

# Load data
reviews_thesis <- fread("../../../Data/Giveaways/cleaned/reviews_thesis.csv")
giveaways_thesis <- fread("../../../Data/Giveaways/giveaways_thesis.csv")
book_df <- fread("../../../Data/Giveaways/book_df_cleanPublisher.csv")


# Set dates correctly
giveaways_thesis <- giveaways_thesis %>% 
  mutate(giveaway_end_date = mdy(giveaway_end_date),
         giveaway_start_date_parsed = mdy(paste(giveaway_start_date, year(giveaway_end_date))),
         giveaway_start_date = if_else(
           giveaway_start_date_parsed > giveaway_end_date,
           giveaway_start_date_parsed %m-% years(1),
           giveaway_start_date_parsed
         )
         ) %>% 
  select(-release_date, -giveaway_start_date_parsed, -giveaway_id, -givaway_url, -book_url, -countries, -listedby_book_n, -listedby_friend_n, -listedby_name, -listedby_url)

     
## Initial statistics giveaways
summary(giveaways_thesis)

giveaways_thesis <- giveaways_thesis %>% 
  group_by(book_title) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  select(-book_title)

write_csv(giveaways_thesis, "../../../Data/Giveaways/cleaned/giveaways_thesis.csv")


# Reviews thesis
## No duplicates
reviews_thesis %>% 
  group_by(new_review_id) %>% 
  filter(n() > 1) %>% 
  ungroup()

reviews_thesis_time <- reviews_thesis %>% 
  select(-order_filter, -review_id, -reviewer_name, -rating_filter) %>% 
  mutate(time = mdy(time))

reviews_thesis_time_v2 <- reviews_thesis_time %>% 
  group_by(time) %>% 
  filter(year(time) > 2006)

reviews_per_month <- reviews_thesis_time_v2 %>%
  mutate(month = floor_date(time, "month")) %>%
  group_by(month) %>%
  summarise(num_reviews = n())

ggplot(reviews_per_month, aes(x = month, y = log(num_reviews))) +
  geom_line() +
  labs(title = "Number of Reviews Per Month",
       x = "Month",
       y = "Log number of Reviews") +
  theme_minimal()

reviews_april_2021 <- reviews_thesis_time_v2 %>%
  filter(year(time) == 2021, month(time) == 4)

# Count the number of reviews per day in April 2021
reviews_per_day_april <- reviews_april_2021 %>%
  group_by(time) %>%
  summarise(num_reviews = n()) %>%
  arrange(desc(num_reviews))

## Initial statistics reviews
summary(reviews_thesis_time_v2)
summary(reviews_per_month)

# Book DF
book_df_duplicate <- book_df %>% 
  group_by(title) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  mutate(book_id = id) %>% 
  select(-id, -title, -country_code, -isbn, -isbn13, -asin, -kindle_asin, -marketplace_id, -image_url, -small_image_url, -description, - edition_information, -url, -link, -public_document, -published, -publisher_raw, -cleanPublisher_noCombineBig5, -cleanPublisher, -amazonPublishing, -asian, -black, -female, -latinx, -black_author, -female_author, -likely_race_2races, -likely_race_american_indian, -likely_race_asian, -likely_race_black, -likely_race_hispanic, -likely_race_white, -likely_gender_female, -likely_gender_male, -`likely_gender_female, male`, -race0_5_american_indian, -race0_5_asian, -race0_5_black, -race0_5_hispanic, -race0_5_white, -race0_6_american_indian, -race0_6_asian, -race0_6_black, -race0_6_hispanic, -race0_6_white, -gender0_6_female, -gender0_6_male, -gender0_9_female, -gender0_9_male)

summary(book_df_duplicate)

write_csv(book_df_duplicate, "../../../Data/Giveaways/cleaned/book_df.csv")

