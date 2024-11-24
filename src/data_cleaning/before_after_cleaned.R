library(tidyverse)
library(data.table)

before_after <- fread("../../../Data/Giveaways/cleaned/before-after.csv")

summary(before_after)

before_after <- before_after %>%
  mutate(
    book_publication_date = as.Date(book_publication_date, format = "%Y-%m-%d"),
    publication_year = year(book_publication_date),
    publication_month = month(book_publication_date),
    publication_day = day(book_publication_date)
  )

before_after <- before_after %>%
  mutate(time_to_giveaway = as.numeric(difftime(giveaway_start_date, book_publication_date, units = "days")))

avg_time_to_giveaway <- mean(before_after$time_to_giveaway[before_after$time_to_giveaway >= 0], na.rm = TRUE)

combined_data <- combined_data %>%
  mutate(publication_date = as.Date(ISOdate(publication_year, publication_month, publication_day)))

combined_data <- combined_data %>%
  mutate(pre_treatment_end = as.Date(publication_date) + avg_time_to_giveaway)

