# Load packages
library(tidyverse)

# Load data
reviews_thesis_notext <- fread("../../Data/Giveaways/reviews_thesis_notext.csv")
reviews_thesis_text <- fread("../../Data/Giveaways/reviews_thesis_text.csv")
reviews_thesis <- fread("../../../Data/Giveaways/cleaned/reviews_thesis.csv")

# Filter out text reviews without text
reviews_thesis_text <- reviews_thesis_text %>% 
  filter(!is.na(text) & text != "")

# Merge datasets
reviews_thesis <- reviews_thesis_notext %>% 
  left_join(
    reviews_thesis_text %>% select(new_review_id, text),
    by = "new_review_id"
  )

# Check for duplicates
reviews_thesis %>% 
  group_by(new_review_id) %>% 
  filter(n() > 1) %>% 
  ungroup()

reviews_thesis <- reviews_thesis %>% 
  select(-order_filter, -rating_filter, -review_id, -reviewer_name)
## Export data
write_csv(reviews_thesis, "../../Data/Giveaways/cleaned/reviews_thesis.csv")

summary(reviews_thesis)
