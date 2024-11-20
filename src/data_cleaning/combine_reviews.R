# Load packages
library(tidyverse)

# Load data
reviews_thesis_notext <- fread("../../../Data/Giveaways/reviews_thesis_notext.csv")
reviews_thesis_text <- fread("../../../Data/Giveaways/reviews_thesis_text.csv")

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

## Export data
write_csv(reviews_thesis, "../../../Data/Giveaways/cleaned/reviews_thesis.csv")
