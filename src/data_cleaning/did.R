similar_reviews_dates_text <- fread("../../../Data/Giveaways/cleaned/similar_review_dates_text.csv")

review_dates_text$treatment <- 1
similar_reviews_dates_text$treatment <- 0

similar_reviews_dates_text <- similar_reviews_dates_text %>% 
  select(-V1)

review_dates_text <- review_dates_text %>% 
  select(-V1)

similar_reviews_dates_text <- similar_reviews_dates_text %>% 
  rename(book_id = similar_book_id)

reviews <- rbind(review_dates_text, similar_reviews_dates_text)

write.csv(reviews, "../../../Data/Giveaways/cleaned/reviews_did.csv")

similar_reviews_dates_notext <- similar_review_dates %>% 
  filter(is.na(text) | text == "") %>% 
  mutate(treatment = 0,
         text = NA,
         FOG = NA,
         word_count = NA) %>% 
  rename(book_id = similar_book_id) %>% 
  select(-V1)

reviews_1 <- rbind(reviews, similar_reviews_dates_notext)

reviews_dates_notext <- review_dates %>% 
  filter(is.na(text) | text == "") %>% 
  mutate(treatment = 1,
         text = NA,
         FOG = NA,
         word_count = NA) %>% 
  select(-V1)

reviews_2 <- rbind(reviews_1, reviews_dates_notext)

write.csv(reviews_2, "../../../Data/Giveaways/cleaned/reviews_did_complete.csv")

