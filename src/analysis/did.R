library(fixest)
library(panelr)
library(tidyverse)
library(data.table)
library(jsonlite)

lines <- readLines("../../../Data/Giveaways/cleaned/coherence_scores.json")
json_data <- lapply(lines, fromJSON)
json_df <- do.call(rbind.data.frame, json_data)
reviews_test <- inner_join(reviews, json_df, by = "new_review_id")
write.csv(reviews_test, "../../../Data/Giveaways/cleaned/all_reviews_fog_coherence.csv")

did_complete <- fread("../../../Data/Giveaways/cleaned/all_reviews_fog_coherence.csv")

did_complete <- did_complete %>% 
  mutate(year_month = paste(publication_year, publication_month, sep="-"))

did_complete_inf <- did_complete %>% 
  filter(FOG != Inf)

before_after_analysis <- did_complete_inf %>% 
  filter(treatment == 1)

before_after_quantity <- before_after_analysis %>% 
  group_by(book_id, POST, shelf, year_month) %>% 
  summarise(num_reviews = n(),
            .groups = "drop")

number_twfe <- feols(
  num_reviews ~ POST | year_month + book_id, 
  cluster = "book_id", 
  data = before_after_quantity
)

summary(number_twfe)

number_twfe_mod <- feols(
  num_reviews ~ POST + shelf + POST*shelf | year_month + book_id, 
  cluster = "book_id",
  data = before_after_quantity
)

summary(number_twfe_mod)

length_gf <- feols(
  word_count ~ POST | year_month + book_id, 
  cluster = "book_id",
  data = before_after_analysis
)

summary(length_gf)

length_gf_mod <- feols(
  word_count ~ POST + shelf + POST*shelf | year_month + book_id, 
  cluster = "book_id",
  data = before_after_analysis
)

summary(length_gf_mod)

twfe_gf_mod <- feols(
  FOG ~ POST + shelf + POST*shelf | year_month + book_id, 
  cluster = "book_id",
  data = before_after_analysis
)

summary(twfe_gf_mod)

twfe_gf <- feols(
  FOG ~ POST | year_month + book_id, 
  cluster = "book_id",
  data = before_after_analysis
)

summary(twfe_gf)

twfe_gf_mod <- feols(
  FOG ~ POST + shelf + POST*shelf | year_month + book_id, 
  cluster = "book_id",
  data = before_after_analysis
)

summary(twfe_gf_mod)

did_quantity <- did_complete_inf %>% 
  group_by(book_id, POST, treatment, shelf, year_month) %>% 
  summarise(num_reviews = n(),
            .groups = "drop")

number_did <- feols(
  num_reviews ~ POST + treatment + POST*treatment | year_month + book_id,
  cluster = "book_id",
  data = did_quantity
)

summary(number_did)

number_did_mod <- feols(
  num_reviews ~ POST + treatment + POST*treatment + shelf + POST*shelf + treatment*shelf + POST*treatment*shelf | year_month + book_id,
  cluster = "book_id",
  data = did_quantity
)

summary(number_did_mod)

length_did <- feols(
  word_count ~ POST + treatment + POST*treatment | year_month + book_id,
  cluster = "book_id",
  data = did_complete_inf
)

summary(length_did)

length_did_mod <- feols(
  word_count ~ POST + treatment + POST*treatment + shelf + POST*shelf + treatment*shelf + POST*treatment*shelf | year_month + book_id,
  cluster = "book_id",
  data = did_complete_inf
)

summary(length_did_mod)

GOF_did <- feols(
  FOG ~ POST + treatment + POST*treatment | year_month + book_id,
  cluster = "book_id",
  data = did_complete_inf
)

summary(GOF_did)

GOF_did_mod <- feols(
  FOG ~ POST + treatment + POST*treatment + shelf + POST*shelf + treatment*shelf + POST*treatment*shelf | year_month + book_id,
  cluster = "book_id",
  data = did_complete_inf
)

summary(GOF_did_mod)
