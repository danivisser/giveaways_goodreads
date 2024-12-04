library(fixest)
library(panelr)
library(tidyverse)
library(data.table)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)
library(did)

did_complete <- fread("../../../Data/Giveaways/cleaned/all_reviews_fog_coherence.csv")

did_complete <- did_complete %>% 
  select(-1, -2, -new_review_id, -text)

did_complete$year_month <- floor_date(did_complete$time, unit = "month")

did_complete <- did_complete[year(year_month) >= 2007]

did_complete <- did_complete %>% 
  select(-time)

min_date <- min(did_complete$year_month)
max_date <- max(did_complete$year_month)

date_range <- seq(min_date, max_date, by = "month")
date_df <- data.frame(date = date_range)
date_df$period <- seq_len(nrow(date_df))

did_complete$date_giveaway<- as.Date(as.yearmon(did_complete$pre_period_end))
#did_complete$giveaway_end_ym<- as.Date(as.yearmon(did_complete$post_period_start))

did_complete <- merge(did_complete, date_df, by.x = "year_month", by.y = "date", all.x = TRUE)
did_complete <- merge(did_complete, date_df, by.x = "date_giveaway", by.y = "date", all.x = TRUE)

names(did_complete)[names(did_complete) == "period.x"] <- "period_review"
names(did_complete)[names(did_complete) == "period.y"] <- "period_giveaway"

did_complete$period_review<- as.numeric(did_complete$period_review)
did_complete$book_id<- as.numeric(as.factor(did_complete$book_id))

did_complete <- did_complete %>% 
  mutate(period_giveaway = ifelse(treatment == 0, 1000, period_giveaway))

did_complete <- did_complete %>% 
  mutate(relative_time = case_when(
    treatment == 1 ~ period_review - period_giveaway,
    TRUE ~ NA_real_
  )) 

obs_by_time <- did_complete %>% 
  group_by(relative_time) %>% 
  summarise(num_obs = n())

ggplot(obs_by_time, aes(x = relative_time, y = num_obs)) + 
  geom_bar(stat = "identity") + 
  theme_minimal()

did_pre_post <- did_complete %>% 
  filter(
    (relative_time >= -10 & relative_time <= 50 & treatment == 1) | (treatment == 0),
    FOG <= 20)

did_pre_post <- did_pre_post %>% 
  select(-date_giveaway, -pre_period_start, -pre_period_end, -post_period_start, -post_period_end)

write.csv(did_pre_post, "../../../Data/Giveaways/cleaned/did_pre_post.csv")
did_pre_post <- fread("../../../Data/Giveaways/cleaned/did_pre_post.csv")

obs_by_time <- did_pre_post %>% 
  group_by(relative_time) %>% 
  summarise(num_obs = n())

ggplot(obs_by_time, aes(x = relative_time, y = num_obs)) + 
  geom_bar(stat = "identity") + 
  theme_minimal()

did_pre_post_10 <- did_pre_post %>% 
  filter(
    (relative_time >= -6 & relative_time <= 20 & treatment == 1) | (treatment == 0))
    
res_sa10 = feols(FOG ~ sunab(period_giveaway, period_review) | book_id, cluster = "book_id", did_pre_post_10)
res_sa_20 = feols(FOG ~ sunab(period_giveaway, period_review) | book_id, cluster = "book_id", lean = TRUE, did_pre_post_20)

iplot(res_sa10)

summary(res_sa10, agg = "att")

table(merged_df$period_giveaway)

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
