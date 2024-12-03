library(fixest)
library(panelr)
library(tidyverse)
library(data.table)


did_complete <- fread("../../../Data/Giveaways/cleaned/all_reviews_fog_coherence.csv")

did_complete <- did_complete %>% 
  select(-1, -2)

did_complete$year_month <- floor_date(did_complete$time, unit = "month")

did_complete <- did_complete[year(year_month) >= 2007]

min_date <- min(did_complete$year_month)
max_date <- max(did_complete$year_month)

date_range <- seq(min_date, max_date, by = "month")
date_df <- data.frame(date = date_range)
date_df$period <- seq_len(nrow(date_df))

did_complete$giveaway_start_ym<- as.Date(as.yearmon(did_complete$pre_period_end))
did_complete$giveaway_end_ym<- as.Date(as.yearmon(did_complete$post_period_start))

merged_df <- merge(did, date_df, by.x = "year_month", by.y = "date", all.x = TRUE)

merged_df <- merge(merged_df, date_df, by.x = "giveaway_start_ym", by.y = "date", all.x = TRUE)

names(merged_df)[names(merged_df) == "period.x"] <- "period_review"
names(merged_df)[names(merged_df) == "period.y"] <- "period_giveaway"

merged_df$treat <- ifelse(merged_df$period_review >= merged_df$period_giveaway, 1, 0)

merged_df$period_review<- as.numeric(merged_df$period_review)
merged_df$book_id<- as.numeric(as.factor(merged_df$book_id))

staggered_notyettreated <- att_gt(yname = "ratings",
                                  tname = "period_review",
                                  idname = "book_id",
                                  gname = "period_giveaway",
                                  control_group = "notyettreated",
                                  data = merged_df,
                                  allow_unbalanced_panel = TRUE
)

staggered_notyettreated_aggregate<- aggte(staggered_notyettreated, type = "dynamic", na.rm = TRUE)

staggered_notyettreated_plot<- ggdid(staggered_notyettreated_aggregate)+ labs(x = "Time Relative to Q&A Adoption (in 30-day bins)", y = "ATT")
print(staggered_notyettreated_plot)


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
