# Load library
library(tidyverse)
library(ggplot2)

# Import data
book_genre <- fread("../../../Data/Giveaways/cleaned/book_genre_filtered.csv")

## Calculate the count of genres
genre <- book_genre %>% 
  group_by(shelf) %>% 
  summarise(total_books = n()) %>%
  arrange(desc(total_books)) %>% 
  slice_head(n = 5)

## Graph of top 5 genres in dataset
genres <- ggplot(genre, aes(x = reorder(shelf, total_books), y = total_books, fill = shelf)) +
  geom_bar(stat = "identity")

## Write created files
write_csv(genre, "../../../Output/top_5_genres.csv")
ggsave("../../../Output/top_5_genres.pdf", plot = genres, width = 8, height = 6)
