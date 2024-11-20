# Download required packages
install.packages("R.utils")
install.packages("data.table")

# Load required packages
library(R.utils)
library(data.table)

## Unzip large files
gunzip("../../Data/Giveaways/similar_rev_thesis.gz", remove = FALSE)

## Read data in R, for writing code now with a maximum of 1000 rows
similar_reviews <- fread("../../../Data/Giveaways/similar_rev_thesis", nrows = 1000)
similar_meta <- fread("../../../Data/Giveaways/similar_meta_thesis.csv", nrows = 1000)
similar_map <- fread("../../../Data/Giveaways/similar_map_thesis.csv", nrows = 1000)

## Export working files
fwrite(similar_reviews, file = "../../../Data/Giveaways/similar_reviews.csv")
