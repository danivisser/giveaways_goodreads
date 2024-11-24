# Download required packages
install.packages("R.utils")
install.packages("data.table")

# Load required packages
library(R.utils)
library(data.table)

## Unzip large files
gunzip("../../Data/Giveaways/similar_rev_thesis.gz", remove = FALSE)

## Read data in R
similar_reviews <- fread("../../../Data/Giveaways/similar_rev_thesis")

## Export working files
fwrite(similar_reviews, file = "../../../Data/Giveaways/cleaned/similar_reviews.csv")
