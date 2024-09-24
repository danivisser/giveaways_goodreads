install.packages("R.utils")
install.packages("data.table")

library(R.utils)
library(data.table)

gunzip("../../Data/Giveaways/similar_rev_thesis.gz", remove = FALSE)

data <- fread("../../Data/Giveaways/similar_rev_thesis")

fwrite(data, file = "../../Data/Giveaways/similar_books.csv")
