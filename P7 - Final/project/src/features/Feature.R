## Clean Data

rm(list = ls())

library(data.table)

set.seed(4)

test <- fread("./project/volume/data/raw/test_data.csv")
train <- fread("./project/volume/data/raw/train_data.csv")

#- Remove all the empty strings
test <- test[!test[,text == ""]]
train <- train[!train[,text == ""]]

#- Save the cleaned data
fwrite(test,"./project/volume/data/interim/test.csv")
fwrite(train,"./project/volume/data/interim/train.csv")

