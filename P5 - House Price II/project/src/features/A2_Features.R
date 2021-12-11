## Null Features
library(data.table)

rm(list = ls())

train <- fread('./project/volume/data/raw/train.csv')
test <- fread('./project/volume/data/raw/test.csv')
sample_sub <- fread('./project/volume/data/raw/sample.csv')


#just cleaning up my data and creating test and train set
train[is.na(train$LotFrontage)]$LotFrontage <- 0



fwrite(train,'./project/volume/data/interim/train.csv')
fwrite(test,'./project/volume/data/interim/test.csv')
