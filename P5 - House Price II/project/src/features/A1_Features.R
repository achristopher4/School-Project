## Features
library(data.table)
#library()

rm(list = ls())


train <- fread('./project/volume/data/raw/train.csv')
test <- fread('./project/volume/data/raw/test.csv')
sample_sub <- fread('./project/volume/data/raw/sample.csv')


# train[is.na(train$LotFrontage)]
colnames(train)
b <- train[is.na(train$LotFrontage), LotFrontage]
length(b)

# LotFrontage has na