rm(list= ls())

library(data.table)

train <- fread('./project/volume/data/raw/Stat_380_train.csv')

test <- fread('./project/volume/data/raw/Stat_380_test.csv')

train <- train[!is.na(train$SalePrice)][,.SD, .SDcols = !c('Id')]
train[is.na(train)] = 0
train$BldgType <- as.character(train$BldgType)
train$Heating <- as.character(train$Heating)
train$CentralAir <- as.character(train$CentralAir)

test[is.na(test)] =0
test$BldgType <- as.character(test$BldgType)
test$Heating <- as.character(test$Heating)
test$CentralAir <- as.character(test$CentralAir)

fwrite(train, './project/volume/data/interim/train.csv')
fwrite(test, './project/volume/data/interim/test.csv')

