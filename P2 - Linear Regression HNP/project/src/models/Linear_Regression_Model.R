## Generating Model

rm(list=ls())

library(caret)
library(data.table)
library(Metrics)
library(ggplot2)

train <- fread('./project/volume/data/interim/train.csv')
test <- fread('./project/volume/data/interim/test.csv')

train_y <- train$SalePrice
train <- train[,.SD, .SDcols = !c('SalePrice')]
dummies <- dummyVars(" ~ .", data=train)

train <- predict(dummies, newdata = train)
train <- data.table(train)
train$SalePrice <- train_y

fit <- lm(SalePrice ~. - BldgTypeTwnhsE - HeatingWall- CentralAirY, data = train)
summary(fit)

#test_training_data <- train[,.SD, .SDcols = !c('SalePrice')]
#test_training_data$Pred_SalePrice <- predict(fit, newdata = test_training_data)
#test_training_data <- data.table(test_training_data)
#test_training_data$Actual_SalePrice <- train_y


saveRDS(dummies,"./project/volume/models/DepDelay_linear_model.dummies")
saveRDS(fit,"./project/volume/models/fit_lm.model")

ColumnId <- data.table(test$Id)
test <- test[,.SD, .SDcols = !c('Id')]
test <- predict(dummies, newdata=test)
test <- data.table(test)
test$Pred_SalePrice <- predict(fit,newdata = test)


submit <- ColumnId
submit$SalePrice <-test[,.(Pred_SalePrice)]
setnames(submit, "V1", "Id")
  
fwrite(submit,'./project/volume/data/processed/submit_lm.csv')

