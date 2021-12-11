## Classification

rm(list = ls())

library(data.table)

set.seed(4)

train <- fread('./project/volume/data/interim/train.csv')
test <- fread('./project/volume/data/interim/test.csv')

## Classification of the train dataset
train_new_id <- train
train_new_id$SaveOrder <- seq.int(nrow(train_new_id))
melt_train <- melt(train_new_id[,!c("text")], id.vars = c("id", "SaveOrder"), measure.vars = colnames(train_new_id[,3:12]))
melt_train <- melt_train[(melt_train$value == 1)]

classification <- train_new_id[,c("id", "text", "SaveOrder")]
classification <- merge(classification, melt_train[,1:3], by="SaveOrder")
classification <- classification[order(classification$SaveOrder)][,c("id.x", "text", "variable")]
classification$cat_num <- as.numeric(classification$variable) -1

setnames(classification, c("id.x"), c("id"))

fwrite(classification,"./project/volume/data/interim/classification.csv")
