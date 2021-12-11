## Embedded

rm(list = ls())
library(httr)
library(data.table)

set.seed(4)

## Accessing a pre-trained Deep Neural Network
getEmbeddings <- function(text){ ## text == input
  input <- list(
    instances = list( text)
  )
  res <- POST("https://dsalpha.vmhost.psu.edu/api/use/v1/models/use:predict", body = input,encode = "json", verbose())
  ##                    Name of network                                         Input           file type     Output
  emb <- unlist(content(res)$predictions)
  ##       extract predictions  ^^
  emb
}

train <- fread('./project/volume/data/interim/train.csv')
emb_dt_train <- NULL

test <- fread('./project/volume/data/interim/test.csv')
emb_dt_test <- NULL

## create a data.table such that each sentence is a 512D vector
for (i in 1:nrow(train)) {
  emb_dt_train <- rbind(emb_dt_train, getEmbeddings(train$text[i]))
}

emb_dt_train <- data.table(emb_dt_train)

fwrite(emb_dt_train,"./project/volume/data/interim/Emd_DT_Train.csv")

for (i in 1:nrow(test)) {
  emb_dt_test <- rbind(emb_dt_test, getEmbeddings(test$text[i]))
}

emb_dt_test <- data.table(emb_dt_test)

fwrite(emb_dt_test,"./project/volume/data/interim/Emd_DT_Test.csv")
