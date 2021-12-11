## XGBoost Model

rm(list = ls())

library(data.table)
library(xgboost)
library(caret)
library(Metrics)

set.seed(4)

emd_data_Train <- fread("./project/volume/data/interim/Emd_DT_Train.csv")
emd_data_Test <- fread("./project/volume/data/interim/Emd_DT_Test.csv")
train <- fread('./project/volume/data/interim/train.csv')
classification <- fread('./project/volume/data/interim/classification.csv')
test <- fread('./project/volume/data/interim/test.csv')
sample <- fread('./project/volume/data/raw/example_sub.csv')

y.train <- as.numeric(classification$cat_num)
y.id <- classification$id

classification$cat_num <- NULL
classification$id <- NULL

dummies <- dummyVars(~., data = emd_data_Train)
x.train <- predict(dummies, newdata = emd_data_Train)

t <- data.table(x.train)

dtrain <- xgb.DMatrix(x.train, label=y.train, missing=NA)

# Initialize my table
hyper_perm_tune <- NULL

num_classes <- length(unique(classification$variable))

## Selecting parameters
param <- list(  objective           = "multi:softprob", ## root mean squared error
                gamma               = 0.02,   ## Min loss reduction 
                booster             = "gbtree", ## 
                eval_metric         = "rmse", ## Can set it to mean absolute error 
                eta                 = 0.05,   ## lambda from notes, default reate of eta is 0.3 
                max_depth           = 5,      ## Largest height of the tree, default is 6, typical values 4-8
                subsample           = 0.9,    ## 
                colsample_bytree    = 1.0,    ## Similar to random_forest, --> random forest every sample by node
                tree_method = 'hist',       ## Approximate 
                nrounds = 183,
                num_class = 10
)

watchlist <- list(train = dtrain)

xgb <- xgboost(data = dtrain, label = y.train, nrounds = 160, num_class = 10, 
               objective = "multi:softprob")

train_pred <- predict(xgb, dtrain, reshape = TRUE)
train_solution <- data.table(train_pred)

## cols
  ## V10 - subredditvideogames
  ## V9  - subreddittravel
  ## V8  - subredditStockMarket
  ## V7  - subredditscience
  ## V6  - subredditReal_Estate
  ## V5  - subredditpolitics
  ## V4  - subredditmagicTCG
  ## V3  - subredditMachineLearning
  ## V2  - subredditCooking
  ## V1  - subredditcars

x.test <- predict(dummies, newdata = emd_data_Test)
dtest <- xgb.DMatrix(x.test, missing=NA)

test_pred <- data.table(predict(xgb, dtest, reshape = TRUE))

setnames(test_pred, c("V1" , "V2" , "V3" , "V4" , "V5" , "V6" , "V7" , "V8" , "V9" , "V10"), 
         c("subredditcars", "subredditCooking", "subredditMachineLearning", "subredditmagicTCG", "subredditpolitics", "subredditReal_Estate", "subredditscience", "subredditStockMarket", "subreddittravel", "subredditvideogames"))

test_solution <- data.table(test$id)
setnames(test_solution, c("V1"), c("id"))
test_solution <- cbind(test_solution, test_pred)

fwrite(test_solution, './project/volume/data/processed/Submit_Null.csv')



