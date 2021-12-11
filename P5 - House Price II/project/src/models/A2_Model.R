## Null Models

#- load in libraries
rm(list = ls())

library(data.table)
library(caret)
library(Metrics)
library(xgboost)

test <- fread("./project/volume/data/interim/test.csv")
train <- fread("./project/volume/data/interim/train.csv")

y.train <- train$SalePrice
y.test <- test$Id

train$SalePrice <- NULL
train$Id <- NULL
test$Id <- NULL


dummies <- dummyVars(~., data = train)
x.train <- predict(dummies, newdata = train)
x.test <- predict(dummies, newdata = test)

dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)


# Initialize my table
hyper_perm_tune <- NULL

## Selecting parameters
param <- list(  objective           = "reg:squarederror", ## root mean squared error
                gamma               = 0.02,   ## Min loss reduction 
                booster             = "gbtree", ## 
                eval_metric         = "rmse", ## Can set it to mean absolute error 
                eta                 = 0.05,   ## lambda from notes, default reate of eta is 0.3 
                max_depth           = 4,      ## Largest height of the tree, default is 6, typical values 4-8
                subsample           = 0.9,    ## 
                colsample_bytree    = 1.0,    ## Similar to random_forest, --> random forest every sample by node
                ## 
                tree_method = 'hist'          ## Approximate  
)


## Cross-validation
XGBfit <- xgb.cv(params = param,
                 nfold = 4,        ## 5fold cross-validation
                 nrounds = 10000,   ## Setting max number of trees, number doesn't matter in most cases
                 missing = NA,    ## 
                 data = dtrain,
                 print_every_n = 2, ## What to print
                 early_stopping_rounds = 25)   ## Training stops when no improvement on testing rmse value


## Manually building a table that keeps track of how well the parameters do
  ## extract best B
best_tree_n <- unclass(XGBfit)$best_iteration

## Keep track of how well parameters perform
  ## t --> transposing table
new_row <- data.table(t(param))
new_row$best_tree_n <- best_tree_n

## evaluation log --> extract how well the model performs --> extract test set rmse for best value
test_error <- unclass(XGBfit)$evaluation_log[best_tree_n,]$test_rmse_mean
new_row$test_error <- test_error

## save the information generated to empty table
## keeps track of how well model is doing compared to previous models
hyper_perm_tune <- rbind(new_row, hyper_perm_tune)

watchlist <- list( train = dtrain)

## Extract best model
param <- list(  objective           = "reg:squarederror", ## root mean squared error
                gamma               = 0.02,   ## Min loss reduction 
                booster             = "gbtree", ## 
                eval_metric         = "rmse", ## Can set it to mean absolute error 
                eta                 = 0.05,   ## lambda from notes, default reate of eta is 0.3 
                max_depth           = 6,      ## Largest height of the tree, default is 6, typical values 4-8
                subsample           = 0.9,    ## 
                colsample_bytree    = 1.0,    ## Similar to random_forest, --> random forest every sample by node
                ## 
                tree_method = 'hist'          ## Approximate  
)

XGBfit <- xgb.train( params = param,
                     nrounds = best_tree_n,
                     missing = NA,
                     data = dtrain,
                     watchlist = watchlist,
                     print_every_n = 1)

pred <- predict(XGBfit, newdata = dtest)

rmse(y.test,pred)

Submit <- data.table(y.test)
setnames(Submit, "y.test", "Id")
Submit$SalePrice <- pred

fwrite(Submit, './project/volume/data/processed/Submit_Null.csv')
