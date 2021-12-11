## XGBoost TNSE Final

rm(list = ls())

library(data.table)
library(xgboost)

set.seed(4)

train <- fread('./project/volume/data/interim/train.csv')
classification <- fread('./project/volume/data/interim/classification.csv')
test <- fread('./project/volume/data/interim/test.csv')
#sample <- fread('./project/volume/data/raw/example_sub.csv')
tsne_train <- fread("./project/volume/data/interim/tsne_train.csv")
tsne_test <- fread("./project/volume/data/interim/tsne_test.csv")

train_id <- data.table(train[,id])
test_id <- data.table(test[,id])

# Initialize my table
hyper_perm_tune <- NULL
num_classes <- length(unique(classification$variable))

y.train <- as.numeric(classification$cat_num)
dummies <- dummyVars(~., data = tsne_train)
x.train <- predict(dummies, newdata =  tsne_train)
t <- data.table(x.train)
dtrain <- xgb.DMatrix(x.train, label=y.train, missing=NA)

pt_eta <- c(0.005, 0.01, 0.05, 0.1, 0.3)
pt_max_depth <- c(4, 5, 6, 7)
pt_max_split <- c(50, 150, 256, 350)

d_result <- c()
bp_params <- c()
d_xgb <- c()

for (x in pt_eta) {
  for (y in pt_max_depth){
    for (z in pt_max_split){
      param <- list( objective = "multi:softprob",
                     max_bin = z,
                     gamma = 0.02,
                     booster             = "gbtree", 
                     eval_metric         = "mlogloss",
                     eta                 = x,
                     max_depth           = y,      
                     subsample           = 0.9,    
                     colsample_bytree    = 1.0,    
                     tree_method = 'hist',       
                     num_class = 10)
      
      
      XGBfit <- xgb.cv(params = param,
                       nfold = 5,    
                       nrounds = 10000, 
                       missing = NA,    
                       data = dtrain,
                       early_stopping_rounds = 25,
                       label = y.train)   
      
      best_tree_n <- unclass(XGBfit)$best_iteration 
      new_row <- data.table(t(param))
      new_row$best_tree_n <- best_tree_n
      
      test_error <- unclass(XGBfit)$evaluation_log[best_tree_n,]$test_mlogloss_mean 
      new_row$test_error <- test_error
      
      hyper_perm_tune <- rbind(new_row, hyper_perm_tune)
      
      watchlist <- list( train = dtrain)
      
      param <- list( objective = "multi:softprob",
                     max_bin = z,
                     gamma = 0.02,   
                     booster             = "gbtree", 
                     eval_metric         = "mlogloss",
                     eta                 = x,
                     max_depth           = y,      
                     subsample           = 0.9,    
                     colsample_bytree    = 1.0,    
                     tree_method = 'hist',       
                     num_class = 10,
                     nrounds = best_tree_n,
                     watchlist = watchlist,
                     missing = NA)
      
      xgb <- xgboost(data = dtrain, label = y.train, params = param, nrounds = best_tree_n)
      
      pred <- predict(xgb, newdata = dtrain, reshape = TRUE)
      
      d_result <- append(d_result, list(data.table(pred)))
      d_params <- append(bp_params, list(xgb))
      d_xgb <- append(d_xgb, list(xgb))
    }
  }
  print(x)
}

hyper_perm_tune$eta <- as.numeric(hyper_perm_tune$eta)
hyper_perm_tune$max_depth <- as.numeric(hyper_perm_tune$max_depth)
hyper_perm_tune$max_bin <- as.numeric(hyper_perm_tune$max_bin)

keycol <-c("eta","max_depth", "max_bin")
sorted_hpt <- setorderv(hyper_perm_tune, keycol)

fwrite(sorted_hpt,"./project/volume/models//hyper_perm_tune_TNSE_1.csv")
hpt2 <- fread('./project/volume/models//hyper_perm_tune_TNSE_1.csv')

best_param <- (setDT(hpt2)[ , .SD[which.min(test_error)]])
bp_index <- which(sorted_hpt$test_error == best_param$test_err)

best_xgb <- d_xgb[[bp_index]]
xgb.save(best_xgb,"./project/volume/models/xgb_best.model")




#lowest_test_error <- best_parm$test_error
#hyper_perm_tune <- rbind(new_row, hyper_perm_tune)

watchlist <- list( train = dtrain)

final_param <- list(objective = "multi:softprob",
                   max_bin = best_param[[2]],
                   gamma = 0.02,   
                   booster             = "gbtree", 
                   eval_metric         = "mlogloss",
                   eta                 = best_param[[6]],
                   max_depth           = best_param[[7]],
                   subsample           = 0.9,    
                   colsample_bytree    = 1.0,    
                   tree_method = 'hist',       
                   num_class = 10,
                   nrounds = best_param[[12]],
                   watchlist = watchlist,
                   missing = NA)

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

best_xgb <- xgboost(data = dtrain, label = y.train, params = final_param, nrounds = best_param[[12]])

x.test <- predict(dummies, newdata = tsne_test)
dtest <- xgb.DMatrix(x.test, missing=NA)

test_pred <- data.table(predict(best_xgb, dtest, reshape = TRUE))

setnames(test_pred, c("V1" , "V2" , "V3" , "V4" , "V5" , "V6" , "V7" , "V8" , "V9" , "V10"), 
         c("subredditcars", "subredditCooking", "subredditMachineLearning", "subredditmagicTCG", "subredditpolitics", "subredditReal_Estate", "subredditscience", "subredditStockMarket", "subreddittravel", "subredditvideogames"))

test_solution <- data.table(test$id)
setnames(test_solution, c("V1"), c("id"))
test_solution <- cbind(test_solution, test_pred)

#null <- fread('./project/volume/data/processed/Submit_Null.csv')

fwrite(test_solution, './project/volume/data/processed/TNSE_XGB_Attempt_3.csv')

