## PCA & XGBoost Model

rm(list = ls())

library(data.table)
library(xgboost)
library(caret)
library(ClusterR)
library(Metrics)
library(ModelMetrics)

set.seed(4)

emd_data_Train <- fread("./project/volume/data/interim/Emd_DT_Train.csv")
emd_data_Test <- fread("./project/volume/data/interim/Emd_DT_Test.csv")
train <- fread('./project/volume/data/interim/train.csv')
classification <- fread('./project/volume/data/interim/classification.csv')
test <- fread('./project/volume/data/interim/test.csv')
sample <- fread('./project/volume/data/raw/example_sub.csv')
pca_train <- fread("./project/volume/data/interim/pca_dt_train.csv")
pca_test <- fread("./project/volume/data/interim/pca_dt_test.csv")


train_id <- data.table(train[,id])
test_id <- data.table(test[,id])


# Initialize my table
hyper_perm_tune <- NULL

num_classes <- length(unique(classification$variable))

y.train <- as.numeric(classification$cat_num)
dummies <- dummyVars(~., data = pca_train)
x.train <- predict(dummies, newdata =  pca_train)
t <- data.table(x.train)
dtrain <- xgb.DMatrix(x.train, label=y.train, missing=NA)

#####
## For loop for xgboost

pt_eta <- c(0.01, 0.05, 0.1, 0.15, 0.2)
pt_max_depth <- c(4, 5, 6)
pt_gamma <- c(0.02, 0.1, 0.5, 1, 5)
pt_max_split <- c(50, 100, 150, 300)

d_result <- c()
bp_params <- c()

for (x in pt_eta) {
   for (y in pt_max_depth){
      for (z in pt_gamma){
         for (w in pt_max_split){
            param <- list( objective = "multi:softprob",
                           max_bin = w,
                           gamma = z,   
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
                           max_bin = w,
                           gamma = z,   
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
            bp_params <- append(bp_params, list(param, xgb))
         }
    }
 }
  print(x)
}

#####

#fwrite(pca_dt,"./project/volume/data/interim/pca_dt.csv")
fwrite(hyper_perm_tune,"./project/volume/data/interim/hyper_perm_tune_PCA_3.csv")

#####

hpt2 <- fread('./project/volume/data/interim/hyper_perm_tune_PCA_2.csv')
## Best Params
   ##

best_parm <- (setDT(hyper_perm_tune)[ , .SD[which.min(test_error)]])[,1:11]

best_param <- list( objective = "multi:softprob",
                    #max_bin = 250,
                    gamma = 0.02,   
                    booster             = "gbtree", 
                    eval_metric         = "mlogloss",
                    eta                 = 0.05,
                    max_depth           = 5,      
                    subsample           = 0.9,    
                    colsample_bytree    = 1.0,    
                    tree_method = 'hist',       
                    #nrounds = 101,
                    missing = NA,
                    num_class = 10,
                    min_child_weight = 0.5
                    #feature_selector = "greedy"
                    #grow_policy = "lossguide"
                    #sample_type = "weighted"
)


xgb <- xgboost(data = dtrain, params = best_param, label = y.train, nrounds = 129)

t2 <- data.table(predict(xgb, newdata = dtrain, reshape = TRUE))
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

x.test <- predict(dummies, newdata = pca_test)
dtest <- xgb.DMatrix(x.test, missing=NA)

test_pred <- data.table(predict(xgb, dtest, reshape = TRUE))

setnames(test_pred, c("V1" , "V2" , "V3" , "V4" , "V5" , "V6" , "V7" , "V8" , "V9" , "V10"), 
         c("subredditcars", "subredditCooking", "subredditMachineLearning", "subredditmagicTCG", "subredditpolitics", "subredditReal_Estate", "subredditscience", "subredditStockMarket", "subreddittravel", "subredditvideogames"))

test_solution <- data.table(test$id)
setnames(test_solution, c("V1"), c("id"))
test_solution <- cbind(test_solution, test_pred)

null <- fread('./project/volume/data/processed/Submit_Null.csv')

fwrite(test_solution, './project/volume/data/processed/PCA_XGB_Attempt_!!!.csv')



