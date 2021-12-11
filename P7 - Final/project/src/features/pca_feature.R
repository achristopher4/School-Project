## PCA

rm(list = ls())

library(data.table)
library(caret)

set.seed(4)

emd_data_Train <- fread("./project/volume/data/interim/Emd_DT_Train.csv")
emd_data_Test <- fread("./project/volume/data/interim/Emd_DT_Test.csv")


#####
## Create Master Table

master <- rbind(emd_data_Train, emd_data_Test, use.names = TRUE)


pca <- prcomp(master, tol = sqrt(.Machine$double.eps), scale = TRUE, subset = T)
pca_dt <- data.table(unclass(pca)$x)

pca_summmary <- summary(pca)

#####

pca_train <- pca_dt[1:200]
pca_test <- pca_dt[201:20754]

fwrite(pca_train,"./project/volume/data/interim/pca_dt_train.csv")
fwrite(pca_test,"./project/volume/data/interim/pca_dt_test.csv")