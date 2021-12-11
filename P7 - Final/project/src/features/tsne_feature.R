## TNSE Feature Selection

## PCA

rm(list = ls())

library(data.table)
library(Rtsne)

set.seed(4)

emd_data_Train <- fread("./project/volume/data/interim/Emd_DT_Train.csv")
emd_data_Test <- fread("./project/volume/data/interim/Emd_DT_Test.csv")


#####
## Create Master Table

master <- rbind(emd_data_Train, emd_data_Test, use.names = TRUE)

tsne1 <- Rtsne(master,
               pca = TRUE,
               perplexity = 30,
               check_duplicates = F,
               pca_scale = TRUE,
               pca_center = T,
               eta = 100,
               dims = 2
)

tsne2 <- Rtsne(master,
               pca = TRUE,
               perplexity = 40,
               check_duplicates = F,
               pca_scale = TRUE,
               pca_center = T,
               eta = 100,
               dims = 2
)

tsne3 <- Rtsne(master,
               pca = TRUE,
               perplexity = 50,
               check_duplicates = F,
               pca_scale = TRUE,
               pca_center = T,
               eta = 100,
               dims = 2
)

tsne4 <- Rtsne(master,
               pca = TRUE,
               perplexity = 60,
               check_duplicates = F,
               pca_scale = TRUE,
               pca_center = T,
               eta = 100,
               dims = 2
)

tsne_dt1 <- data.table(tsne1$Y)
tsne_dt2 <- data.table(tsne2$Y)
setnames(tsne_dt2, c("V1", "V2"), c("V3", "V4"))
tsne_dt3 <- data.table(tsne3$Y)
setnames(tsne_dt3, c("V1", "V2"), c("V5", "V6"))
tsne_dt4 <- data.table(tsne4$Y)
setnames(tsne_dt4, c("V1", "V2"), c("V7", "V8"))



####


master_tsne <- cbind(tsne_dt1, tsne_dt2, tsne_dt3, tsne_dt4)

tsne_train <- master_tsne[1:200]
tsne_test <- master_tsne[201:20754]

fwrite(tsne_train,"./project/volume/data/interim/tsne_train.csv")
fwrite(tsne_test,"./project/volume/data/interim/tsne_test.csv")



