
rm(list = ls())
  
library(tidyverse)
library(data.table)
library(ggplot2)

Sample <- fread("./project/volume/data/raw/Sample_submission.csv")

Test <- fread("./project/volume/data/raw/Stat_380_test.csv")

Training <- fread("./project/volume/data/raw/Stat_380_train.csv")

#names(Training)

## Attempt 1
selectCol <- c("OverallQual", "OverallCond", "SalePrice")
groupBy <- c("OverallQual", "OverallCond")

QCavg <- Training[, .(SalePrice = mean(SalePrice)), by = c("BldgType")]

Submission <- merge(Test, 
                    QCavg,
                    all.x = T)

fwrite(Submission[, .(Id, SalePrice)], "./project/volume/data/processed/submission.csv")


## Attempt 2
QCdcast <- dcast(Training, 
                 OverallCond ~ OverallQual,
                 mean,
                 na.rm = T, 
                 value.var = c("SalePrice"))

BTQdcast <- dcast(Training, 
                 BldgType ~ OverallQual,
                 mean,
                 na.rm = T, 
                 value.var = c("SalePrice"))

BTCdcast <- dcast(Training, 
                  BldgType ~ OverallCond,
                  mean,
                  na.rm = T, 
                  value.var = c("SalePrice"))

QCmelt <- melt(QCdcast, id = c("OverallCond"))
BTQmelt <- melt(BTQdcast, id = c("BldgType"))
BTCmelt <- melt(BTCdcast, id = c("BldgType"))

setnames(QCmelt, "variable", "OverallQual")
setnames(QCmelt, "value", "SalePrice")

setnames(BTQmelt, "variable", "OverallQual")
setnames(BTQmelt, "value", "SalePrice")

setnames(BTCmelt, "variable", "OverallCond")
setnames(BTCmelt, "value", "SalePrice")

QCmelt$OverallQual <- as.integer(QCmelt$OverallQual)
BTQmelt$OverallQual <- as.integer(BTQmelt$OverallQual)
BTCmelt$OverallCond <- as.integer(BTCmelt$OverallCond)

Submission2 <- merge(Test, QCmelt, by.x= c("OverallCond", "OverallQual"),  by.y= c("OverallCond", "OverallQual"))

Replace_NA_1 <- merge(Test, BTQmelt, by.x= c("BldgType", "OverallQual"),  by.y= c("BldgType", "OverallQual"))
Replace_NA_2 <- merge(Test, BTCmelt, by.x= c("BldgType", "OverallCond"),  by.y= c("BldgType", "OverallCond"))

Submission2[is.na(SalePrice)]

Submission2[is.na(Submission2$SalePrice), ]$SalePrice <- Replace_NA_1[is.na(Submission2$SalePrice), ]$SalePrice
Submission2[is.na(Submission2$SalePrice), ]$SalePrice <- Replace_NA_2[is.na(Submission2$SalePrice), ]$SalePrice

fwrite(Submission2[, .(Id, SalePrice)], "./project/volume/data/processed/submission2.csv")

## Attempt 3

ttt <- dcast(Training, 
             CentralAir+Heating~BldgType,
             mean,
             na.rm = T,
             value.var = c("SalePrice"))
