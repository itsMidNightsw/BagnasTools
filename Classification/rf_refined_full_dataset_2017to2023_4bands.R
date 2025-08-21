#random forest refined full dataset (4bands 2017to2023)

library(randomForest)
library(datasets)
library(caret)
library(microbenchmark)
library(devtools)
#install_github("araastat/reprtree")
#devtools::install("reprtree-master")
library(reprtree)


datarf = read.csv(file = paste(getwd(),"/Data/Data_Random_Forest/refined_full_polygon_dataset_2017to2023_4bands.csv", sep = ""))

#datarf = datarf[-c(1)]

datarf$pre_abs = as.factor(datarf$pre_abs)
datarf$date = as.factor(datarf$date)
datarf$rouge = as.numeric(datarf$rouge)
datarf$pir = as.numeric(datarf$pir)
datarf[which(is.na(datarf$rouge)),8] = 0
datarf[which(is.na(datarf$pir)),9] = 0
summary(datarf) # na check
presabs = datarf[,c(6:10)]
#presabs = datarf[,c(3,6:10)]#avec date

##RF presabs
set.seed(222)
ind <- sample(2, nrow(presabs), replace = TRUE, prob = c(0.8, 0.2))
train <- presabs[ind==1,]
test <- presabs[ind==2,]
#test$date = as.character(test$date)#avec date
#test$date = as.numeric(test$date)#avec date
#test2017 = test[test$date < 20230000,]#avec date
#test2023 = test[test$date > 20230000,]#avec date

##actual random forest

rfpa <- randomForest(pre_abs~., ntree = 500,data=train, proximity=TRUE, na.action = na.omit)
#rfpa <- randomForest(pre_abs~bleu+vert+rouge+pir, ntree = 500,data=train, proximity=TRUE, na.action = na.omit) #avec date
#rfpa <- randomForest(presence.absence~., ntree = 500, mtry = 2 ,data=train, proximity=TRUE)#difference ?
#rfpa <- randomForest(presence.absence~., ntree = 500, mtry = 1 ,data=train, proximity=TRUE)
print(rfpa)




## Prediction & Confusion Matrix – train data

p1pa <- predict(rfpa, train)
confusionMatrix(p1pa, train$ pre_abs)

## Prediction & Confusion Matrix – test data

p2pa <- predict(rfpa, test)
confusionMatrix(p2pa, test$ pre_abs)




varImpPlot(rfpa, main = "Relative importance of the bands in the Random Forest classification",
           labels = c("Blue","Green", "Red","Near Infrared")) 






RFDoTrace <- randomForest(pre_abs~., ntree = 500,data=train, proximity=TRUE,
                          do.trace = 10, na.action = na.omit)



plot(RFDoTrace)
legend("topright", "try")
plot(rfpa)

reprtree:::plot.getTree(rfpa, k = 1, depth = 5, cex = 1)
#treetest = getTree(rfpa, k = 1)
#partialPlot(rfpa, x.var = pir, pred.data = test)
#MDSplot(rfpa, fac = pre_abs)














############# With randomforestgls (takes spatial autocorrelation into acc) ###############
# 
# library(RandomForestsGLS)
# 
# datarf = read.csv(file = "~/Thèse/Random Forest/refined_full_polygon_dataset_2017to2023_4bands.csv")
# #datarf = datarf[-c(1)]
# 
# datarf$pre_abs = as.factor(datarf$pre_abs)
# datarf$date = as.factor(datarf$date)
# datarf$rouge = as.numeric(datarf$rouge)
# datarf$pir = as.numeric(datarf$pir)
# datarf[which(is.na(datarf$rouge)),8] = 0
# datarf[which(is.na(datarf$pir)),9] = 0
# summary(datarf) # na check
# presabs = datarf#[,c(6:10)]
# #presabs = datarf[,c(3,6:10)]#avec date

# 
# presabs$date
# data2017_1 = subset(presabs, presabs$date == "20170314")
# data2017_2 = subset(presabs, presabs$date == "20170403")
# data2017_2$x = data2017_2$x + 10000
# data2017_2$y = data2017_2$x + 100000
# data2017_3 = subset(presabs, presabs$date == "20170523")
# data2017_3$x = data2017_3$x + 20000
# data2017_3$y = data2017_3$x + 200000
# data2017_4 = subset(presabs, presabs$date == "20170612")
# data2017_4$x = data2017_4$x + 30000
# data2017_4$y = data2017_4$x + 300000
# data2017_5 = subset(presabs, presabs$date == "20170707")
# data2017_5$x = data2017_5$x + 40000
# data2017_5$y = data2017_5$x + 400000
# data2017_6 = subset(presabs, presabs$date == "20170806")
# data2017_6$x = data2017_6$x + 50000
# data2017_6$y = data2017_6$x + 500000
# data2017_7 = subset(presabs, presabs$date == "20170905")
# data2017_7$x = data2017_7$x + 60000
# data2017_7$y = data2017_7$x + 600000
# 
# data2023_1 = subset(presabs, presabs$date == "20230226")
# data2023_1$x = data2023_1$x + 70000
# data2023_1$y = data2023_1$x + 700000
# data2023_2 = subset(presabs, presabs$date == "20230328")
# data2023_2$x = data2023_2$x + 80000
# data2023_2$y = data2023_2$x + 800000
# data2023_3 = subset(presabs, presabs$date == "20230417")
# data2023_3$x = data2023_3$x + 90000
# data2023_3$y = data2023_3$x + 900000
# data2023_4 = subset(presabs, presabs$date == "20230517")
# data2023_4$x = data2023_4$x + 100000
# data2023_4$y = data2023_4$x + 1000000
# data2023_5 = subset(presabs, presabs$date == "20230601")
# data2023_5$x = data2023_5$x + 110000
# data2023_5$y = data2023_5$x + 1100000
# data2023_6 = subset(presabs, presabs$date == "20230626")
# data2023_6$x = data2023_6$x + 120000
# data2023_6$y = data2023_6$x + 1200000
# data2023_7 = subset(presabs, presabs$date == "20230716")
# data2023_7$x = data2023_7$x + 130000
# data2023_7$y = data2023_7$x + 1300000
# 
# 
# presabs = data2023_7
# 
# presabsV2 = rbind(data2017_1,data2017_2,data2017_3,data2017_4,data2017_5,data2017_6,data2017_7,
#                   data2023_1,data2023_2,data2023_3,data2023_4,data2023_5,data2023_6,data2023_7)
# 
# vecadd = c(1:7112)
# 
# presabsV2$x = presabsV2$x + vecadd
# 
# set.seed(222)
# ind <- sample(2, nrow(presabs), replace = TRUE, prob = c(0.8, 0.2))
# train <- presabs[ind==1,]
# test <- presabs[ind==2,]
# 
# 
# coords_train = as.matrix(train[,c(1,2)])
# coords_test = as.matrix(test[,c(1,2)])
# 
# y_train = train$pre_abs
# y_train = ifelse(y_train == "abs", 0, 1)
# #y_train = as.factor(y_train)
# y_test = test$pre_abs
# y_test = ifelse(y_test == "abs", 0, 1)
# #y_test = as.factor(y_test)
# 
# x_train = as.matrix(train[,c(6:9)])
# x_test = as.matrix(test[,c(6:9)])
# 
# est_unknown <- RFGLS_estimate_spatial(coords_train, y_train, x_train, ntree = 10, cov.model = "exponential",
#                                       nthsize = 100, param_estimate = TRUE)
# 
# 
# 
# 
# pred<- RFGLS_predict(est_unknown, x_test)
# 
# 




