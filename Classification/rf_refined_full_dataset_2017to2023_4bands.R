#random forest refined full dataset (4bands 2017to2023)

library(randomForest)
library(caret)


datarf = read.csv(file = paste(getwd(),"/Data/Data_Random_Forest/refined_full_polygon_dataset_2017to2023_4bands.csv", sep = ""))


datarf$pre_abs = as.factor(datarf$pre_abs)
datarf$date = as.factor(datarf$date)
datarf$rouge = as.numeric(datarf$rouge)
datarf$pir = as.numeric(datarf$pir)
datarf[which(is.na(datarf$rouge)),8] = 0
datarf[which(is.na(datarf$pir)),9] = 0
summary(datarf) # na check
presabs = datarf[,c(6:10)]


##RF presabs
set.seed(222)
ind <- sample(2, nrow(presabs), replace = TRUE, prob = c(0.8, 0.2))
train <- presabs[ind==1,]
test <- presabs[ind==2,]


##actual random forest
rfpa <- randomForest(pre_abs~., ntree = 500,data=train, proximity=TRUE, na.action = na.omit)
print(rfpa)


## Prediction & Confusion Matrix – train data

p1pa <- predict(rfpa, train)
confusionMatrix(p1pa, train$ pre_abs)

## Prediction & Confusion Matrix – test data

p2pa <- predict(rfpa, test)
confusionMatrix(p2pa, test$ pre_abs)


## plot
varImpPlot(rfpa, main = "Relative importance of the bands in the Random Forest classification",
           labels = c("Blue","Green", "Red","Near Infrared")) 







