
library(readr)
library(lubridate)
library(tidyr)
library(dplyr)
library(caret)
library(randomForest)
library(ranger)
library(data.table)

load("data/data_sampled.rdata")
data<-data_sampled
rm(data_sampled)


##Formula
f <- as.formula(
  paste("trip_duration", 
  paste(features, collapse = " + "), 
  sep = " ~ ")
  )

linear <- lm(f,data=data)

tr <-trainControl(method="cv",number=3) 
cv.lin<- train(f, method="lm" ,data=data, trControl = tr)


##package randomForest vraiment trèèèès lent
rf.ranger <-ranger(data=data[,c("weekDay","distance", "month", "pickupHour","trip_duration")], dependent.variable.name="trip_duration", classification =
            F, max.depth=5, min.node.size=100)


tr2 <-trainControl(method="cv",number=3) 

grille = data.frame(mtry=c(20),splitrule=c('variance'),min.node.size=c(100))

f <- as.formula(
  paste("trip_duration", 
        paste(c("weekDay","distance", "month", "pickupHour","pickupBcode","dropoffBcode"), collapse = " + "), 
        sep = " ~ ")
)
cv.rf<- train(f, method="ranger" ,data=data, trControl = tr2,metric="MAE",tuneGrid = grille)


### XG boost

xgbGrid <- expand.grid(#nrounds = c(100,200),
                      nrounds = 100,
                       #max_depth = c(3, 5, 10, 15, 20),
                       max_depth = 10,
                       #colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       colsample_bytree = seq(0.5, 0.5, length.out = 1),
                       ## valeurs par défaut : 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

require(Matrix)
sparse_matrix <- sparse.model.matrix(trip_duration ~ ., data = data %>% select(c("trip_duration","weekDay","distance", "month", "pickupHour","pickupBcode","dropoffBcode")))[,-1]
X_train_dmat = xgb.DMatrix(sparse_matrix, label = data$trip_duration)
X_train = xgb.DMatrix(as.matrix(data %>% select(c("weekDay","distance", "month", "pickupHour","pickupBcode","dropoffBcode"))))
y_train = data$trip_duration

xgb_model = train(X_train, y_train, trControl = tr2, tuneGrid = xgbGrid, 
                  method = "xgbTree")

