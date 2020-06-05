library(readr)
library(lubridate)
library(tidyr)
library(dplyr)
library(caret)
library(randomForest)
library(ranger)


## GET prepared dataset ###
data <- read.csv2(file="taxiTrainDataSet.csv",sep=";",header=TRUE, row.names = NULL)
features_to_remove <- c("id","bbox.x","note.x","geometry","bbox.y","note.y")
#data <-data[,!(names(data) %in% features_to_remove)]
data <- data %>% select(-features_to_remove)

##Transform data type
num_cols<-c("distance","maxTemp","maxWind","maxPrecip","pickup_longitude","pickup_latitude","dropoff_longitude", "dropoff_latitude")
data[,num_cols] = sapply(data[,num_cols], as.numeric)
fact_cols = c("month","weekDay", "pickupHour","dropoffHour","pickupBcode","pickupZcta","dropoffBcode","dropoffZcta")
data[,fact_cols] = lapply(data[,fact_cols], factor)


##Outliers
resultat <- boxplot(data[,"trip_duration"],range = 10, outline = FALSE,plot=FALSE)
valeurs_aberrantes <- resultat$out
data <- data[data$trip_duration<min(valeurs_aberrantes),]


### Main features
features  <- c("month","weekDay", "pickupHour","pickupNeighborhood","dropoffNeighborhood","distance","maxTemp","maxWind","maxPrecip")
#"dropoffZcta"
#"pickupZcta"

##Handle NA
#plot summary
library(VIM)
analysis_na<-summary(aggr(data,sortVar=TRUE))$combinations

#Remove lines if NA in one of the columns we're interedted in
lines_to_keep <- complete.cases(data[,c(features,"trip_duration")])
data <- data[lines_to_keep,]

##Formula
f <- as.formula(
  paste("trip_duration", 
  paste(features, collapse = " + "), 
  sep = " ~ ")
  )

linear <- lm(f,data=data)

tr <-trainControl(method="cv",number=10) 
cv.lin<- train(f, method="lm" ,data=data, trControl = tr)


##package randomForest vraiment trèèèès lent
rf.ranger <-ranger(data=data[,c("weekDay","distance", "month", "pickupHour","trip_duration")], dependent.variable.name="trip_duration", classification =
            F, max.depth=5, min.node.size=100)
tr2 <-trainControl(method="cv",number=3) 
cv.rf<- train(f, method="ranger" ,data=data, trControl = tr2,metric="MAE")