library(readr)
library(lubridate)
library(tidyr)
library(dplyr)
library(caret)
library(randomForest)
library(ranger)
library(data.table)


## GET prepared dataset ###
data <- data.table::fread(file="taxiTrainDataSet.csv",sep=";",header=TRUE)
features_to_remove <- c("id","bbox.x","note.x","geometry","bbox.y","note.y")
#data <-data[,!(names(data) %in% features_to_remove)]
data <- data %>% select(-features_to_remove)

##Transform data type
num_cols<-c("distance","maxTemp","maxWind","maxPrecip","pickup_longitude","pickup_latitude","dropoff_longitude", "dropoff_latitude")
#data[,num_cols] = sapply(data[,num_cols], as.numeric)
data <- data %>% mutate_at(.vars = vars(num_cols),.funs = as.numeric)
fact_cols = c("month","weekDay", "pickupHour","dropoffHour","pickupBcode","pickupZcta","dropoffBcode","dropoffZcta")
#data[,fact_cols] = lapply(data[,fact_cols], factor)
data <- data %>% mutate_at(.vars = vars(fact_cols),.funs = as.factor)


##Outliers
resultat <- boxplot(data[,"trip_duration"],range = 10, outline = FALSE,plot=FALSE)
valeurs_aberrantes <- resultat$out
data <- data[data$trip_duration<min(valeurs_aberrantes),]


### Main features
features  <- c("month","weekDay", "pickupHour","pickupBcode","dropoffBcode","distance","maxTemp","maxWind","maxPrecip")
#"dropoffZcta"
#"pickupZcta"

##Handle NA
#plot summary
library(VIM)
analysis_na<-summary(aggr(data,sortVar=TRUE))$combinations

#Remove lines if NA in one of the columns we're interedted in
lines_to_keep <- complete.cases(data[,c(features,"trip_duration")])
data <- data[lines_to_keep,]


##### Echantillonnage
ratio <- 0.1
nb_lines <- floor(nrow(data)*ratio)
data_sampled <- data[sample(nrow(data))[1:nb_lines],]


save(data_sampled,file = 'data/data_sampled.rdata')