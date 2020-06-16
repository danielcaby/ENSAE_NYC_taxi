library(readr)
library(lubridate)
library(tidyr)
library(dplyr)
library(caret)
library(randomForest)
library(ranger)
library(data.table)
library(VIM)

## GET prepared dataset ###
# data <- data.table::fread(file="taxiTrainDataSet.csv",sep=";",header=TRUE)
taxi <- read_rds("data/nycTaxiTrainDataSet.rds")

##Transform data type
num_cols<-c("distance", "pickup_longitude","pickup_latitude","dropoff_longitude", "dropoff_latitude", "pickupNbEvents", "dropoffNbEvents", "pickupNbCrashesbyHour", "dropoffNbCrashesbyHour", "pickupNbBusDelayedHeavyTrafficbyHour", "dropoffNbBusDelayedHeavyTrafficbyHour", "maxTemp", "maxWind", "maxPrecip")
#data[,num_cols] = sapply(data[,num_cols], as.numeric)
taxi <- taxi %>% mutate_at(.vars = vars(num_cols),.funs = as.numeric)
fact_cols = c("month","weekDay", "pickupHour","dropoffHour","pickupBcode", "pickupBorough", "pickupNeighborhood", "pickupZcta","dropoffBcode","dropoffBorough","dropoffNeighborhood","dropoffZcta")
#data[,fact_cols] = lapply(data[,fact_cols], factor)
taxi <- taxi %>% mutate_at(.vars = vars(fact_cols),.funs = as.factor)


##Outliers
resultat <- boxplot(taxi[,"trip_duration"],range = 10, outline = FALSE,plot=FALSE)
valeurs_aberrantes <- resultat$out
taxi <- taxi[taxi$trip_duration<min(valeurs_aberrantes),]


# Rajout de la variable isRain de type boolean = Est ce qu'il est tombé de la pluie à l'heure considérée ?
# taxi <- mutate(taxi, maxPrecip = as.numeric(maxPrecip))
# taxi <- mutate(taxi,
#              isRain = ifelse(maxPrecip > 0, TRUE, FALSE))

# Rajout de la variable isHeavyTraffic
#  
### Main features
features  <- c("month","weekDay", "pickupHour","pickupNeighborhood","dropoffNeighborhood","distance")


##Handle NA
#plot summary
analysis_na<-summary(aggr(taxi,sortVar=TRUE))$combinations

#Remove lines if NA in one of the columns we're interedted in
# Variables sorted by number of missings: 
#   Variable        Count
# maxPrecip 1.245368e-01
# maxWind 7.322668e-02
# maxTemp 4.972556e-05
lines_to_keep <- complete.cases(taxi)
taxi <- taxi[lines_to_keep,]

##### Echantillonnage
ratio <- 0.1
nb_lines <- floor(nrow(taxi)*ratio)
data_sampled <- taxi[sample(nrow(taxi))[1:nb_lines],]

##### Sauvegarde
save(data_sampled,file = 'data/data_sampled3.rdata')