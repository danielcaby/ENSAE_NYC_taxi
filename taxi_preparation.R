###############################################################################################################################
# Packages
###############################################################################################################################library(dplyr)
library(readr)
library(jsonlite)
library(lubridate)
library(sf)
library(geojsonsf)
library(tidyr)
library(dplyr)
library(fuzzyjoin)


###############################################################################################################################
# chargement des données
################################################################################################################################ 
# Taxi train ==> https://www.kaggle.com/c/nyc-taxi-trip-duration
#taxi <- read.csv2(file='data/train.csv', sep=",", dec=".")
taxi <- read_csv("data/NYC_taxi_2016.csv")

# Shape file arrondissement et zip code ==> https://earthworks.stanford.edu/catalog/nyu_2451_34509
#nycShapeFile <- geojson_sf('data/shape/nyu-2451-34509-geojson.json')
nycShapeFile <- geojson_sf("data/nyu-2451-34509-geojson.json")
# Météo ==> package RIEM mais proxy EDF veut pas alors alors la source direct ==> https://mesonet.agron.iastate.edu/request/download.phtml
# Extraction manuelle des données météo sur les 4 aéroports -JRB, LGA, NYC, JFK- les plus proches de NYC
nycWeather <- read.csv2(file="data/NYC_weather.csv", sep=",", dec=".")
nyc_zipcodeByNbhoods <- read_csv("data/nyc_zipcodeByNbhoods.csv")


# Evenements
# https://data.ny.gov/City-Government/NYC-Permitted-Event-Information-Historical/bkfu-528j
nycEvents <- data.table::fread(file="data/NYC_Permitted_Event_Information_-_Historical.csv",sep=",",header=TRUE)
# --> https://data.ny.gov/City-Government/NYC-Permitted-Event-Information-Historical/bkfu-528j

# Motor crashes
nycCrashes <- data.table::fread(file="data/Motor_Vehicle_Collisions_-_Crashes.csv",sep=",",header=TRUE)
# --> https://data.ny.gov/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95

# Traffic 
# nycTrafficVolume <- data.table::fread(file="data/Annual_Average_Daily_Traffic__AADT___Beginning_1977.csv",sep=",",header=TRUE)
# --> https://data.ny.gov/Transportation/Annual-Average-Daily-Traffic-AADT-Beginning-1977/6amx-2pbv
# --> KO : On a qu'une somme annuelle

# nycTrafficVolume <- data.table::fread(file="data/Traffic_Volume_Counts__2014-2018_.csv",sep=",",header=TRUE)
# --> KO : On a que 34 jours de données sur S1 2016...Cancellation
# --> + rattachement au segment ID ?

nycBusDelay <- data.table::fread(file="data/Bus_Breakdown_and_Delays.csv",sep=",",header=TRUE)
# --> https://data.ny.gov/Transportation/Bus-Breakdown-and-Delays/ez4e-fazm



# TAXI############################################################################################################################### 
# Parsing date et split jusqu'à la granularité horaire
taxi <- mutate(taxi, 
               pickup_datetime = ymd_hms(pickup_datetime),
               dropoff_datetime = ymd_hms(dropoff_datetime),
               year = year(pickup_datetime),
               month = month(pickup_datetime),
               day = day(pickup_datetime),
               weekDay = wday(pickup_datetime),
               pickupHour = hour(pickup_datetime),
               dropoffHour = hour(dropoff_datetime))
########
# TAXI + zipcode############################################################################################################################### 
# Création objet geometry pickup dans jeu de données
taxi <- st_as_sf(taxi,
                 coords = c("pickup_longitude","pickup_latitude"),
                 agr = "constant",
                 dim = "XY",
                 crs=4326, # = WGS84
                 stringsAsFactors = F,
                 remove = F)
# Inner join sur la base des zipcode ==> Hypothèse 1 : on exclut les trajets dont départ hors NYC
taxi <- st_join(taxi, nycShapeFile, join = st_within,left=F) 
# Renommage
taxi <- taxi %>% 
  mutate (pickupBcode = bcode) %>% 
  mutate (pickupZcta = zcta) %>% 
  select(-bcode) %>%
  select(-zcta)

# A priori, on ne peut avoir qu'un objet Sf:geometry dans un dataframe
# Suppression de l'objet geometry
st_geometry(taxi) <- NULL
# Création objet geometry dropoff dans jeu de données
taxi <- st_as_sf(taxi,
                 coords = c("dropoff_longitude","dropoff_latitude"),
                 agr = "constant",
                 dim = "XY",
                 crs=4326, # = WGS84
                 stringsAsFactors = F,
                 remove = F)
# Inner join sur la base des zipcode ==> Hypothèse 2 : on exclut les trajets dont arrivée hors NYC
taxi <- st_join(taxi, nycShapeFile, join = st_within,left=F)
# Renommage
taxi <- taxi %>% 
  mutate (dropoffBcode = bcode) %>% 
  mutate (dropoffZcta = zcta) %>% 
  select(-bcode) %>%
  select(-zcta)

features_to_remove <- c("id","bbox.x","note.x","geometry","bbox.y","note.y")
taxi <- taxi %>% select(-features_to_remove)

# Pour faire des tests rapides ==> https://www.coordonnees-gps.fr/

# left join sur la base des zipcode pour récupérer le libellé des borough et neighborhood de pickup
# https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm

taxi <- taxi %>% mutate(pickupZcta = as.numeric(pickupZcta))
taxi <- left_join(taxi, nyc_zipcodeByNbhoods, by=c("pickupZcta" = "zipCode"))
taxi <- taxi %>% rename(pickupBorough = borough)
taxi <- taxi %>% rename(pickupNeighborhood = neighborhood)

# left join sur la base des zipcode pour récupérer le libellé des borough et neighborhood de dropoff
taxi <- taxi %>% mutate(dropoffZcta = as.numeric(dropoffZcta))
taxi <- left_join(taxi, nyc_zipcodeByNbhoods, by=c("dropoffZcta" = "zipCode"))
taxi <- taxi %>% rename(dropoffBorough = borough)
taxi <- taxi %>% rename(dropoffNeighborhood = neighborhood)

########
# TAXI + zipcode + distance####
# calcul et rajout distance
source("distance_computation.R")
taxi <- taxi %>% 
  mutate (distance = getDistanceFromLatLonInKm(dropoff_latitude,dropoff_longitude,pickup_latitude,pickup_longitude))
# Pour faire des tests rapides ==> https://www.coordonnees-gps.fr/

########
# TAXI + zipcode + distance + Météo####
# Parsing date 
nycWeather <- mutate(nycWeather, 
                     validDate = ymd_hm(valid),
                     wYear = year(validDate),
                     wMonth = month(validDate),
                     wDay = day(validDate),
                     wWeekDay = wday(validDate),
                     # On associe le timestamp de l'observation météo pour la prochaine jointure avec taxi
                     wHour = hour(round_date(validDate, unit = 'hour')))

# On ne retient que la température, vent et précipitations mais il existe d'autres variables potentiellement explicatives
# https://www.weather.gov/media/asos/aum-toc.pdf
aggWeather <- nycWeather %>% group_by(wYear, wMonth, wDay, wHour) %>%
  summarise(
    # on prend la valeur max observée (sur les 4 aéroports considérés) pour chacun des points horaires 
    maxTemp = max(tmpc),
    maxWind = max(sped),
    maxPrecip = max(p01m))

# ajout des données météo par jointure sur l'heure de départ
taxi <- left_join(taxi, aggWeather, by=c("year"="wYear","month"="wMonth","day"="wDay","pickupHour"="wHour"))


# taxi <- data.table::fread(file="data/taxiTrainDataSetOK.csv",sep=";",header=TRUE)

# -----------------Evènements-DEB----------------------------
# On supprime les caracteres speciaux dans le nommage des colonnes
names(nycEvents) <- gsub("[[:punct:]]","",make.names(names(nycEvents)))

# on filtre sur les évènements dans la période qui nous intéresse
nycEvents$StartDateTime<- as.POSIXct(strptime(nycEvents$StartDateTime, "%m/%d/%Y %I:%M:%S %p"))
nycEvents$EndDateTime<- as.POSIXct(strptime(nycEvents$EndDateTime, "%m/%d/%Y %I:%M:%S %p"))
period <- interval(date('2016-01-01'),date('2016-06-30'))
filtered2k16nycEvents<-nycEvents %>% filter(StartDateTime %within% period | EndDateTime %within% period)

# Idée en plus / à tester si on a le temps dans les étapes suivantes : On pourrait filtrer sur le StreetClosureType != NA ?

# On compte les évènements qui débutent / jour
aggfiltered2k16nycEvents <- filtered2k16nycEvents %>% group_by(jour = date(StartDateTime), EventBorough) %>%
  summarise(
    nbEvents = n_distinct(EventID)) 


# Juste pour assurer la jointure / fucking dates
aggfiltered2k16nycEvents <- mutate(aggfiltered2k16nycEvents, 
                                   year = year(jour),
                                   month = month(jour),
                                   day = day(jour))

# on fait une jointure pour ajouter le nombre d'évènement qui débute par jour et par borough 
taxi <- left_join(taxi, aggfiltered2k16nycEvents, by=c("pickupBorough"="EventBorough","year"="year","month"="month","day"="day"))
taxi <- taxi %>% mutate(pickupNbEvents = nbEvents)
taxi <- taxi %>% select(-jour) %>% select(-nbEvents)
taxi <- taxi %>% mutate(pickupNbEvents = replace_na(pickupNbEvents, 0))
taxi <- left_join(taxi, aggfiltered2k16nycEvents, by=c("dropoffBorough"="EventBorough","year"="year","month"="month","day"="day"))
taxi <- taxi %>% mutate(dropoffNbEvents = nbEvents)
taxi <- taxi %>% select(-jour) %>% select(-nbEvents)
taxi <- taxi %>% mutate(dropoffNbEvents = replace_na(dropoffNbEvents, 0))

# Attention : on rate environ 10 % = ceux qui se tiennent sur plusieurs jours... on pourrait essayer de les comptabiliser avec un peu plus de temps :
# blankEventCalendar <- data.frame(date = seq.Date(from =as.Date("2016-01-01 00:00", "%Y-%m-%d %H:%M"), to=as.Date("2016-06-30 00:00", "%Y-%m-%d %H:%M"), by="day"), borough = "Manhattan", nbEvents = 0) %>% 
#   bind_rows(data.frame(date = seq.Date(from =as.Date("2016-01-01 00:00", "%Y-%m-%d %H:%M"), to=as.Date("2016-06-30 00:00", "%Y-%m-%d %H:%M"), by="day"), borough = "Brooklyn", nbEvents = 0)) %>% 
#   bind_rows(data.frame(date = seq.Date(from =as.Date("2016-01-01 00:00", "%Y-%m-%d %H:%M"), to=as.Date("2016-06-30 00:00", "%Y-%m-%d %H:%M"), by="day"), borough = "Queens", nbEvents = 0)) %>% 
#   bind_rows(data.frame(date = seq.Date(from =as.Date("2016-01-01 00:00", "%Y-%m-%d %H:%M"), to=as.Date("2016-06-30 00:00", "%Y-%m-%d %H:%M"), by="day"), borough = "Bronx", nbEvents = 0)) %>% 
#   bind_rows(data.frame(date = seq.Date(from =as.Date("2016-01-01 00:00", "%Y-%m-%d %H:%M"), to=as.Date("2016-06-30 00:00", "%Y-%m-%d %H:%M"), by="day"), borough = "Staten Island", nbEvents = 0))
# 
# nycEventCalendar <- fuzzy_left_join(
#   blankEventCalendar, filtered2k16nycEvents,
#   by = c(
#     "borough" = "EventBorough",
#     "date" = "StartDateTime",
#     "date" = "EndDateTime"
#   ),
#   match_fun = list(`==`, `>=`, `<=`)
# )
# -----------------Evènements-FIN----------------------------

# -----------------Crashes - Deb----------------------------
# On supprime les caracteres speciaux dans le nommage des colonnes
names(nycCrashes) <- gsub("[[:punct:]]","",make.names(names(nycCrashes)))

# on filtre sur les évènements dans la période qui nous intéresse
nycCrashes$dateCrash <- as.POSIXct(strptime(paste(nycCrashes$CRASHDATE,nycCrashes$CRASHTIME, sep=" "), "%m/%d/%Y %H:%M"))
period <- interval(date('2016-01-01'),date('2016-06-30'))
filtered2k16nycCrashes<-nycCrashes %>% filter(dateCrash %within% period)

filtered2k16nycCrashes <- filtered2k16nycCrashes %>% filter(!is.na(ZIPCODE))

# Juste pour assurer la jointure / fucking dates
filtered2k16nycCrashes <- mutate(filtered2k16nycCrashes, 
                                   year = year(dateCrash),
                                   month = month(dateCrash),
                                   day = day(dateCrash),
                                   hour = hour(dateCrash))

# On compte les accidents par heure et par zipcode
aggfiltered2k16nycCrashesbyHour <- filtered2k16nycCrashes %>% group_by(year, month, day, hour, ZIPCODE) %>%
  summarise(
    nbCrashesbyHour = n_distinct(COLLISIONID)) 

View(aggfiltered2k16nycCrashesbyHour)

# on fait une jointure pour ajouter le nombre de crash par heure et zipcode de pickup et dropoff
taxi <- left_join(taxi, aggfiltered2k16nycCrashesbyHour, by=c("pickupZcta"="ZIPCODE","year"="year","month"="month","day"="day", "pickupHour"="hour"))
taxi <- taxi %>% mutate(pickupNbCrashesbyHour = nbCrashesbyHour)
taxi <- taxi %>% select(-nbCrashesbyHour)
taxi <- taxi %>% mutate(pickupNbCrashesbyHour = replace_na(pickupNbCrashesbyHour, 0))
taxi <- left_join(taxi, aggfiltered2k16nycCrashesbyHour, by=c("dropoffZcta"="ZIPCODE","year"="year","month"="month","day"="day", "dropoffHour"="hour"))
taxi <- taxi %>% mutate(dropoffNbCrashesbyHour = nbCrashesbyHour)
taxi <- taxi %>% select(-nbCrashesbyHour)
taxi <- taxi %>% mutate(dropoffNbCrashesbyHour = replace_na(dropoffNbCrashesbyHour, 0))

# on fait une jointure pour ajouter le nombre d'évènement qui débute par jour et par borough 
# Idée en plus / à tester si on a le temps dans les étapes suivantes : on pourrait pondérer en fonction de la gravité : mortalité / nombre de véhicules impliqués ?
# Idée en plus / à tester si on a le temps dans les étapes suivantes : on pourrait aussi utiliser les coordonnées géographiques de l'accident plus précisément ?
# -----------------Crashes-FIN----------------------------

# -----------------Get Bus Delay information - Deb----------------------------
# On supprime les caracteres speciaux dans le nommage des colonnes
names(nycBusDelay) <- gsub("[[:punct:]]","",make.names(names(nycBusDelay)))

# on filtre sur la raison 'Heavy traffic'
filteredBusDelay <- nycBusDelay %>% filter(Reason == 'Heavy Traffic')

# on filtre sur les évènements dans la période qui nous intéresse
filteredBusDelay$dateDelay <- as.POSIXct(strptime(filteredBusDelay$OccurredOn, "%m/%d/%Y %I:%M:%S %p"))

period <- interval(date('2016-01-01'),date('2016-06-30'))
filtered2k16BusDelay <-filteredBusDelay %>% filter(dateDelay %within% period)

filtered2k16BusDelay <- filtered2k16BusDelay %>% filter(length(Boro) > 0)

filtered2k16BusDelay <- filtered2k16BusDelay %>% filter(Boro == "Manhattan" |
                                          Boro == "Queens" |
                                        Boro == "Brooklyn" |
                                        Boro == "Bronx" |
                                        Boro == "Staten Island")
# Juste pour assurer la jointure / fucking dates
filtered2k16BusDelay <- mutate(filtered2k16BusDelay, 
                                 year = year(dateDelay),
                                 month = month(dateDelay),
                                 day = day(dateDelay),
                                 hour = hour(dateDelay))

# On compte les accidents par heure et par zipcode
aggFiltered2k16BusDelay <- filtered2k16BusDelay %>% group_by(year, month, day, hour, Boro) %>%
  summarise(
    nbBusDelayedHeavyTrafficbyHour = n_distinct(BusbreakdownID)) 

View(aggFiltered2k16BusDelay)
# On ajoute le nb d'accidents par heure et par borough de départ + indicateur d'heavy traffic
taxi <- left_join(taxi, aggFiltered2k16BusDelay, by=c("year"="year","month"="month","day"="day","pickupHour"="hour", "pickupBorough"="Boro"))
taxi <- taxi %>% mutate(pickupNbBusDelayedHeavyTrafficbyHour = nbBusDelayedHeavyTrafficbyHour)
taxi <- taxi %>% select(-nbBusDelayedHeavyTrafficbyHour)
taxi <- taxi %>% mutate(pickupNbBusDelayedHeavyTrafficbyHour = replace_na(pickupNbBusDelayedHeavyTrafficbyHour, 0))

# On ajoute le nb d'accidents par heure et par borough d'arrivée + indicateur d'heavy traffic
taxi <- left_join(taxi, aggFiltered2k16BusDelay, by=c("year"="year","month"="month","day"="day","dropoffHour"="hour","dropoffBorough"="Boro"))
taxi <- taxi %>% mutate(dropoffNbBusDelayedHeavyTrafficbyHour = nbBusDelayedHeavyTrafficbyHour)
taxi <- taxi %>% select(-nbBusDelayedHeavyTrafficbyHour)
taxi <- taxi %>% mutate(dropoffNbBusDelayedHeavyTrafficbyHour = replace_na(dropoffNbBusDelayedHeavyTrafficbyHour, 0))

# A voir comment on aggrège les données de crashes, bus delay by heavy traffic...
# -----------------Get Bus Delay information - Deb----------------------------

# Sauvegarde du jeu de données
# write.table(taxi,file="data/taxiTrainDataSet.csv",sep=";", row.names=FALSE)
saveRDS(taxi, file = 'data/nycTaxiTrainDataSet.rds')
