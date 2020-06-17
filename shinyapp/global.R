# # # # global.R pour l'application
# # # 
# 

#On installe tous les package qui manquent si besoin
#on vérifie que tous les packages sont bien installés
list.of.packages <- c("shiny", "dplyr","leaflet","plotly",
                      "colourpicker","rAmCharts","shinythemes","sf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages,dependencies = T)

# Liste des packages utiles
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(colourpicker)
library(rAmCharts)
library(shinythemes)
library(sf)



# Load RData
load(file='taxi.RData')
load(file='data_sampled3.rdata')
tds <- data_sampled

# Création des données pour les cartes
#Carte 1, on ajoute au nycShapeFile les nb trajets 
data_map1 <- nycShapeFile %>%
  left_join(.,
            tds %>%
              group_by(pickupZcta)%>%
              summarise(nb_trips = NROW(vendor_id)),
            by=c("zcta"="pickupZcta"))

# Création des structures pour les listes du frontal Rshiny
yearSelectListChoices <- taxiDf %>%
  distinct(pickupYear) %>%
  arrange(pickupYear)

# DEB BUG https://stackoverflow.com/questions/43098849/display-only-one-value-in-selectinput-in-r-shiny-app/54525603#54525603 ==> le nom de la liste est bien crade 
names(yearSelectListChoices) <- 2016
# FIN BUG

monthSelectListChoices <- taxiDf %>%
  distinct(pickupMonth) %>%
  arrange(pickupMonth)

daySelectListChoices <- taxiDf %>%
  distinct(pickupDay) %>%
  arrange(pickupDay)

hourSelectListChoices <- taxiDf %>%
  distinct(pickupHour) %>%
  arrange(pickupHour)
