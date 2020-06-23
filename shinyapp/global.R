# # # # global.R pour l'application
# # # 
# 

##############################################################################################
#Packages
##############################################################################################

#On installe tous les package qui manquent si besoin
#on vérifie que tous les packages sont bien installés
list.of.packages <- c("shiny", "dplyr","leaflet","plotly",
                      "colourpicker","rAmCharts","shinythemes","sf","DT")
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
library(DT)

##############################################################################################
#chargement des données
##############################################################################################

# Load RData
#load(file='taxi.RData')
#Echantillon données
load(file='data_sampled3.rdata')
#full dataset
tds <- data_sampled
full_taxi <- readRDS("data/nycTaxiTrainDataSet.rds") %>%
  dplyr::mutate(maxTemp = as.numeric(maxTemp),
                maxWind = as.numeric(maxWind),
                maxPrecip = as.numeric(maxPrecip))
#modèle entrainé
rf_taxi <- readRDS("data/taxiRfModel.rds")

##############################################################################################
# Création des données pour l'exploration de data onglet exploration
##############################################################################################

# Création des structures pour les listes du frontal Rshiny
yearSelectListChoices <- full_taxi %>%
  distinct(year) %>%
  arrange(year)

# DEB BUG https://stackoverflow.com/questions/43098849/display-only-one-value-in-selectinput-in-r-shiny-app/54525603#54525603 ==> le nom de la liste est bien crade 
names(yearSelectListChoices) <- 2016
# FIN BUG

monthSelectListChoices <- full_taxi %>%
  distinct(month) %>%
  arrange(month)

daySelectListChoices <- full_taxi %>%
  distinct(day) %>%
  arrange(day)

hourSelectListChoices <- full_taxi %>%
  distinct(pickupHour) %>%
  arrange(pickupHour)

zctaSelectListChoices <- full_taxi %>%
  distinct(pickupZcta) %>%
  arrange(pickupZcta)

##############################################################################################
# Création des données pour les cartes
##############################################################################################

#Carte 1, on ajoute au nycShapeFile les nb trajets 
#option 1 avec echantillon
# data_map1 <- nycShapeFile %>%
#   left_join(.,
#             tds %>%
#               dplyr::group_by(pickupZcta)%>%
#               dplyr::summarise(nb_trips = NROW(vendor_id)),
#             by=c("zcta"="pickupZcta"))

# option 2 full data
data_map1_bis <- nycShapeFile %>%
  left_join(.,
            full_taxi %>%
              dplyr::mutate(pickupZcta = as.factor(pickupZcta)) %>%
              dplyr::group_by(pickupZcta)%>%
              dplyr::summarise(nb_trips = NROW(vendor_id)) %>%
              dplyr::ungroup(),
            by=c("zcta"="pickupZcta"))
