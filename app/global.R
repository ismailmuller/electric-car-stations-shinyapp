# Description -------------------------------------------------------------
## Title: Geolocation and Status of electric car charging stations in Switzerland
## Author: Ismail Muller (muller.ismail@gmail.com)
## Data Source: Swiss administraton
## Reference: https://map.geo.admin.ch/?lang=en&topic=energie&bgLayer=ch.swisstopo.pixelkarte-grau&zoom=1&layers=ch.bfe.ladestellen-elektromobilitaet&E=2598013.90&N=1198041.62&catalogNodes=2419,2420,2427,2480,2429,2431,2434,2436,2767,2441,3206

# Packages ----------------------------------------------------------------
# if(!require(pacman)) install.packages("pacman")
# library(pacman)
# 
# pacman::p_load(
#   jsonlite, tidyverse, 
#   leaflet, tidygeocoder,
#   shiny, flexdashboard, shinythemes, fontawesome,
#   shinydashboard
# )

library(jsonlite)
library(tidyverse)
library(tidygeocoder)
library(leaflet)
library(shiny)
library(shinythemes)
library(flexdashboard)
library(fontawesome)
library(shinydashboard)

source("data_etl.R")

# icons
my_icons <- awesomeIconList(
  "Unknown" = makeAwesomeIcon(icon = "fa-plug", library = "fa", markerColor = "grey" ),
  "Available" = makeAwesomeIcon(icon = "fa-plug", library = "fa", markerColor = "green" ),
  "Occupied" = makeAwesomeIcon(icon = "fa-plug", library = "fa", markerColor = "red" ),
  "OutOfService" = makeAwesomeIcon(icon = "fa-times", library = "fa", markerColor = "red" )
)

# basic map
basic_map <- stations %>% 
  leaflet() %>%
  # addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addAwesomeMarkers(lng = ~longitude, lat = ~latitude,
                    popup = ~ LABEL,
                    icon = ~ my_icons[EVSEStatus],
                    clusterOptions = markerClusterOptions()) 



