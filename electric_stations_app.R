# Description -------------------------------------------------------------
## Title: Geolocation and Status of electric car charging stations in Switzerland
## Author: Ismail Muller (muller.ismail@gmail.com)
## Data Source: Swiss administraton
## Reference: https://map.geo.admin.ch/?lang=en&topic=energie&bgLayer=ch.swisstopo.pixelkarte-grau&zoom=1&layers=ch.bfe.ladestellen-elektromobilitaet&E=2598013.90&N=1198041.62&catalogNodes=2419,2420,2427,2480,2429,2431,2434,2436,2767,2441,3206

# Packages ----------------------------------------------------------------
library(jsonlite) # read JSON 
library(tidyverse) # meta-package for data-wrangling
library(fontawesome) # Package to add icons in the map-popup # devtools::install_github("rstudio/fontawesome")
library(leaflet) # package for mapping
library(shiny) # for making an app
library(shinythemes) # nice themes for shiny
library(flexdashboard) 

# Data import -------------------------------------------------------------
stations_url <- "https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/data/oicp/ch.bfe.ladestellen-elektromobilitaet.json"
status_url <- "https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/status/oicp/ch.bfe.ladestellen-elektromobilitaet.json"

stations_json <- jsonlite::read_json(stations_url, flatten = TRUE)
status_json <- jsonlite::read_json(status_url, flatten = TRUE)


# Utility functions -------------------------------------------------------
adresse_format <- function(named_list){
  named_list$HouseNum <- str_replace_all(named_list$HouseNum, "^(-|\\.|nan)$", "") %>% replace_na("")
  paste(named_list$Street, named_list$HouseNum, ",", named_list$PostalCode, named_list$City, sep=" " )
}

map_chr_paste_collapse_unique <- function(x){
  # function that collapses all elements of x into a string.
  ## Useful to "unlist" lists and flatten vectors
  paste_collapse_unique <- function(x){
    if(is.list(x)){
      x <- unlist(x)
    } else { x }
    x <- c( na.omit(x) )
    x <- x[x != "NA"]
    paste0( unique( x ), collapse = ", " )
  }
  
  map_chr(x, paste_collapse_unique)
}

# Data wrangling ----------------------------------------------------------
## status
status <- map_df(status_json$EVSEStatuses, as_tibble) %>% 
  transmute(EvseID = map_chr(EVSEStatusRecord, "EvseID"),
            EVSEStatus = map_chr(EVSEStatusRecord, "EVSEStatus"))

## locations
stations <- map_df(stations_json$EVSEData, as_tibble) %>% 
  unnest_wider(EVSEDataRecord) %>% 
  left_join(status, by = "EvseID") %>% 
  mutate(GeoCoordinates = unlist(GeoCoordinates) ) %>% 
  tidyr::separate(GeoCoordinates, into = c("longitude", "latitude"), 
                  sep = ",", convert = TRUE, remove = FALSE ) %>% 
  mutate(Address_unlisted = map_chr(Address, adresse_format),
         Address_unlisted = str_replace_all(Address_unlisted, " , ", ", ") %>% str_trim(),
         IsOpen24Hours = factor(IsOpen24Hours, levels = c(TRUE, FALSE), labels = c("Is open 24 Hours", "Is not open 24 Hours"))
         ) %>% 
  mutate_at(vars(AuthenticationModes, PaymentOptions, ValueAddedServices, Plugs, GeoChargingPointEntrance), 
            .funs = map_chr_paste_collapse_unique) %>% 
  mutate(LABEL = str_glue("{fa('info')}  {EVSEStatus}
      <br/>{fa('user')} Network: {OperatorName}
      <br/>{fa('location-arrow')} {Address_unlisted}
      <br/>{fa('unlock')} {AuthenticationModes}
      <br/>{fa('credit-card')} Payment: {PaymentOptions}
      <br/>$ Price: Please contact the operator
      {ifelse(Accessibility!='Unspecified', str_glue('<br/>Access: {Accessibility}'), '')}
      {ifelse(ValueAddedServices!='None', str_glue(\"<br/> {fa('plus')} {ValueAddedServices}\"), '')}
      <br/>{fa('map')} {GeoCoordinates}
      <br/>{fa('phone')} {HotlinePhoneNum}
      <br/>{fa('clock')} {IsOpen24Hours}
      <br/>{fa('plug')} {Plugs}
                          ")) %>% 
  dplyr::filter( latitude > 45 )

# App - UI ----------------------------------------------------------------
ui <- navbarPage(
  title = "Electric car charging stations",
  id = "interactive_map_panel",
  theme = shinythemes::shinytheme("flatly"),
  
  # first tab with interactive map
  tabPanel(
    title = "Interactive Map",
    
    h4("Interactive map showing charging stations for electric cars in Switzerland"),
    
    # ... with a map
    leaflet::leafletOutput("leaf_map", width = "100%"),
    
    # ... panel for choosing controls
    absolutePanel(id = "controls", 
                  # class = "panel panel-default",
                  fixed = TRUE, draggable = TRUE,
                  top = 140, bottom = "auto", 
                  right = 20, left = "auto",
                  width = "auto", height = "auto",
                  
                  gaugeOutput("availability_gauge")
    )
  ), # end of firt tab paenl
  
  # second tab with the data
  tabPanel(
    title = "Data Explorer",
    DT::DTOutput("data")
  ), # end of second tab panel
  
  # ohter text,
  helpText(tagList(
    "The data is actualised when the app is launched.",
    "Unofficial App with official data from", a("admin.ch", href = stations_url),
    "You can take a look at the official app here", a("http://www.ich-tanke-strom.ch/", href = "http://www.ich-tanke-strom.ch/"),
    "For more swiss open data, visit", a("opendata.swiss", href = "https://opendata.swiss/en/")
  )),
  
  helpText(tagList(
    "Contact the author per",
    a("mail", href = "mailto:muller.ismail@gmail.com"),
    "or",
    a("linkedin", href = "https://www.linkedin.com/in/ismail-muller/")
  ))
  
  
) # end of UI

# App - Server ------------------------------------------------------------
my_icons <- awesomeIconList(
  "Unknown" = makeAwesomeIcon(icon = "fa-plug", library = "fa", markerColor = "grey" ),
  "Available" = makeAwesomeIcon(icon = "fa-plug", library = "fa", markerColor = "green" ),
  "Occupied" = makeAwesomeIcon(icon = "fa-plug", library = "fa", markerColor = "red" ),
  "OutOfService" = makeAwesomeIcon(icon = "fa-times", library = "fa", markerColor = "red" )
)

server <- function(input, output, session){
  
  # availability gauge
  output$availability_gauge <- renderGauge({
    bounds <- input$leaf_map_bounds
    rate <- stations %>% 
      filter( latitude <= bounds$north, latitude >= bounds$south, 
              longitude <= bounds$east, longitude >= bounds$west  ) %>% 
      summarize(n_stations = n_distinct(EvseID), 
                n_available = n_distinct(EvseID[EVSEStatus == "Available"])) %>% 
      mutate(rate = n_available / n_stations * 100) %>% 
      select(rate) %>% 
      as.numeric()
    
    gauge(value = round(rate), min = 0, max = 100, 
          symbol = "%", label = "Availability in region",
          sectors = gaugeSectors(success=c(75,100), danger = c(0,25), warning = c(26,74)) )
    })
  
  
  # interactive map
  output$leaf_map <- renderLeaflet({
    stations %>% 
      leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude,
                        popup = ~ LABEL,
                        icon = ~ my_icons[EVSEStatus],
                        clusterOptions = markerClusterOptions())
  })
  
  # data
  output$data <- DT::renderDT({
    stations %>% 
      select_if( function(x)!is.list(x) ) %>% 
      select(-longitude, -latitude, -LABEL)
  })
}

# App - runApp ------------------------------------------------------------
shinyApp(ui, server)
