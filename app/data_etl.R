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
