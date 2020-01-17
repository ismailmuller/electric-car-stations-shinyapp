# App - Server ------------------------------------------------------------
function(input, output, session){
  
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
  
  # address
  adress <- eventReactive(input$go_button, {
    tidygeocoder::geo_osm(input$adress)
  }, ignoreNULL = FALSE)
  
  # interactive map
  output$leaf_map <- renderLeaflet({
    basic_map %>% 
      leaflet::addMarkers(lng = adress()$long, lat = adress()$lat, label = "Your adress") %>%
      setView(lng = adress()$long, lat = adress()$lat, zoom = 15)
  })
  
  # data
  output$data <- DT::renderDT({
    stations %>% 
      select_if( function(x)!is.list(x) ) %>% 
      select(-longitude, -latitude, -LABEL)
  })
}
