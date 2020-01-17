header <- dashboardHeader(title = "Charging stations", disable = TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "map", icon = icon("globe")),
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Get Code",  icon = icon("github"), 
             href = "https://github.com/ismailmuller/electric-car-stations-shinyapp"),
    menuItem("Email", icon = icon("envelope"),
             href = "mailto:muller.ismail@gmail.com"),
    textInput("adress", "Adresse", value = "Jet d'eau, Genève"),
    actionButton("go_button", label = "GO!"),
    gaugeOutput("availability_gauge")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
            leaflet::leafletOutput("leaf_map", width = "100%")),
    tabItem(tabName = "data",
            DT::DTOutput("data"))
  )
)

dashboardPage(
  header,
  sidebar,
  body
)
