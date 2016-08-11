library(shiny); 
library(rCharts); 
library(leaflet)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = p("Afficher la disponibilité"),
                   choices = list("Des places" = 1, "Des vélos" = 2), 
                   selected = 1)
    ),
    mainPanel(
      leafletOutput("mymap", width=1000, height=800)
    )

  )
)
)