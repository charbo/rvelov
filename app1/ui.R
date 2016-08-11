library("shiny")
library("leaflet");

# Define UI for slider demo application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("Sliders"),
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      # Simple integer interval
      sliderInput("integer", "Integer:", 
                  min=0, max=45, value=15, step=15)
      
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      leafletOutput("mymap", width=1000, height=800),
      tableOutput("values")
    )
  )
))

