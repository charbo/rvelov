

library(shiny)

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
                  min=0, max=15, value=5, step=5)
      
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      tableOutput("values")
    )
  )
))

