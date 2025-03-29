
#### Setup #### 

library(shiny)
library(shinythemes)

#### End #### 

shinyUI(fluidPage(theme = shinytheme("sandstone"),
  
  #### Title panel ####
  
  titlePanel("Test"), 
  
  #### End #### 
  
  sidebarLayout(
    
    sidebarPanel(
      
      #### Input panel #### 
      
      selectInput(inputId="view", 
                  label="Select a view:", 
                  choices=c("National View", "State View")
      ),
      uiOutput("selectionT"), # Target
      uiOutput("selectionR"), # Row
      uiOutput("selectionS")  # Sector

      #### End #### 
      
    ), 
    
    mainPanel(
      
      #### Output figure #### 
      
      h3(textOutput("titleFig")),
      plotlyOutput("displayFig", width = "100%", height = "100%"),
      br(),
      
      #### End #### 
      
      #### Output text ####
      
      textOutput("summary")
      
      #### End #### 
      
    )
  )
))
