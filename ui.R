
#### Setup #### 

library(shiny)

#### End #### 

shinyUI(fluidPage(
  
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
      
      plotlyOutput("displayFig"),
      
      #### End #### 
      
      #### Output text ####
      
      textOutput("summary")
      
      #### End #### 
      
    )
  )
))
