
#### Setup #### 

library(shiny)
library(plotly)
library(bslib)
library(shinythemes)

#### End #### 

shinyUI(fluidPage(
  
                  #### Theme #### 
                  
                  theme = bs_theme(
                    version = 5,
                    base_font = "'Verlag', 'Helvetica Neue', Helvetica, Arial, sans-serif", 
                    heading_font = "'Verlag', 'Helvetica Neue', Helvetica, Arial, sans-serif", 
                    code_font = "'Verlag', 'Helvetica Neue', Helvetica, Arial, sans-serif"
                  ),
                  
                  #### End #### 
                  
                  #### Ping to prevent disconnects ####
                  
                  tags$script(HTML("
                    setInterval(function() {
                      Shiny.onInputChange('keep_alive', new Date());
                    }, 15000);  // every 15 seconds
                  ")),
                  
                  #### End #### 
                  
                  #### Error message #### 
                  
                  tags$head(tags$style(".shiny-output-error{color: grey;}")),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: visible; content: 'Please wait for the figure to load or make another selection.'; }"
                  ),
                  
                  #### End #### 
                  
                  #### Title panel ####
                  
                  titlePanel("TCF Financial Aid Distributions Tool"), 
                  
                  helpText("This interactive tool displays the results from more than 3,600 tables that the authors generated from NCES Datalab to examine how money for college is distributed across student groups. The figures reflect undergraduates enrolled in 2019-20, collected by NCES for the National Postsecondary Student Aid Study-Administrative Collection (NPSAS-AC)."), 
                  br(),
                  helpText("To use this tool, first select whether you would like the tool to display figures for the nation overall (National View), for state public higher education 2- and 4-year sectors (State View), or for contextual information about the grouping variables (Context View). Then, select a numeric target variable, a grouping variable, and a sector. In 'State View,' only states with representative samples for the selected sector are included."),
                  br(),
                  helpText("We used two filters that applied to all data pulls from NCES Datalab: full-time enrollment and federal financial aid application status. We focus on students enrolled full-time so that differences in financial aid awards would not be attributed to enrollment intensity. We focus on those who applied for federal financial aid so that differences in financial aid receipt would not be attributed to application behavior."),
                  br(),
                  helpText("The tool may take a second to load after entering a new selection."),
                  
                  #### End #### 
                  
                  br(), 
                  br(),
                  
                  fluidRow(
                    column(
                      12,
                      
                      #### Inputs #### 
                      
                      selectInput(inputId="view", 
                                  label="Select a view:", 
                                  choices=c(
                                    "National View", 
                                    "State View", 
                                    "Context View"
                                  ), 
                                  selected="National View"
                      ),
                      uiOutput("selectionT"), # Target
                      uiOutput("selectionR"), # Row
                      uiOutput("selectionS"), # Sector
                      uiOutput("selectionI")  # Sector
                      
                      #### End #### 
                      
                    ), 
                    style = "background-color: #f3f3f3ff; padding: 10px; border-radius: 5px;"
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(
                      12,
                      
                      #### Output figure #### 
                      
                      h3(textOutput("titleFig")),
                      plotlyOutput("displayFig", width = "675px", height = "auto"),
                      br(),
                      
                      #### End #### 
                      
                      #### Output text ####
                      
                      tags$div(
                        style = "border: 2px dashed #acacab; background-color: #B5E2FA; padding: 10px; display: inline-block; width: 675px;",
                        textOutput("summary")
                      )
                      ,
                      br(), 
                      br()
                      
                      #### End #### 
                           
                    )
                  )
))
