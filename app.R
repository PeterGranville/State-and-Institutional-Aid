
#### Setup ####

library(DT)
library(shiny)
library(scales)
library(readxl)
library(plotly)
library(tidyverse)
library(shinythemes)

#### End #### 

ui <- fluidPage(theme = shinytheme("sandstone"),

  #### Title panel ####
                
  titlePanel("Financial Aid Distributions"),

  #### End #### 
  
  sidebarLayout(
    
    #### User inputs #### 
    
    sidebarPanel(
        selectInput(inputId = "targetVariable",
                    label = "Select financial aid variable:",
                    choices = c(
                      "---Section 1: Aid distributions---",
                      "Share receiving federal Pell Grant",
                      "Average federal Pell Grant award",
                      "Share receiving federal campus-based aid (SEOG, FWS)",
                      "Average federal campus-based aid (SEOG, FWS) award",
                      "Share receiving Title IV loans (includes Parent PLUS Loans)",
                      "Average Title IV loan (includes Parent PLUS Loans) amount",
                      "Share receiving state grants",
                      "Average state grant award",
                      "Share receiving institutional grants",
                      "Average institutional grant award",
                      "Share receiving private source grants",
                      "Average private source grant award",
                      "---Section 2: Need-based and merit-based aid---",
                      "Share receiving state non-need & merit grants",
                      "Average state non-need & merit grant award",
                      "Share receiving state need-based grants",
                      "Average state need-based grant award",
                      "Share receiving institutional non-need & merit grants",
                      "Average institutional non-need & merit grant award",
                      "Share receiving institutional need-based grants",
                      "Average institutional need-based grant award",
                      "---Section 3: Combined aid measures---",
                      "Share receiving state or institutional grants",
                      "Average combined state and institutional grant award",
                      "Share whose grants exceed federal need",
                      "Average grant amount in excess of federal need",
                      "---Section 4: Net price---",
                      "Share with unmet tuition and fees after all grants",
                      "Median net tuition and fees after all grants",
                      "Average net tuition and fees after all grants",
                      "Median net tuition and fees after all grants as a percentage of income",
                      "Average net student budget after all grants",
                      "Median net student budget after all grants",
                      "Median net student budget after all grants as a percentage of income",
                      "---Section 5: Pricing and need---",
                      "Median tuition and fees paid",
                      "Median student budget (attendance adjusted)",
                      "Median Expected Family Contribution"
                                )
        ), 
        selectInput(inputId = "sectorSelect",
                    label = "Filter for a sector in the 'Analysis' tab:",
                    choices = c(
                      "---No filter selected---", 
                      "Public 2-years only", 
                      "Public 4-years only"
                    )
        ),
        selectInput(inputId = "rowSelect",
                    label = "Filter for a grouping variable:",
                    choices = c(
                      "---No filter selected---", 
                      "Income Quartile", 
                      "Pell Recipient Status", 
                      "Zero EFC Status",
                      "Parents' Highest Education Level", 
                      "Race/Ethnicity", 
                      "Institution HBCU Status", 
                      "Institution MSI Status", 
                      "Institution Selectivity"
                    )
        ),
        submitButton(text = "Enter my selections", icon = NULL, width = NULL)
    ),
    
    #### End #### 
    
    mainPanel(
      tabsetPanel(
        
        #### Analysis tab #### 
        
        tabPanel("Analysis", fluid=TRUE, 
                 br(),
                 h4("Filler text 1"), 
                 br()
        ), 
        
        #### End #### 
        
        #### Public 2-year tab ####
        
        tabPanel("State view: Public 2-years", fluid=TRUE, 
                 br(),
                 h4("State public two-year view goes here"),
                 br()
        ),
        
        #### End #### 
        
        #### Public 4-year tab ####
        
        tabPanel("State view: Public 4-years", fluid=TRUE, 
                 br(),
                 h4("State public four-year view goes here"),
                 br()
        )
        
        #### End #### 
        
      )
    )
  )
)

server <- function(input, output) {

  observe({
    
    #### Load data #### 
    
    ampSector3 <- read.csv("Data/AMP-SECTOR3.csv", header=TRUE)
    ampInststat2Y <- read.csv("Data/AMP-INSTSTAT-2Y.csv", header=TRUE)
    ampInststat4Y <- read.csv("Data/AMP-INSTSTAT-4Y.csv", header=TRUE)
    distSector3 <- read.csv("Data/DIST-SECTOR3.csv", header=TRUE)
    targetLookup <- read_excel("Data/Target-Lookup.xls") %>% select(-(`Section`)) %>% select(-(`Entry`))
    axisBounds <- read.csv("Axis-Bounds.csv", row.names=FALSE)
    
    #### End #### 
    
    #### Set chart dimensions ####
    
    overallWidth <- 800
    heightIntercept <- 300
    heightCoefficient <- 20
    
    #### End #### 
    
    #### Write chartFn1 ####
    
    chartFn1 <- function(targetName, rowName, measureName, subtable, sector3Val){
      
      if(subtable=="SECTOR3"){
        tempDF <- AMP.SECTOR3
      }else{
        if(subtable=="INSTSTAT"){
          if(sector3Val=="Public 4-year"){
            tempDF <- AMP.INSTSTAT.4Y
          }else{
            if(sector3Val=="Public 2-year"){
              tempDF <- AMP.INSTSTAT.2Y
            }else{
              stop("Bad sector3Val input")
            }
          }
        }else{
          stop("Bad subtable input")
        }
      }
      
      tempDF <- tempDF %>% filter(
        `Target name`==targetName, 
        `Row name`==rowName, 
        `Measure name`==measureName
      )
      tempDF <- left_join(x=tempDF, y=axisTitles1, by=c("Target name", "Measure name"))
      tempDF <- left_join(x=tempDF, y=axisBounds, by="Target name")
      
      if(measureName=="Share >0"){
        tempDF <- tempDF %>% mutate(
          `Target value` = `Target value` / 100, 
          `Lower bound` = `Lower bound` / 100, 
          `Upper bound` = `Upper bound` / 100
        ) %>% mutate(
          `Target value` = ifelse(`Target value` < 0, 0, `Target value`), 
          `Lower bound` = ifelse(`Lower bound` < 0, 0, `Lower bound`), 
          `Upper bound` = ifelse(`Upper bound` < 0, 0, `Upper bound`)
        ) %>% mutate(
          `Target value` = ifelse(`Target value` > 1, 1, `Target value`), 
          `Lower bound` = ifelse(`Lower bound` > 1, 1, `Lower bound`), 
          `Upper bound` = ifelse(`Upper bound` > 1, 1, `Upper bound`)
        )
      }
      
      tempDF <- tempDF %>% mutate(
        `Row value` = factor(`Row value`, levels=rev(levels(`Row value`)))
      ) 
      
      if(subtable=="SECTOR3"){
        if(measureName=="Share >0"){
          tempDF <- tempDF %>% mutate(
            `For Tooltip`=paste(
              "Target variable: ", tempDF$`Target name`, '\n', 
              "Selected group: ", tempDF$`Row value`, '\n', 
              "Sector: ", tempDF$`Sector value`, '\n', 
              "Estimate: ", percent(tempDF$`Target value`, accuracy=0.1), '\n',
              "95% confidence interval: ", percent(tempDF$`Lower bound`, accuracy=0.1), " to ", percent(tempDF$`Upper bound`, accuracy=0.1), 
              sep=""
            )
          )
        }
        if(measureName %in% c("Median", "Average")){
          tempDF <- tempDF %>% mutate(
            `For Tooltip`=paste(
              "Target variable: ", tempDF$`Target name`, '\n', 
              "Selected group: ", tempDF$`Row value`, '\n', 
              "Sector: ", tempDF$`Sector value`, '\n', 
              "Estimate: ", dollar(tempDF$`Target value`, accuracy=1), '\n',
              "95% confidence interval: ", dollar(tempDF$`Lower bound`, accuracy=1), " to ", dollar(tempDF$`Upper bound`, accuracy=1), 
              sep=""
            )
          )
        }
      }else{
        if(subtable=="INSTSTAT"){
          if(measureName=="Share >0"){
            tempDF <- tempDF %>% mutate(
              `For Tooltip`=paste(
                "Target variable: ", tempDF$`Target name`, '\n', 
                "Selected group: ", tempDF$`Row value`, '\n', 
                "State: ", tempDF$`State`, '\n', 
                "Estimate: ", percent(tempDF$`Target value`, accuracy=0.1), '\n',
                "95% confidence interval: ", percent(tempDF$`Lower bound`, accuracy=0.1), " to ", percent(tempDF$`Upper bound`, accuracy=0.1), 
                sep=""
              )
            )
          }
          if(measureName %in% c("Median", "Average")){
            tempDF <- tempDF %>% mutate(
              `For Tooltip`=paste(
                "Target variable: ", tempDF$`Target name`, '\n', 
                "Selected group: ", tempDF$`Row value`, '\n', 
                "State: ", tempDF$`State`, '\n', 
                "Estimate: ", dollar(tempDF$`Target value`, accuracy=1), '\n',
                "95% confidence interval: ", dollar(tempDF$`Lower bound`, accuracy=1), " to ", dollar(tempDF$`Upper bound`, accuracy=1), 
                sep=""
              )
            )
          }
        }else{
          stop("Bad input")
        }
      }
      
      
      plot1 <- ggplot(
        data=tempDF, mapping=aes(x=`Target value`, y=`Row value`, fill=`Row value`, text=`For Tooltip`)
      ) + geom_point() + geom_errorbar(
        aes(xmin=`Lower bound`, xmax=`Upper bound`, width=0.2)
      ) + labs(
        x=tempDF$`Title name`[1], y=""
      )
      
      if(subtable=="SECTOR3"){
        plot1 <- plot1 + facet_grid(`Sector value` ~ .) 
      }else{
        if(subtable=="INSTSTAT"){
          plot1 <- plot1 + facet_grid(`State` ~ .) 
        }else{
          stop("Bad input")
        }
      }
      
      if(measureName=="Share >0"){
        plot1 <- plot1 + scale_x_continuous(
          labels=percent_format(accuracy=1), limits=c(-0.03, 1.03), breaks=c(0, 0.25, 0.5, 0.75, 1)
        ) 
      }
      if(measureName %in% c("Median", "Average")){
        plot1 <- plot1 + scale_x_continuous(
          labels=dollar_format(accuracy=1, limits=c(
            tempDF$`Lower bound for axis`[1], 
            tempDF$`Upper bound for axis`[1]
          ))
        ) 
      }
      
      return(ggplotly(plot1, tooltip="text", width=overallWidth, height=heightIntercept+(nrow(tempDF)*heightCoefficient)))
      
      rm(plot1, tempDF)
      
    }
    
    #### End #### 
    
    #### Write chartFn2 ####
    
    chartFn2 <- function(distributionName, rowName){
      
      tempDF <- DIST.SECTOR3 %>% filter(
        `Distribution name`==distributionName, 
        `Row name`==rowName
      )
      
      tempDF <- tempDF %>% mutate(
        `Share` = `Share` / 100, 
        `Lower bound` = `Lower bound` / 100, 
        `Upper bound` = `Upper bound` / 100
      ) %>% mutate(
        `Share` = ifelse(`Share` < 0, 0, `Share`), 
        `Lower bound` = ifelse(`Lower bound` < 0, 0, `Lower bound`), 
        `Upper bound` = ifelse(`Upper bound` < 0, 0, `Upper bound`)
      ) %>% mutate(
        `Share` = ifelse(`Share` > 1, 1, `Share`), 
        `Lower bound` = ifelse(`Lower bound` > 1, 1, `Lower bound`), 
        `Upper bound` = ifelse(`Upper bound` > 1, 1, `Upper bound`)
      )
      
      # Confirm whether this is needed:
      tempDF <- tempDF %>% mutate(
        `Row value` = factor(`Row value`, levels=rev(levels(`Row value`)))
      ) 
      
      tempDF <- tempDF %>% mutate(
        `For Tooltip`=paste(
          "Distribution variable: ", tempDF$`Distribution name`, '\n', 
          "Selected group: ", tempDF$`Row value`, '\n', 
          "Sector: ", tempDF$`Sector value`, '\n', 
          "Estimate: ", percent(tempDF$`Share`, accuracy=0.1), '\n',
          "95% confidence interval: ", percent(tempDF$`Lower bound`, accuracy=0.1), " to ", percent(tempDF$`Upper bound`, accuracy=0.1), 
          sep=""
        )
      )
      
      plot1 <- ggplot(
        data=tempDF, mapping=aes(x=`Share`, y=`Row value`, fill=`Category name`, text=`For Tooltip`)
      ) + geom_bar(
        position="stack", stat="identity"
      ) + facet_grid(
        `Sector value` ~ .
      ) + labs(
        x=paste("Distribution of ", tempDF$`Distribution name`[1], " by ", tempDF$`Row name`[1]), y=""
      ) + theme(
        legend.position="top"
      ) + scale_x_continuous(labels=percent_format(accuracy=1))
      
      plot1 <- ggplotly(
        plot1, tooltip="text", width=overallWidth, height=heightIntercept+(nrow(tempDF)*heightCoefficient)
      ) %>% layout(legend = list(
        orientation = "h",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 0.5)             # put legend in center of x-axis
      )
      
      return(plot1)
      
      rm(plot1, tempDF)
      
    }
    
    #### End #### 
    
    output$`testPrintout` <- renderText({
      paste("Target: ", input$`targetVariable`, '\n',
            "Sector: ", input$`sectorSelect`, '\n',
            "Row: ", input$`rowSelect`, '\n',
            "Nrow 1: ", nrow(ampSector3), '\n',
            "Nrow 2: ", nrow(ampInststat2Y), '\n', 
            "Nrow 3: ", nrow(ampInststat4Y), '\n',
            "Nrow 4: ", nrow(distSector3), 
            sep="")
    })
    
  })
  
}

shinyApp(ui = ui, server = server)
