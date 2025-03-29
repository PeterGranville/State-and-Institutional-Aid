
#### Setup ####

library(shiny)
library(readxl)
library(scales)
library(plotly)
library(tidyverse)

#### End #### 

shinyServer(function(output, input)({
  
  ###############################################
  #### UI-responsive inputs                  ####
  ###############################################
  
  #### Establish target variable lists ####
  
  nationalTargetChoices <- c(
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
  
  stateTargetChoices <- c(
    "Share receiving state grants",
    "Average state grant award",
    "Share receiving institutional grants",
    "Average institutional grant award",
    "Median net student budget after all grants",
    "Median net student budget after all grants as a percentage of income",
    "Median student budget (attendance adjusted)"
  )
  
  #### End #### 
  
  #### Establish row variable lists ####
  
  nationalRowChoices <- c(
    "Income Quartile", 
    "Pell Recipient Status", 
    "Zero EFC Status",
    "Parents' Highest Education Level", 
    "Race/Ethnicity", 
    "Institution HBCU Status", 
    "Institution MSI Status", 
    "Institution Selectivity"
  )
  
  stateRowChoices <- c(
    "Income Quartile", 
    "Pell Recipient Status", 
    "Zero EFC Status",
    "Parents' Highest Education Level", 
    "Race/Ethnicity"
  )
  
  #### End ####
  
  #### Establish sector lists ####
  
  nationalSectorChoices <- c(
    "All Institutions",
    "Public 4-Years",
    "Public 2-Years", 
    "Other"
  )
  
  stateSectorChoices <- c(
    "Public 4-Years",
    "Public 2-Years"
  )
  
  #### End #### 
  
  #### Set choice lists ####
  
  targetChoices <- reactive({
    
    switch(input$view, 
           "National View" = nationalTargetChoices,
           "State View" = stateTargetChoices
           )
    
  })
  
  rowChoices <- reactive({
    
    switch(input$view, 
           "National View" = nationalRowChoices,
           "State View" = stateRowChoices
    )
    
  })
  
  sectorChoices <- reactive({
    
    switch(input$view, 
           "National View" = nationalSectorChoices,
           "State View" = stateSectorChoices
    )
    
  })
  
  #### End #### 
  
  #### Define renderUI for dropdown menus #### 
  
  output$selectionT <- renderUI({
    
    selectInput("selectTarget", 
                "Select a target variable", 
                choices=targetChoices())
    
  })
  
  output$selectionR <- renderUI({
    
    selectInput("selectRow", 
                "Select a grouping variable", 
                choices=rowChoices())
    
  })
  
  output$selectionS <- renderUI({
    
    selectInput("selectSector", 
                "Select a sector", 
                choices=sectorChoices())
    
  })
  
  #### End #### 
  
  ###############################################
  #### Figure generation                     ####
  ###############################################
  
  output$displayFig <- renderPlotly({
    
    #### Create "print" objects ####
    
    printView <- input$view
    printTarget <- input$selectTarget
    printRow <- input$selectRow
    printSector <- input$selectSector
    
    if(printTarget=="---Section 1: Aid distributions---"){
      printTarget <- "Share receiving federal Pell Grant"
    }
    if(printTarget=="---Section 2: Need-based and merit-based aid---"){
      printTarget <- "Share receiving state non-need & merit grants"
    }
    if(printTarget=="---Section 3: Combined aid measures---"){
      printTarget <- "Share receiving state or institutional grants"
    }
    if(printTarget=="---Section 4: Net price---"){
      printTarget <- "Share with unmet tuition and fees after all grants"
    }
    if(printTarget=="---Section 5: Pricing and need---"){
      printTarget <- "Median tuition and fees paid"
    }
    
    # printView <- "State View"
    # printTarget <- "Average state grant award"
    # printRow <- "Zero EFC Status"
    # printSector <- "Public 4-Years"
    
    #### End #### 
    
    #### Create factor lists ####
    
    levels.SectorValue <- c(
      "Total", 
      "Public 4-year",
      "Public 2-year", 
      "Other"
    )
    
    levels.TargetName <- c(
      "Federal Pell Grant",
      "Federal campus-based aid (SEOG, FWS)",
      "Title IV loans (includes Parent PLUS Loans)",
      "State grants total",
      "Institution grants total",
      "Private source grants",
      "State non-need & merit grants",
      "State need-based grants",
      "Institution non-need & merit grants",
      "Institutional need-based grants",
      "Total state and institutional grants",
      "Grant amount exceeding federal need",
      "Tuition and fees minus all grants",
      "Net tuition after all grants as percent of income",
      "Student budget minus all grants",
      "Net price after grants as percent of income",
      "Expected Family Contribution",
      "Tuition and fees paid",
      "Student budget (attendance adjusted)"
    )
    
    levels.RowName <- c(
      "Overall",
      "Income Quartile", 
      "Zero EFC Status", 
      "Pell Recipient Status", 
      "Parents' Highest Education Level", 
      "Race/Ethnicity", 
      "Institution HBCU Status", 
      "Institution MSI Status", 
      "Institution Selectivity", 
      "Institution State"
    )
    
    levels.MeasureName <- c(
      "Share >0", 
      "Average", 
      "Median"
    )
    
    levels.RowValue <- c(
      
      "Total",
      
      # Income Quartile
      "Top quartile",
      "Upper-middle quartile",
      "Lower-middle quartile",
      "Bottom quartile",
      
      # Institution HBCU Status
      "HBCUs",
      "Non-HBCUs",
      
      # Institution MSI Status
      "AAPISI",
      "HBCU",
      "HSI",
      "PBI",
      "Tribal college",
      "Other MSI",
      "Not an MSI",
      
      # Institution Selectivity
      "Very selective",
      "Moderately selective",  
      "Minimally selective",
      "Open admission",
      "Not a 4-year institution",
      
      # Institution State
      "Alabama",
      "Alaska",
      "Arizona",
      "Arkansas",
      "California",
      "Colorado",
      "Connecticut",
      "Delaware",
      "District of Columbia",
      "Florida",
      "Georgia",
      "Hawaii",
      "Idaho",
      "Illinois",
      "Indiana",
      "Iowa",
      "Kansas",
      "Kentucky",
      "Louisiana",
      "Maine",
      "Maryland",
      "Massachusetts",
      "Michigan",
      "Minnesota",
      "Mississippi",
      "Missouri",
      "Montana",
      "Nebraska",
      "Nevada",
      "New Hampshire",
      "New Jersey",
      "New Mexico",
      "New York",
      "North Carolina",
      "North Dakota",
      "Ohio",
      "Oklahoma",
      "Oregon",
      "Pennsylvania",
      "Puerto Rico",
      "Rhode Island",
      "South Carolina",
      "South Dakota",
      "Tennessee",
      "Texas",
      "Utah",
      "Vermont",
      "Virginia",
      "Washington",
      "West Virginia",
      "Wisconsin",
      "Wyoming",
      
      # Parents' Highest Education Level
      "College or beyond",
      "High school",
      "Middle school/junior high",
      
      # Pell Recipient Status
      "Pell recipient",
      "Not a Pell recipient",
      
      # Race/Ethnicity
      "White",
      "Hispanic or Latino",
      "Black or African American",
      "Asian",
      "Native American",
      "Native Hawaiian/Pacific Islander",
      "More than one race",
      
      # Zero EFC Status
      "Zero EFC",
      "Nonzero EFC"
    )
    
    levels.DistributionName <- c(
      "Income Quartile", 
      "Zero EFC Status", 
      "Pell Recipient Status", 
      "Parents' Highest Education Level", 
      "Race/Ethnicity", 
      "High School GPA",
      "Institution HBCU Status", 
      "Institution MSI Status", 
      "Institution Selectivity" 
    )
    
    levels.CategoryName <- c(
      levels.RowValue, c(
        # High School GPA
        "0.5 to 0.9", 
        "1.0 to 1.4", 
        "1.5 to 1.9", 
        "2.0 to 2.4", 
        "2.5 to 2.9", 
        "3.0 to 3.4", 
        "3.5 to 4.0"
      )
    )
    
    levels.State <- c(
      "Total",
      "Alabama",
      "Alaska",
      "Arizona",
      "Arkansas",
      "California",
      "Colorado",
      "Connecticut",
      "Delaware",
      "District of Columbia",
      "Florida",
      "Georgia",
      "Hawaii",
      "Idaho",
      "Illinois",
      "Indiana",
      "Iowa",
      "Kansas",
      "Kentucky",
      "Louisiana",
      "Maine",
      "Maryland",
      "Massachusetts",
      "Michigan",
      "Minnesota",
      "Mississippi",
      "Missouri",
      "Montana",
      "Nebraska",
      "Nevada",
      "New Hampshire",
      "New Jersey",
      "New Mexico",
      "New York",
      "North Carolina",
      "North Dakota",
      "Ohio",
      "Oklahoma",
      "Oregon",
      "Pennsylvania",
      "Puerto Rico",
      "Rhode Island",
      "South Carolina",
      "South Dakota",
      "Tennessee",
      "Texas",
      "Utah",
      "Vermont",
      "Virginia",
      "Washington",
      "West Virginia",
      "Wisconsin",
      "Wyoming"
    )
    
    #### End #### 
    
    #### Load targetLookup and axisBounds ####
    
    targetLookup <- read_excel("Target-Lookup.xls") %>% select(-(`Entry`)) %>% select(-(`Section`))
    axisBounds <- read.csv("Axis-Bounds.csv", header=TRUE, check.names=FALSE)
    
    #### End #### 
    
    #### Load dataset, create tempDF ####
    
    if(printView=="National View"){
      tempDF <- read.csv(
        "AMP-SECTOR3.csv", header=TRUE, check.names=FALSE
      )
    }
    if(printView=="State View"){
      if(printSector=="Public 4-Years"){
        tempDF <- read.csv(
          "AMP-INSTSTAT-4Y.csv", header=TRUE, check.names=FALSE
        ) %>% mutate(
          `Sector value` = rep("Public 4-year")
        )
      }
      if(printSector=="Public 2-Years"){
        tempDF <- read.csv(
          "AMP-INSTSTAT-2Y.csv", header=TRUE, check.names=FALSE
        ) %>% mutate(
          `Sector value` = rep("Public 2-year")
        )
      }
    }
    
    #### End #### 
    
    #### Import targetLookup and axisBounds to tempDF ####
    
    tempDF <- left_join(x=tempDF, y=targetLookup, by=c("Target name", "Measure name"))
    tempDF <- left_join(x=tempDF, y=axisBounds, by="Target name")
    
    #### End #### 
    
    #### Filter by row variable and sector ####
    
    tempDF <- tempDF %>% filter(
      `Title`==printTarget, 
      `Row name`==printRow
    )
    
    if(printSector=="All Institutions"){
      tempDF <- tempDF %>% filter(
        `Sector value`=="Total"
      )
    }
    if(printSector=="Public 4-Years"){
      tempDF <- tempDF %>% filter(
        `Sector value`=="Public 4-year"
      )
    }
    if(printSector=="Public 2-Years"){
      tempDF <- tempDF %>% filter(
        `Sector value`=="Public 2-year"
      )
    }
    if(printSector=="Other"){
      tempDF <- tempDF %>% filter(
        `Sector value`=="Other"
      )
    }
    
    #### End #### 
    
    #### Clean percentages ####
    
    if(substr(printTarget, 1, 5)=="Share"){
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
    
    #### End #### 
    
    #### Set row value and sector value as factors ####
    
    tempDF <- tempDF %>% mutate(
      `Sector value` = factor(`Sector value`, levels=levels.SectorValue),
      `Row value` = factor(`Row value`, levels=rev(levels.RowValue))
    ) 
    
    #### End #### 
    
    #### Establish tooltip ####
    
    if(printView=="National View"){
      if(substr(printTarget, 1, 5)=="Share"){
        tempDF <- tempDF %>% mutate(
          `For Tooltip`=paste(
            "Target variable: ", tempDF$`Title`, '\n', 
            "Selected group: ", tempDF$`Row value`, '\n', 
            "Sector: ", tempDF$`Sector value`, '\n', 
            "Estimate: ", percent(tempDF$`Target value`, accuracy=0.1), '\n',
            "95% confidence interval: ", percent(tempDF$`Lower bound`, accuracy=0.1), " to ", percent(tempDF$`Upper bound`, accuracy=0.1), 
            sep=""
          )
        )
      }
      if(substr(printTarget, 1, 5) %in% c("Media", "Avera")){
        tempDF <- tempDF %>% mutate(
          `For Tooltip`=paste(
            "Target variable: ", tempDF$`Title`, '\n', 
            "Selected group: ", tempDF$`Row value`, '\n', 
            "Sector: ", tempDF$`Sector value`, '\n', 
            "Estimate: ", dollar(tempDF$`Target value`, accuracy=1), '\n',
            "95% confidence interval: ", dollar(tempDF$`Lower bound`, accuracy=1), " to ", dollar(tempDF$`Upper bound`, accuracy=1), 
            sep=""
          )
        )
      }
    }else{
      if(printView=="State View"){
        if(substr(printTarget, 1, 5)=="Share"){
          tempDF <- tempDF %>% mutate(
            `For Tooltip`=paste(
              "Target variable: ", tempDF$`Title`, '\n', 
              "Selected group: ", tempDF$`Row value`, '\n', 
              "State/Sector: ", tempDF$`State`, " ", printSector, '\n', 
              "Estimate: ", percent(tempDF$`Target value`, accuracy=0.1), '\n',
              "95% confidence interval: ", percent(tempDF$`Lower bound`, accuracy=0.1), " to ", percent(tempDF$`Upper bound`, accuracy=0.1), 
              sep=""
            )
          )
        }
        if(substr(printTarget, 1, 5) %in% c("Media", "Avera")){
          tempDF <- tempDF %>% mutate(
            `For Tooltip`=paste(
              "Target variable: ", tempDF$`Title`, '\n', 
              "Selected group: ", tempDF$`Row value`, '\n', 
              "State/Sector: ", tempDF$`State`, " ", printSector, '\n',
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
    
    #### End #### 
    
    #### Define plot1 ####
    
    plot1 <- ggplot(
      data=tempDF, mapping=aes(x=`Target value`, y=`Row value`, fill=`Row value`, text=`For Tooltip`)
    ) + geom_point() + geom_errorbar(
      aes(xmin=`Lower bound`, xmax=`Upper bound`, width=0.2)
    ) + labs(
      x=tempDF$`Title`[1], y=""
    )
    
    #### End #### 
    
    #### Format plot1 facet ####
    
    if(printView=="State View"){
      plot1 <- plot1 + facet_wrap(
        `State`~., strip.position = "top", ncol=1
      )
    }
    
    #### End #### 
    
    #### Format plot1 x-axis ####
    
    if(substr(printTarget, 1, 5)=="Share"){
      plot1 <- plot1 + scale_x_continuous(
        labels=percent_format(accuracy=1), limits=c(-0.03, 1.03), breaks=c(0, 0.25, 0.5, 0.75, 1)
      ) 
    }
    
    if(substr(printTarget, 1, 5) %in% c("Media", "Avera")){
      plot1 <- plot1 + scale_x_continuous(
        labels=dollar_format(accuracy=1, limits=c(
          tempDF$`Lower bound for axis`[1], 
          tempDF$`Upper bound for axis`[1]
        ))
      ) 
    }
    
    #### End #### 
    
    #### Finalize plot1 #### 
  
    ggplotly(
      plot1, tooltip="text"
    ) %>% layout(legend = list(
      orientation = "h",   
      xanchor = "center",
      x = 0.5)             
    )
  
    #### End ####   
    
  })
  
  ###############################################
  #### Text generation                       ####
  ###############################################
    
  output$summary <- renderText({
    
    #### Create "print" objects ####
    
    printView <- input$view
    printTarget <- input$selectTarget
    printRow <- input$selectRow
    printSector <- input$selectSector
    
    #### End #### 
    
    #### Create factor lists ####
    
    levels.SectorValue <- c(
      "Total", 
      "Public 4-year",
      "Public 2-year", 
      "Other"
    )
    
    levels.TargetName <- c(
      "Federal Pell Grant",
      "Federal campus-based aid (SEOG, FWS)",
      "Title IV loans (includes Parent PLUS Loans)",
      "State grants total",
      "Institution grants total",
      "Private source grants",
      "State non-need & merit grants",
      "State need-based grants",
      "Institution non-need & merit grants",
      "Institutional need-based grants",
      "Total state and institutional grants",
      "Grant amount exceeding federal need",
      "Tuition and fees minus all grants",
      "Net tuition after all grants as percent of income",
      "Student budget minus all grants",
      "Net price after grants as percent of income",
      "Expected Family Contribution",
      "Tuition and fees paid",
      "Student budget (attendance adjusted)"
    )
    
    levels.RowName <- c(
      "Overall",
      "Income Quartile", 
      "Zero EFC Status", 
      "Pell Recipient Status", 
      "Parents' Highest Education Level", 
      "Race/Ethnicity", 
      "Institution HBCU Status", 
      "Institution MSI Status", 
      "Institution Selectivity", 
      "Institution State"
    )
    
    levels.MeasureName <- c(
      "Share >0", 
      "Average", 
      "Median"
    )
    
    levels.RowValue <- c(
      
      "Total",
      
      # Income Quartile
      "Top quartile",
      "Upper-middle quartile",
      "Lower-middle quartile",
      "Bottom quartile",
      
      # Institution HBCU Status
      "HBCUs",
      "Non-HBCUs",
      
      # Institution MSI Status
      "AAPISI",
      "HBCU",
      "HSI",
      "PBI",
      "Tribal college",
      "Other MSI",
      "Not an MSI",
      
      # Institution Selectivity
      "Very selective",
      "Moderately selective",  
      "Minimally selective",
      "Open admission",
      "Not a 4-year institution",
      
      # Institution State
      "Alabama",
      "Alaska",
      "Arizona",
      "Arkansas",
      "California",
      "Colorado",
      "Connecticut",
      "Delaware",
      "District of Columbia",
      "Florida",
      "Georgia",
      "Hawaii",
      "Idaho",
      "Illinois",
      "Indiana",
      "Iowa",
      "Kansas",
      "Kentucky",
      "Louisiana",
      "Maine",
      "Maryland",
      "Massachusetts",
      "Michigan",
      "Minnesota",
      "Mississippi",
      "Missouri",
      "Montana",
      "Nebraska",
      "Nevada",
      "New Hampshire",
      "New Jersey",
      "New Mexico",
      "New York",
      "North Carolina",
      "North Dakota",
      "Ohio",
      "Oklahoma",
      "Oregon",
      "Pennsylvania",
      "Puerto Rico",
      "Rhode Island",
      "South Carolina",
      "South Dakota",
      "Tennessee",
      "Texas",
      "Utah",
      "Vermont",
      "Virginia",
      "Washington",
      "West Virginia",
      "Wisconsin",
      "Wyoming",
      
      # Parents' Highest Education Level
      "College or beyond",
      "High school",
      "Middle school/junior high",
      
      # Pell Recipient Status
      "Pell recipient",
      "Not a Pell recipient",
      
      # Race/Ethnicity
      "White",
      "Hispanic or Latino",
      "Black or African American",
      "Asian",
      "Native American",
      "Native Hawaiian/Pacific Islander",
      "More than one race",
      
      # Zero EFC Status
      "Zero EFC",
      "Nonzero EFC"
    )
    
    levels.DistributionName <- c(
      "Income Quartile", 
      "Zero EFC Status", 
      "Pell Recipient Status", 
      "Parents' Highest Education Level", 
      "Race/Ethnicity", 
      "High School GPA",
      "Institution HBCU Status", 
      "Institution MSI Status", 
      "Institution Selectivity" 
    )
    
    levels.CategoryName <- c(
      levels.RowValue, c(
        # High School GPA
        "0.5 to 0.9", 
        "1.0 to 1.4", 
        "1.5 to 1.9", 
        "2.0 to 2.4", 
        "2.5 to 2.9", 
        "3.0 to 3.4", 
        "3.5 to 4.0"
      )
    )
    
    levels.State <- c(
      "Total",
      "Alabama",
      "Alaska",
      "Arizona",
      "Arkansas",
      "California",
      "Colorado",
      "Connecticut",
      "Delaware",
      "District of Columbia",
      "Florida",
      "Georgia",
      "Hawaii",
      "Idaho",
      "Illinois",
      "Indiana",
      "Iowa",
      "Kansas",
      "Kentucky",
      "Louisiana",
      "Maine",
      "Maryland",
      "Massachusetts",
      "Michigan",
      "Minnesota",
      "Mississippi",
      "Missouri",
      "Montana",
      "Nebraska",
      "Nevada",
      "New Hampshire",
      "New Jersey",
      "New Mexico",
      "New York",
      "North Carolina",
      "North Dakota",
      "Ohio",
      "Oklahoma",
      "Oregon",
      "Pennsylvania",
      "Puerto Rico",
      "Rhode Island",
      "South Carolina",
      "South Dakota",
      "Tennessee",
      "Texas",
      "Utah",
      "Vermont",
      "Virginia",
      "Washington",
      "West Virginia",
      "Wisconsin",
      "Wyoming"
    )
    
    #### End #### 
    
    #### Load targetLookup and axisBounds ####
    
    targetLookup <- read_excel("Target-Lookup.xls") %>% select(-(`Entry`)) %>% select(-(`Section`))

    #### End #### 
    
    #### Load dataset, create tempDF ####
    
    if(printView=="National View"){
      tempDF <- read.csv(
        "AMP-SECTOR3.csv", header=TRUE, check.names=FALSE
      )
    }
    if(printView=="State View"){
      if(printSector=="Public 4-Years"){
        tempDF <- read.csv(
          "AMP-INSTSTAT-4Y.csv", header=TRUE, check.names=FALSE
        ) %>% mutate(
          `Sector value` = rep("Public 4-year")
        )
      }
      if(printSector=="Public 2-Years"){
        tempDF <- read.csv(
          "AMP-INSTSTAT-2Y.csv", header=TRUE, check.names=FALSE
        ) %>% mutate(
          `Sector value` = rep("Public 2-year")
        )
      }
    }
    
    #### End #### 
    
    #### Import targetLookup to tempDF ####
    
    tempDF <- left_join(x=tempDF, y=targetLookup, by=c("Target name", "Measure name"))

    #### End #### 
    
    #### Filter by row variable and sector ####
    
    tempDF <- tempDF %>% filter(
      `Title`==printTarget, 
      `Row name`==printRow
    )
    
    if(printSector=="All Institutions"){
      tempDF <- tempDF %>% filter(
        `Sector value`=="Total"
      )
    }
    if(printSector=="Public 4-Years"){
      tempDF <- tempDF %>% filter(
        `Sector value`=="Public 4-year"
      )
    }
    if(printSector=="Public 2-Years"){
      tempDF <- tempDF %>% filter(
        `Sector value`=="Public 2-year"
      )
    }
    if(printSector=="Other"){
      tempDF <- tempDF %>% filter(
        `Sector value`=="Other"
      )
    }
    
    #### End #### 
    
    #### Set row value and sector value as factors ####
    
    tempDF <- tempDF %>% mutate(
      `Sector value` = factor(`Sector value`, levels=levels.SectorValue),
      `Row value` = factor(`Row value`, levels=rev(levels.RowValue))
    ) 
    
    #### End #### 
    
    #### Return text ####
    
    paste("The view is ", printView, ". The target is ", printTarget, ". The row is ", printRow, ". The sector is ", printSector, ". The number of rows is ", nrow(tempDF), ".", sep="")
    
    #### End #### 
    
  })
  
}))



