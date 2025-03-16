
#### Setup ####

library(scales)
library(tidyverse)

#### End #### 

################################################
#### Disaggregated by SECTOR3               ####
################################################

#### Write function to load averages, percentages, and shares above zero ####

loadDatalab1 <- function(
  sectionName, 
  retrievalCode, 
  sectorValRow, 
  dataStart, 
  dataEnd 
){
  
  dataLength <- dataEnd - dataStart + 1
  
  workingDir <- paste("/Users/peter_granville/Net Price Equity/NPSAS Data/", sectionName, sep="")
  setwd(workingDir)
  rm(workingDir)
  
  fileList <- list.files(getwd())
  correctFileName <- NA
  for(i in (1:length(fileList))){
    tempDF <- read.csv(fileList[i], header=FALSE)
    if(grepl(retrievalCode, tempDF$V1[nrow(tempDF)])){
      correctFileName <- fileList[i]
    }
    rm(tempDF)
  }
  rm(i, fileList)
  
  if(is.na(correctFileName)){
    stop("File with retrieval code not found")
  }else{
    
    dataDF <- read.csv(
      file=correctFileName,
      skip=dataStart-1, 
      nrows=dataLength,
      header=FALSE
    )
    rm(dataLength)
    
    sectorValDF <- read.csv(
      file=correctFileName, 
      skip=sectorValRow-1, 
      nrows=1,
      header=FALSE
    )
    
    sectorName <- sectorValDF$V1[1]
    rm(sectorValDF)
    
    targetName <- dataDF$V2[1]
    
    if(dataDF$V2[2]== "0 <= X <= 0"){ 
      # Percentages
      measureName <- "Share >0"
      rowName <- dataDF$V1[5]
      dataDF <- dataDF %>% select(-`V2`) %>% select(-`V3`)
      if("100%" %in% dataDF$`V6`){dataDF <- dataDF %>% select(-`V6`)}
      dataDF <- dataDF %>% filter(
        (`V1` != "") & (`V4` != "")
      )
      dataDF <- dataDF %>% rename(
        `V2` = `V4`, 
        `V3` = `V5`
      )
    }else{
      # Averages or Medians 
      measureName <- dataDF$V2[2]
      rowName <- dataDF$V1[5]
      measureName <- gsub("[() ]", "", measureName)
      dataDF <- dataDF %>% filter(
        (`V1` != "") & (`V2` != "")
      )
    }

    if((measureName %in% c("Average", "Median", "Share >0"))==FALSE){
      stop("The measure detetcted is not average, median, or share above 0. Use different function.")
    }else{
      dataDF <- dataDF %>% rename(
        `Row value` = `V1`, 
        `Target value` = `V2`, 
        `Target confidence interval` = `V3`
      ) %>% mutate(
        `Sector value` = rep(sectorName), 
        `Target name` = rep(targetName), 
        `Row name` = rep(rowName),
        `Measure name` = rep(measureName), 
        `Source code` = rep(retrievalCode), 
        `Section name` = rep(sectionName)
      )
      return(dataDF)
      rm(dataDF, sectorName, targetName, rowName, measureName, correctFile)
    }
  }
}

#### End #### 

#### Write function to load for % distributions ####

#### Write function to load averages, percentages, and shares above zero ####

loadDatalab2 <- function(
    sectionName, 
    retrievalCode, 
    sectorValRow, 
    dataStart, 
    dataEnd 
){
  
  dataLength <- dataEnd - dataStart + 1
  
  workingDir <- paste("/Users/peter_granville/Net Price Equity/NPSAS Data/", sectionName, sep="")
  setwd(workingDir)
  rm(workingDir)
  
  fileList <- list.files(getwd())
  correctFileName <- NA
  for(i in (1:length(fileList))){
    tempDF <- read.csv(fileList[i], header=FALSE)
    if(grepl(retrievalCode, tempDF$V1[nrow(tempDF)])){
      correctFileName <- fileList[i]
    }
    rm(tempDF)
  }
  rm(i, fileList)
  
  if(is.na(correctFileName)){
    stop("File with retrieval code not found")
  }else{
    
    dataDF <- read.csv(
      file=correctFileName,
      skip=dataStart-1, 
      nrows=dataLength,
      header=FALSE
    )
    rm(dataLength)
    
    sectorValDF <- read.csv(
      file=correctFileName, 
      skip=sectorValRow-1, 
      nrows=1,
      header=FALSE
    )
    
    sectorName <- sectorValDF$V1[1]
    rm(sectorValDF)
    
    distributionName <- dataDF$V2[1]
    rowName <- dataDF$V1[5]
    nDist <- (ncol(dataDF) - 2) / 2
    
    if(nDist==2){
      if("100%" %in% dataDF$`V6`){dataDF <- dataDF %>% select(-`V6`)}
      distCategoryName1 <- dataDF$V2[2] 
      distCategoryName2 <- dataDF$V4[2] 
    }
    if(nDist==3){
      if("100%" %in% dataDF$`V8`){dataDF <- dataDF %>% select(-`V8`)}
      distCategoryName1 <- dataDF$V2[2] 
      distCategoryName2 <- dataDF$V4[2] 
      distCategoryName3 <- dataDF$V6[2] 
      
    }
    if(nDist==4){
      if("100%" %in% dataDF$`V10`){dataDF <- dataDF %>% select(-`V10`)}
      distCategoryName1 <- dataDF$V2[2] 
      distCategoryName2 <- dataDF$V4[2] 
      distCategoryName3 <- dataDF$V6[2] 
      distCategoryName4 <- dataDF$V8[2] 
    }
    if(nDist==5){
      if("100%" %in% dataDF$`V12`){dataDF <- dataDF %>% select(-`V12`)}
      distCategoryName1 <- dataDF$V2[2] 
      distCategoryName2 <- dataDF$V4[2] 
      distCategoryName3 <- dataDF$V6[2] 
      distCategoryName4 <- dataDF$V8[2] 
      distCategoryName5 <- dataDF$V10[2] 
    }
    if(nDist==6){
      if("100%" %in% dataDF$`V14`){dataDF <- dataDF %>% select(-`V14`)}
      distCategoryName1 <- dataDF$V2[2] 
      distCategoryName2 <- dataDF$V4[2] 
      distCategoryName3 <- dataDF$V6[2] 
      distCategoryName4 <- dataDF$V8[2] 
      distCategoryName5 <- dataDF$V10[2] 
      distCategoryName6 <- dataDF$V12[2] 
    }
    if(nDist==7){
      if("100%" %in% dataDF$`V16`){dataDF <- dataDF %>% select(-`V16`)}
      distCategoryName1 <- dataDF$V2[2] 
      distCategoryName2 <- dataDF$V4[2] 
      distCategoryName3 <- dataDF$V6[2] 
      distCategoryName4 <- dataDF$V8[2] 
      distCategoryName5 <- dataDF$V10[2] 
      distCategoryName6 <- dataDF$V12[2] 
      distCategoryName7 <- dataDF$V14[2] 
    }

    dataDF <- dataDF %>% filter(
      `V1` != "", 
      `V2` != ""
    ) %>% rename(
      `Row value` = `V1`,
      `Category 1 share` = `V2`, 
      `Category 1 interval` = `V3`,
      `Category 2 share` = `V4`, 
      `Category 2 interval` = `V5`
    )
    categoryNamesDF <- data.frame(
      `Category number` = character(), 
      `Category name` = character(), 
      check.names=FALSE
    ) %>% add_row(
      `Category number` = "Category 1", 
      `Category name` = distCategoryName1
    ) %>% add_row(
      `Category number` = "Category 2", 
      `Category name` = distCategoryName2
    )
    if(nDist >= 3){
      dataDF <- dataDF %>% rename(
        `Category 3 share` = `V6`, 
        `Category 3 interval` = `V7`
      )
      categoryNamesDF <- categoryNamesDF %>% add_row(
        `Category number` = "Category 3", 
        `Category name` = distCategoryName3
      )
    }
    if(nDist >= 4){
      dataDF <- dataDF %>% rename(
        `Category 4 share` = `V8`, 
        `Category 4 interval` = `V9`
      )
      categoryNamesDF <- categoryNamesDF %>% add_row(
        `Category number` = "Category 4", 
        `Category name` = distCategoryName4
      )
    }
    if(nDist >= 5){
      dataDF <- dataDF %>% rename(
        `Category 5 share` = `V10`, 
        `Category 5 interval` = `V11`
      )
      categoryNamesDF <- categoryNamesDF %>% add_row(
        `Category number` = "Category 5", 
        `Category name` = distCategoryName5
      )
    }
    if(nDist >= 6){
      dataDF <- dataDF %>% rename(
        `Category 6 share` = `V12`, 
        `Category 6 interval` = `V13`
      )
      categoryNamesDF <- categoryNamesDF %>% add_row(
        `Category number` = "Category 6", 
        `Category name` = distCategoryName6
      )
    }
    if(nDist >= 7){
      dataDF <- dataDF %>% rename(
        `Category 7 share` = `V14`, 
        `Category 7 interval` = `V15`
      )
      categoryNamesDF <- categoryNamesDF %>% add_row(
        `Category number` = "Category 7", 
        `Category name` = distCategoryName7
      )
    }
    
    sharesDF <- dataDF %>% select(
      `Row value`, 
      any_of(c(
        "Category 1 share", 
        "Category 2 share", 
        "Category 3 share", 
        "Category 4 share", 
        "Category 5 share", 
        "Category 6 share", 
        "Category 7 share"
      ))
    ) %>% pivot_longer(
      cols=any_of(c(
        "Category 1 share", 
        "Category 2 share", 
        "Category 3 share", 
        "Category 4 share", 
        "Category 5 share", 
        "Category 6 share", 
        "Category 7 share"
      )), 
      names_to="Category number", 
      values_to="Share"
    )
    intervalsDF <- dataDF %>% select(
      `Row value`, 
      any_of(c(
        "Category 1 interval", 
        "Category 2 interval", 
        "Category 3 interval", 
        "Category 4 interval", 
        "Category 5 interval", 
        "Category 6 interval", 
        "Category 7 interval"
      ))
    ) %>% pivot_longer(
      cols=any_of(c(
        "Category 1 interval", 
        "Category 2 interval", 
        "Category 3 interval", 
        "Category 4 interval", 
        "Category 5 interval", 
        "Category 6 interval", 
        "Category 7 interval"
      )), 
      names_to="Category number", 
      values_to="Interval"
    )
    
    sharesDF$`Category number` <- gsub(" share", "", sharesDF$`Category number`)
    intervalsDF$`Category number` <- gsub(" interval", "", intervalsDF$`Category number`)
    
    dataDF <- full_join(x=sharesDF, y=intervalsDF, by=c("Row value", "Category number"))
    rm(sharesDF, intervalsDF)
    
    dataDF <- left_join(x=dataDF, y=categoryNamesDF, by="Category number")
    rm(categoryNamesDF)
    
    
    
    
    if(dataDF$V2[2]== "0 <= X <= 0"){ 
      # Percentages
      measureName <- "Share >0"
      dataDF <- dataDF %>% select(-`V2`) %>% select(-`V3`)
      if("100%" %in% dataDF$`V6`){dataDF <- dataDF %>% select(-`V6`)}
      dataDF <- dataDF %>% filter(
        (`V1` != "") & (`V4` != "")
      )
      dataDF <- dataDF %>% rename(
        `V2` = `V4`, 
        `V3` = `V5`
      )
    }else{
      # Averages or Medians 
      measureName <- dataDF$V2[2]
      rowName <- dataDF$V1[5]
      measureName <- gsub("[() ]", "", measureName)
      dataDF <- dataDF %>% filter(
        (`V1` != "") & (`V2` != "")
      )
    }
    
    if((measureName %in% c("Average", "Median", "Share >0"))==FALSE){
      stop("The measure detetcted is not average, median, or share above 0. Use different function.")
    }else{
      dataDF <- dataDF %>% rename(
        `Row value` = `V1`, 
        `Distribution value` = `V2`, 
        `Distribution confidence interval` = `V3`
      ) %>% mutate(
        `Sector value` = rep(sectorName), 
        `Distribution name` = rep(distributionName), 
        `Row name` = rep(rowName),
        `Measure name` = rep(measureName), 
        `Source code` = rep(retrievalCode), 
        `Section name` = rep(sectionName)
      )
      return(dataDF)
      rm(dataDF, sectorName, distributionName, rowName, measureName, correctFile)
    }
  }
}

#### End #### 

#### End #### 

# A1
# A2
# A3
# A4
# A5
# A6
# A7
# A8
# A9
# A10
# A11
# B1
# B2
# C1
# C2
# C3
# C4
# C5
# C6
# C7
# D1
# D2
# D3
# D4
# D5
# D6
# D7
# D8
# D9
# D10
# E1
# E2
# E3
# E4
# E5
# F1
# F2
# F3
# F4
# F5

