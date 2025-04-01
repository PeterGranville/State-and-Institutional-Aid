
#### Setup ####

library(scales)
library(plotly)
library(tidyverse)
library(kableExtra)

setwd("/Users/peter_granville/Net Price Equity")

#### End #### 

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
    if(sectionName=="D10 Percentages"){
      rowName <- "Overall"
    }else{
      rowName <- dataDF$V1[5]
    }
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
    
    totalCategories <- length(unique(dataDF$`Category number`))
    
    dataDF <- dataDF %>% mutate(
      `Sector value` = rep(sectorName), 
      `Distribution name` = rep(distributionName), 
      `Number of categories` = rep(totalCategories),
      `Row name` = rep(rowName),
      `Source code` = rep(retrievalCode), 
      `Section name` = rep(sectionName)
    )
    return(dataDF)
    rm(dataDF, sectorName, distributionName, rowName, measureName, correctFile, totalCategories)
  }
}

#### End #### 

#### Write function to identify retrieval code, sectorRowVal, dataStart, and dataEnd from files ####

fileScan <- function(fileName, folderName, subtableSelect){
  
  returnDF <- data.frame(
    `folder` = character(), 
    `subtable` = character(), 
    `retrievalCode` = character(), 
    `sectorRowVal` = numeric(), 
    `dataStart` = numeric(), 
    `dataEnd` = numeric()
  )
  
  workingDir <- paste("/Users/peter_granville/Net Price Equity/NPSAS Data/", folderName, sep="")
  setwd(workingDir)
  rm(workingDir)
  
  tempDF <- read.csv(fileName, header=FALSE, blank.lines.skip=FALSE, col.names=paste("V", (1:20), sep="")) 
  tempDF <- tempDF %>% mutate(
    `Original row number` = (1:nrow(tempDF))
  )
  
  retrievalCodeLine <- tempDF$V1[nrow(tempDF)]
  retrievalCodeLine <- gsub("The code to retrieve results is ", "", retrievalCodeLine)
  retrievalCodeLine <- gsub("[.]", "", retrievalCodeLine)
  
  backstopLine <- tempDF %>% filter(
    (grepl("! Interpret", `V1`)) | (grepl("Reporting standards", `V1`)) | (grepl("The names of the variables", `V1`)) | (grepl("The weight variable used", `V1`))
  )
  backstopLine <- min(backstopLine$`Original row number`, na.rm=TRUE)
  
  if(subtableSelect=="SECTOR3"){
    searchTerm <- "NPSAS institution sector"
  }else{
    if(subtableSelect=="INSTSTAT"){
      searchTerm <- "NPSAS institution state"
    }else{
      stop("Improper subtable selection")
    }
  }
  
  sectorLabels <- tempDF %>% filter(
    grepl(searchTerm, `V1`)==TRUE, 
    grepl("Filtered by", `V1`)==FALSE
  )
  rm(searchTerm)
  sectorLabels <- sectorLabels$`Original row number`

  intervalLabels <- tempDF %>% filter(
    grepl("CONFIDENCE INTERVALS", `V1`)==TRUE, 
  )
  intervalLabels <- intervalLabels$`Original row number`
  
  if(length(sectorLabels) != length(intervalLabels)){
    stop("Error")
  }else{
    for(i in (1:length(sectorLabels))){
      startOfSet <- intervalLabels[i]
      if(i==length(sectorLabels)){
        endOfSet <- backstopLine
      }else{
        endOfSet <- sectorLabels[i+1]
      }
      if(tempDF$V1[endOfSet - 1]==""){endOfSet <- endOfSet - 1}
      returnDF <- returnDF %>% add_row(
        `folder` = folderName,
        `subtable` = subtableSelect,
        `retrievalCode` = retrievalCodeLine, 
        `sectorRowVal` = sectorLabels[i], 
        `dataStart` = startOfSet + 1, 
        `dataEnd` = endOfSet - 1
      )
      rm(startOfSet, endOfSet)
    }
    rm(i)
  }
  
  return(returnDF)
  rm(backstopLine, intervalLabels, sectorLabels, retrievalCodeLine, tempDF, returnDF)
}

#### End #### 

#### Write function to identify key info from all files in a folder ####

folderScan <- function(folderName0, subtableSelect0){
  
  workingDir <- paste("/Users/peter_granville/Net Price Equity/NPSAS Data/", folderName0, sep="")
  setwd(workingDir)
  rm(workingDir)
  
  fileList <- list.files(getwd())
  
  for(i in (1:length(fileList))){
    
    tempDF <- fileScan(fileName=fileList[i], folderName=folderName0, subtableSelect=subtableSelect0)
    if(i==1){
      returnDF <- tempDF
    }else{
      returnDF <- rbind(returnDF, tempDF)
    }
    rm(tempDF)
    
  }
  rm(i)
  
  return(returnDF)
  rm(fileList)
}

#### End #### 

#### Write function to pull all the prior functions together ####

runAllFunctions <- function(foldersForProcessing, functionSelection, subtableSelection){
  
  for(i in (1:length(foldersForProcessing))){
    
    print(paste("Running folder ", foldersForProcessing[i], ".", sep=""))
    runThese <- folderScan(folderName0=foldersForProcessing[i], subtableSelect0=subtableSelection)
    for(j in (1:nrow(runThese))){
      
      if(functionSelection=="loadDatalab1"){
        innerEnvelope <- loadDatalab1(
          sectionName = runThese$folder[j], 
          retrievalCode = runThese$retrievalCode[j], 
          sectorValRow = runThese$sectorRowVal[j], 
          dataStart = runThese$dataStart[j], 
          dataEnd = runThese$dataEnd[j]
        )
      }else{
        if(functionSelection=="loadDatalab2"){
          innerEnvelope <- loadDatalab2(
            sectionName = runThese$folder[j], 
            retrievalCode = runThese$retrievalCode[j], 
            sectorValRow = runThese$sectorRowVal[j], 
            dataStart = runThese$dataStart[j], 
            dataEnd = runThese$dataEnd[j]
          )
        }else{
          stop("Error")
        }
      }
      if(j==1){
        middleEnvelope <- innerEnvelope
      }else{
        middleEnvelope <- rbind(middleEnvelope, innerEnvelope)
      }
      rm(innerEnvelope)
    }
    rm(j)
    
    if(i==1){
      outerEnvelope <- middleEnvelope
    }else{
      outerEnvelope <- rbind(outerEnvelope, middleEnvelope)
    }
    rm(middleEnvelope)
  }
  rm(i)
  
  return(outerEnvelope)
  rm(outerEnvelope)
}

#### End #### 

################################################
#### Disaggregated by SECTOR3               ####
################################################

#### Averages, Medians, Percents (loadDatalab1) ####

AMP.SECTOR3 <- runAllFunctions(foldersForProcessing=c(
  "A1 Averages", 
  "A1 Percentages", 
  "A2 Averages", 
  "A2 Percentages", 
  "A3 Averages",    
  "A3 Percentages", 
  "A4 Averages", 
  "A4 Percentages", 
  "A5 Averages", 
  "A5 Percentages", 
  "A6 Averages", 
  "A6 Percentages", 
  "A7 Averages", 
  "A7 Percentages", 
  "A8 Averages",     
  "A8 Percentages", 
  "A9 Averages", 
  "A9 Percentages", 
  "A10 Averages", 
  "A10 Percentages", 
  "A11 Averages", 
  "A11 Percentages", 
  "B1 Medians", 
  "C1 Averages", 
  "C1 Percentages", 
  "C2 Averages", 
  "C2 Medians", 
  "C3 Averages",     
  "C3 Medians", 
  "C3 Percentages", 
  "C4 Medians", 
  "C5 Medians", 
  "C6 Medians", 
  "C7 Medians" 
), functionSelection="loadDatalab1", subtableSelection="SECTOR3")

#### End #### 

#### Distributions (loadDatalab2) ####

DIST.SECTOR3 <- runAllFunctions(foldersForProcessing=c(
  "B2 Percentages",  
  "D1 Percentages", 
  "D2 Percentages", 
  "D3 Percentages", 
  "D4 Percentages", 
  "D5 Percentages", 
  "D6 Percentages", 
  "D7 Percentages", 
  "D8 Percentages", 
  "D9 Percentages", 
  "D10 Percentages"
), functionSelection="loadDatalab2", subtableSelection="SECTOR3")

#### End #### 

################################################
#### Disaggregated by INSTSTAT: Four-years  ####
################################################

#### Averages, Medians, Percents (loadDatalab1) ####

AMP.INSTSTAT.4Y <- runAllFunctions(foldersForProcessing=c(
  "E1 Averages",
  "E1 Percentages",
  "E2 Averages",
  "E2 Percentages",
  "E3 Medians",
  "E4 Medians",
  "E5 Medians"
), functionSelection="loadDatalab1", subtableSelection="INSTSTAT")

#### End #### 

################################################
#### Disaggregated by INSTSTAT: Two-years   ####
################################################

#### Averages, Medians, Percents (loadDatalab1) ####

AMP.INSTSTAT.2Y <- runAllFunctions(foldersForProcessing=c(
  "F1 Averages", 
  "F1 Percentages", 
  "F2 Averages",
  "F2 Percentages", 
  "F3 Medians", 
  "F4 Medians", 
  "F5 Medians"
), functionSelection="loadDatalab1", subtableSelection="INSTSTAT")

#### End #### 

################################################
#### Store data files                       ####
################################################

#### Small adjustments ####

AMP.INSTSTAT.2Y <- AMP.INSTSTAT.2Y %>% rename(`State` = `Sector value`)
AMP.INSTSTAT.4Y <- AMP.INSTSTAT.4Y %>% rename(`State` = `Sector value`)

#### End #### 

#### Retitle row names ####

AMP.SECTOR3 <- AMP.SECTOR3 %>% mutate(
  `Row name` = ifelse(`Row name`=="Expected Family Contribution", "Zero EFC Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Federal Pell Grant", "Pell Recipient Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Historical black college indicator", "Institution HBCU Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Income quartile (administrative collection)", "Income Quartile", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Minority Serving Institution indicator", "Institution MSI Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="NPSAS institution state", "Institution State", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Parents' highest education level (administrative collection)", "Parents' Highest Education Level", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Race/ethnicity (with multiple)", "Race/Ethnicity", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Selectivity (All 4-year institutions)", "Institution Selectivity", `Row name`)
) %>% mutate(
  `Sector value` = gsub("NPSAS institution sector - 3 categories - ", "", `Sector value`)
)

DIST.SECTOR3 <- DIST.SECTOR3 %>% mutate(
  `Row name` = ifelse(`Row name`=="Expected Family Contribution", "Zero EFC Status", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="Expected Family Contribution", "Zero EFC Status", `Distribution name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Federal Pell Grant", "Pell Recipient Status", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="Federal Pell Grant", "Pell Recipient Status", `Distribution name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Historical black college indicator", "Institution HBCU Status", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="Historical black college indicator", "Institution HBCU Status", `Distribution name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Income quartile (administrative collection)", "Income Quartile", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="Income quartile (administrative collection)", "Income Quartile", `Distribution name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Minority Serving Institution indicator", "Institution MSI Status", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="Minority Serving Institution indicator", "Institution MSI Status", `Distribution name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="NPSAS institution state", "Institution State", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="NPSAS institution state", "Institution State", `Distribution name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Parents' highest education level (administrative collection)", "Parents' Highest Education Level", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="Parents' highest education level (administrative collection)", "Parents' Highest Education Level", `Distribution name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Race/ethnicity (with multiple)", "Race/Ethnicity", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="Race/ethnicity (with multiple)", "Race/Ethnicity", `Distribution name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Selectivity (All 4-year institutions)", "Institution Selectivity", `Row name`), 
  `Distribution name` = ifelse(`Distribution name`=="Selectivity (All 4-year institutions)", "Institution Selectivity", `Distribution name`)
) %>% mutate(
  `Distribution name` = ifelse(`Distribution name`=="Grade point average in high school (dependent students)", "High School GPA", `Distribution name`)
) %>% mutate(
  `Sector value` = gsub("NPSAS institution sector - 3 categories - ", "", `Sector value`)
)

AMP.INSTSTAT.2Y <- AMP.INSTSTAT.2Y %>% mutate(
  `Row name` = ifelse(`Row name`=="Expected Family Contribution", "Zero EFC Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Federal Pell Grant", "Pell Recipient Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Historical black college indicator", "Institution HBCU Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Income quartile (administrative collection)", "Income Quartile", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Minority Serving Institution indicator", "Institution MSI Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="NPSAS institution state", "Institution State", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Parents' highest education level (administrative collection)", "Parents' Highest Education Level", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Race/ethnicity (with multiple)", "Race/Ethnicity", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Selectivity (All 4-year institutions)", "Institution Selectivity", `Row name`)
) %>% mutate(
  `State` = gsub("NPSAS institution state - ", "", `State`)
)

AMP.INSTSTAT.4Y <- AMP.INSTSTAT.4Y %>% mutate(
  `Row name` = ifelse(`Row name`=="Expected Family Contribution", "Zero EFC Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Federal Pell Grant", "Pell Recipient Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Historical black college indicator", "Institution HBCU Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Income quartile (administrative collection)", "Income Quartile", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Minority Serving Institution indicator", "Institution MSI Status", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="NPSAS institution state", "Institution State", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Parents' highest education level (administrative collection)", "Parents' Highest Education Level", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Race/ethnicity (with multiple)", "Race/Ethnicity", `Row name`)
) %>% mutate(
  `Row name` = ifelse(`Row name`=="Selectivity (All 4-year institutions)", "Institution Selectivity", `Row name`)
) %>% mutate(
  `State` = gsub("NPSAS institution state - ", "", `State`)
)

#### End #### 

#### Rename row values ####

rowValueRename <- AMP.SECTOR3 %>% mutate(`Count` = rep(1))
rowValueRename <- aggregate(
  data=rowValueRename, `Count` ~ `Row value` + `Row name`, FUN=sum
) %>% select(
  -(`Count`)
)
rowValueRename <- rowValueRename %>% mutate(
  `New value` = `Row value`
) %>% mutate(
  `New value` = ifelse((`Row value`=="Yes") & (`Row name`=="Institution HBCU Status"), "HBCUs", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="No") & (`Row name`=="Institution HBCU Status"), "Non-HBCUs", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="American Indian/Alaska Native-serving") & (`Row name`=="Institution MSI Status"), "Tribal college", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="Asian/Native Hawaiian/Pacific Islander-serving") & (`Row name`=="Institution MSI Status"), "AAPISI", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="Black/African American-serving, non-HBCU") & (`Row name`=="Institution MSI Status"), "PBI", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="Hispanic/Latino-serving") & (`Row name`=="Institution MSI Status"), "HSI", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="Non-minority serving") & (`Row name`=="Institution MSI Status"), "Not an MSI", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="Other minority-serving") & (`Row name`=="Institution MSI Status"), "Other MSI", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="0 <= X <= 0") & (`Row name`=="Pell Recipient Status"), "Not a Pell recipient", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="1 <= X <= 9293") & (`Row name`=="Pell Recipient Status"), "Pell recipient", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="American Indian or Alaska Native") & (`Row name`=="Race/Ethnicity"), "Native American", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="Native Hawaiian/other Pacific Islander") & (`Row name`=="Race/Ethnicity"), "Native Hawaiian/Pacific Islander", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="0 <= X <= 0") & (`Row name`=="Zero EFC Status"), "Zero EFC", `New value`)
) %>% mutate(
  `New value` = ifelse((`Row value`=="1 <= X <= 1000000") & (`Row name`=="Zero EFC Status"), "Nonzero EFC", `New value`)
) 

AMP.SECTOR3 <- left_join(
  x=AMP.SECTOR3, y=rowValueRename, by=c("Row value", "Row name")
) %>% select(
  -(`Row value`)
) %>% rename(
  `Row value` = `New value`
)

AMP.INSTSTAT.2Y <- left_join(
  x=AMP.INSTSTAT.2Y, y=rowValueRename, by=c("Row value", "Row name")
) %>% select(
  -(`Row value`)
) %>% rename(
  `Row value` = `New value`
)

AMP.INSTSTAT.4Y <- left_join(
  x=AMP.INSTSTAT.4Y, y=rowValueRename, by=c("Row value", "Row name")
) %>% select(
  -(`Row value`)
) %>% rename(
  `Row value` = `New value`
)

DIST.SECTOR3 <- left_join(
  x=DIST.SECTOR3, y=rowValueRename, by=c("Row value", "Row name")
) %>% select(
  -(`Row value`)
) %>% rename(
  `Row value` = `New value`
)

#### End #### 

#### Rename category names for distributions ####

categoryNameRename <- rowValueRename %>% rename(
  `Category name` = `Row value`, 
  `Distribution name` = `Row name`
) %>% add_row(
  `Category name`="0.5-0.9 (D- to D)", `Distribution name`="High School GPA", `New value`="0.5 to 0.9"
) %>% add_row(
  `Category name`="1.0-1.4 (D to C-)", `Distribution name`="High School GPA", `New value`="1.0 to 1.4"
) %>% add_row(
  `Category name`="1.5-1.9 (C- to C)", `Distribution name`="High School GPA", `New value`="1.5 to 1.9"
) %>% add_row(
  `Category name`="2.0-2.4 (C to B-)", `Distribution name`="High School GPA", `New value`="2.0 to 2.4"
) %>% add_row(
  `Category name`="2.5-2.9 (B- to B)", `Distribution name`="High School GPA", `New value`="2.5 to 2.9"
) %>% add_row(
  `Category name`="3.0-3.4 (B to A-)", `Distribution name`="High School GPA", `New value`="3.0 to 3.4"
) %>% add_row(
  `Category name`="3.5-4.0 (A- to A)", `Distribution name`="High School GPA", `New value`="3.5 to 4.0"
)

DIST.SECTOR3 <- left_join(
  x=DIST.SECTOR3, y=categoryNameRename, by=c("Distribution name", "Category name")
) %>% select(
  -(`Category name`)
) %>% rename(
  `Category name` = `New value`
)

rm(categoryNameRename, rowValueRename)

#### End #### 

#### Set bounds ####

suppressWarnings({
  
  AMP.SECTOR3 <- AMP.SECTOR3 %>% separate(
    `Target confidence interval`, c("Lower bound", "Upper bound"), " - "
  ) %>% mutate(
    `Target value` = gsub(" !!", "", `Target value`), 
    `Target value` = gsub(" !", "", `Target value`), 
    `Target value` = gsub(",", "", `Target value`), 
    `Lower bound` = gsub(" !!", "", `Lower bound`), 
    `Lower bound` = gsub(" !", "", `Lower bound`), 
    `Lower bound` = gsub(",", "", `Lower bound`), 
    `Upper bound` = gsub(" !!", "", `Upper bound`), 
    `Upper bound` = gsub(" !", "", `Upper bound`),
    `Upper bound` = gsub(",", "", `Upper bound`)
  ) %>% mutate(
    `Target value` = as.numeric(`Target value`), 
    `Lower bound` = as.numeric(`Lower bound`), 
    `Upper bound` = as.numeric(`Upper bound`)
  )
  
  AMP.INSTSTAT.2Y <- AMP.INSTSTAT.2Y %>% separate(
    `Target confidence interval`, c("Lower bound", "Upper bound"), " - "
  ) %>% mutate(
    `Target value` = gsub(" !!", "", `Target value`), 
    `Target value` = gsub(" !", "", `Target value`), 
    `Target value` = gsub(",", "", `Target value`), 
    `Lower bound` = gsub(" !!", "", `Lower bound`), 
    `Lower bound` = gsub(" !", "", `Lower bound`), 
    `Lower bound` = gsub(",", "", `Lower bound`), 
    `Upper bound` = gsub(" !!", "", `Upper bound`), 
    `Upper bound` = gsub(" !", "", `Upper bound`),
    `Upper bound` = gsub(",", "", `Upper bound`)
  ) %>% mutate(
    `Target value` = as.numeric(`Target value`), 
    `Lower bound` = as.numeric(`Lower bound`), 
    `Upper bound` = as.numeric(`Upper bound`)
  )
  
  AMP.INSTSTAT.4Y <- AMP.INSTSTAT.4Y %>% separate(
    `Target confidence interval`, c("Lower bound", "Upper bound"), " - "
  ) %>% mutate(
    `Target value` = gsub(" !!", "", `Target value`), 
    `Target value` = gsub(" !", "", `Target value`), 
    `Target value` = gsub(",", "", `Target value`), 
    `Lower bound` = gsub(" !!", "", `Lower bound`), 
    `Lower bound` = gsub(" !", "", `Lower bound`), 
    `Lower bound` = gsub(",", "", `Lower bound`), 
    `Upper bound` = gsub(" !!", "", `Upper bound`), 
    `Upper bound` = gsub(" !", "", `Upper bound`),
    `Upper bound` = gsub(",", "", `Upper bound`)
  ) %>% mutate(
    `Target value` = as.numeric(`Target value`), 
    `Lower bound` = as.numeric(`Lower bound`), 
    `Upper bound` = as.numeric(`Upper bound`)
  )
  
  DIST.SECTOR3 <- DIST.SECTOR3 %>% separate(
    `Interval`, c("Lower bound", "Upper bound"), " - "
  ) %>% mutate(
    `Share` = gsub(" !!", "", `Share`), 
    `Share` = gsub(" !", "", `Share`), 
    `Share` = gsub(",", "", `Share`), 
    `Lower bound` = gsub(" !!", "", `Lower bound`), 
    `Lower bound` = gsub(" !", "", `Lower bound`), 
    `Lower bound` = gsub(",", "", `Lower bound`), 
    `Upper bound` = gsub(" !!", "", `Upper bound`), 
    `Upper bound` = gsub(" !", "", `Upper bound`),
    `Upper bound` = gsub(",", "", `Upper bound`)
  ) %>% mutate(
    `Share` = as.numeric(`Share`), 
    `Lower bound` = as.numeric(`Lower bound`), 
    `Upper bound` = as.numeric(`Upper bound`)
  )
  
})

minValues <- rbind(
  aggregate(data=AMP.SECTOR3, `Lower bound` ~ `Target name` + `Measure name`, FUN=min), 
  aggregate(data=AMP.INSTSTAT.2Y, `Lower bound` ~ `Target name` + `Measure name`, FUN=min), 
  aggregate(data=AMP.INSTSTAT.4Y, `Lower bound` ~ `Target name` + `Measure name`, FUN=min)
) %>% filter(`Measure name` %in% c("Median", "Average"))

maxValues <- rbind(
  aggregate(data=AMP.SECTOR3, `Upper bound` ~ `Target name` + `Measure name`, FUN=max), 
  aggregate(data=AMP.INSTSTAT.2Y, `Upper bound` ~ `Target name` + `Measure name`, FUN=max), 
  aggregate(data=AMP.INSTSTAT.4Y, `Upper bound` ~ `Target name` + `Measure name`, FUN=max)
) %>% filter(`Measure name` %in% c("Median", "Average"))

minValues <- aggregate(data=minValues, `Lower bound` ~ `Target name`, FUN=min)
maxValues <- aggregate(data=maxValues, `Upper bound` ~ `Target name`, FUN=max)

axisBounds <- full_join(
  x=minValues, y=maxValues, by="Target name"
) %>% rename(
  `Lower bound for axis` = `Lower bound`, 
  `Upper bound for axis` = `Upper bound`
) %>% mutate(
  `Distance between bounds` = `Upper bound for axis` - `Lower bound for axis`
) %>% mutate(
  `Extra margin` = `Distance between bounds` * 0.03
) %>% mutate(
  `Lower bound for axis` = `Lower bound for axis` - `Extra margin`, 
  `Upper bound for axis` = `Upper bound for axis` + `Extra margin`
) %>% select(
  -(`Distance between bounds`), -(`Extra margin`)
)

axisBounds <- axisBounds %>% add_row(
  `Target name` = "Income Quartile", `Lower bound for axis` = -0.03, `Upper bound for axis` = 1.03
) %>% add_row(
  `Target name` = "Zero EFC Status", `Lower bound for axis` = -0.03, `Upper bound for axis` = 1.03
) %>% add_row(
  `Target name` = "Pell Recipient Status", `Lower bound for axis` = -0.03, `Upper bound for axis` = 1.03
) %>% add_row(
  `Target name` = "Parents' Highest Education Level", `Lower bound for axis` = -0.03, `Upper bound for axis` = 1.03
) %>% add_row(
  `Target name` = "Race/Ethnicity", `Lower bound for axis` = -0.03, `Upper bound for axis` = 1.03
) %>% add_row(
  `Target name` = "Institution HBCU Status", `Lower bound for axis` = -0.03, `Upper bound for axis` = 1.03
) %>% add_row(
  `Target name` = "Institution MSI Status", `Lower bound for axis` = -0.03, `Upper bound for axis` = 1.03
) %>% add_row(
  `Target name` = "Institution Selectivity", `Lower bound for axis` = -0.03, `Upper bound for axis` = 1.03
)

rm(minValues, maxValues)

#### End #### 

#### Write files ####

setwd("/Users/peter_granville/Net Price Equity/Financial-Aid-Distributions")

write.csv(AMP.INSTSTAT.2Y, "AMP-INSTSTAT-2Y.csv", row.names=FALSE)
write.csv(AMP.INSTSTAT.4Y, "AMP-INSTSTAT-4Y.csv", row.names=FALSE)
write.csv(AMP.SECTOR3, "AMP-SECTOR3.csv", row.names=FALSE)
write.csv(DIST.SECTOR3, "DIST-SECTOR3.csv", row.names=FALSE)
write.csv(axisBounds, "Axis-Bounds.csv", row.names=FALSE)

setwd("/Users/peter_granville/Net Price Equity")

#### End #### 

################################################
#### Old, no longer needed                  ####
################################################

# #### List unique tables ####
# 
# AMP.SECTOR3 <- AMP.SECTOR3 %>% mutate(
#   `Table ID 1` = paste(`Target name`, sep=" | "),
#   `Table ID 2` = paste(`Target name`, `Row name`, sep=" | "),
#   `Table ID 3` = paste(`Target name`, `Row name`, `Measure name`, sep=" | "),
#   `Table ID 4` = paste(`Target name`, `Row name`, `Measure name`, `Sector value`, sep=" | ")
# )
# length(unique(AMP.SECTOR3$`Table ID 1`))
# length(unique(AMP.SECTOR3$`Table ID 2`))
# length(unique(AMP.SECTOR3$`Table ID 3`))
# length(unique(AMP.SECTOR3$`Table ID 4`))
# 
# DIST.SECTOR3 <- DIST.SECTOR3 %>% mutate(
#   `Table ID 1` = paste(`Distribution name`, sep=" | "),
#   `Table ID 2` = paste(`Distribution name`, `Row name`, sep=" | "),
#   `Table ID 3` = paste(`Distribution name`, `Row name`, `Sector value`, sep=" | ")
# )
# length(unique(DIST.SECTOR3$`Table ID 1`))
# length(unique(DIST.SECTOR3$`Table ID 2`))
# length(unique(DIST.SECTOR3$`Table ID 3`))
# 
# AMP.INSTSTAT.2Y <- AMP.INSTSTAT.2Y %>% mutate(
#   `Table ID 1` = paste(`Target name`, sep=" | "),
#   `Table ID 2` = paste(`Target name`, `Row name`, sep=" | "),
#   `Table ID 3` = paste(`Target name`, `Row name`, `Measure name`, sep=" | "),
#   `Table ID 4` = paste(`Target name`, `Row name`, `Measure name`, `State`, sep=" | ")
# )
# length(unique(AMP.INSTSTAT.2Y$`Table ID 1`))
# length(unique(AMP.INSTSTAT.2Y$`Table ID 2`))
# length(unique(AMP.INSTSTAT.2Y$`Table ID 3`))
# length(unique(AMP.INSTSTAT.2Y$`Table ID 4`))
# 
# AMP.INSTSTAT.4Y <- AMP.INSTSTAT.4Y %>% mutate(
#   `Table ID 1` = paste(`Target name`, sep=" | "),
#   `Table ID 2` = paste(`Target name`, `Row name`, sep=" | "),
#   `Table ID 3` = paste(`Target name`, `Row name`, `Measure name`, sep=" | "),
#   `Table ID 4` = paste(`Target name`, `Row name`, `Measure name`, `State`, sep=" | ")
# )
# length(unique(AMP.INSTSTAT.4Y$`Table ID 1`))
# length(unique(AMP.INSTSTAT.4Y$`Table ID 2`))
# length(unique(AMP.INSTSTAT.4Y$`Table ID 3`))
# length(unique(AMP.INSTSTAT.4Y$`Table ID 4`))
# 
# #### End #### 
# 
# #### Create factor lists ####
# 
# levels.SectorValue <- c(
#   "Total", 
#   "Public 4-year",
#   "Public 2-year", 
#   "Other"
# )
# 
# levels.TargetName <- c(
#   "Federal Pell Grant",
#   "Federal campus-based aid (SEOG, FWS)",
#   "Title IV loans (includes Parent PLUS Loans)",
#   "State grants total",
#   "Institution grants total",
#   "Private source grants",
#   "State non-need & merit grants",
#   "State need-based grants",
#   "Institution non-need & merit grants",
#   "Institutional need-based grants",
#   "Total state and institutional grants",
#   "Grant amount exceeding federal need",
#   "Tuition and fees minus all grants",
#   "Net tuition after all grants as percent of income",
#   "Student budget minus all grants",
#   "Net price after grants as percent of income",
#   "Expected Family Contribution",
#   "Tuition and fees paid",
#   "Student budget (attendance adjusted)"
# )
# 
# levels.RowName <- c(
#   "Overall",
#   "Income Quartile", 
#   "Zero EFC Status", 
#   "Pell Recipient Status", 
#   "Parents' Highest Education Level", 
#   "Race/Ethnicity", 
#   "Institution HBCU Status", 
#   "Institution MSI Status", 
#   "Institution Selectivity", 
#   "Institution State"
# )
# 
# levels.MeasureName <- c(
#   "Share >0", 
#   "Average", 
#   "Median"
# )
# 
# levels.RowValue <- c(
#   
#   "Total",
#   
#   # Income Quartile
#   "Top quartile",
#   "Upper-middle quartile",
#   "Lower-middle quartile",
#   "Bottom quartile",
#   
#   # Institution HBCU Status
#   "HBCUs",
#   "Non-HBCUs",
#   
#   # Institution MSI Status
#   "AAPISI",
#   "HBCU",
#   "HSI",
#   "PBI",
#   "Tribal college",
#   "Other MSI",
#   "Not an MSI",
#   
#   # Institution Selectivity
#   "Very selective",
#   "Moderately selective",  
#   "Minimally selective",
#   "Open admission",
#   "Not a 4-year institution",
#   
#   # Institution State
#   "Alabama",
#   "Alaska",
#   "Arizona",
#   "Arkansas",
#   "California",
#   "Colorado",
#   "Connecticut",
#   "Delaware",
#   "District of Columbia",
#   "Florida",
#   "Georgia",
#   "Hawaii",
#   "Idaho",
#   "Illinois",
#   "Indiana",
#   "Iowa",
#   "Kansas",
#   "Kentucky",
#   "Louisiana",
#   "Maine",
#   "Maryland",
#   "Massachusetts",
#   "Michigan",
#   "Minnesota",
#   "Mississippi",
#   "Missouri",
#   "Montana",
#   "Nebraska",
#   "Nevada",
#   "New Hampshire",
#   "New Jersey",
#   "New Mexico",
#   "New York",
#   "North Carolina",
#   "North Dakota",
#   "Ohio",
#   "Oklahoma",
#   "Oregon",
#   "Pennsylvania",
#   "Puerto Rico",
#   "Rhode Island",
#   "South Carolina",
#   "South Dakota",
#   "Tennessee",
#   "Texas",
#   "Utah",
#   "Vermont",
#   "Virginia",
#   "Washington",
#   "West Virginia",
#   "Wisconsin",
#   "Wyoming",
#   
#   # Parents' Highest Education Level
#   "College or beyond",
#   "High school",
#   "Middle school/junior high",
#   
#   # Pell Recipient Status
#   "Pell recipient",
#   "Not a Pell recipient",
#   
#   # Race/Ethnicity
#   "White",
#   "Hispanic or Latino",
#   "Black or African American",
#   "Asian",
#   "Native American",
#   "Native Hawaiian/Pacific Islander",
#   "More than one race",
#   
#   # Zero EFC Status
#   "Zero EFC",
#   "Nonzero EFC"
# )
# 
# levels.DistributionName <- c(
#   "Income Quartile", 
#   "Zero EFC Status", 
#   "Pell Recipient Status", 
#   "Parents' Highest Education Level", 
#   "Race/Ethnicity", 
#   "High School GPA",
#   "Institution HBCU Status", 
#   "Institution MSI Status", 
#   "Institution Selectivity" 
# )
# 
# levels.CategoryName <- c(
#   levels.RowValue, c(
#     # High School GPA
#     "0.5 to 0.9", 
#     "1.0 to 1.4", 
#     "1.5 to 1.9", 
#     "2.0 to 2.4", 
#     "2.5 to 2.9", 
#     "3.0 to 3.4", 
#     "3.5 to 4.0"
#   )
# )
# 
# levels.State <- c(
#   "Total",
#   "Alabama",
#   "Alaska",
#   "Arizona",
#   "Arkansas",
#   "California",
#   "Colorado",
#   "Connecticut",
#   "Delaware",
#   "District of Columbia",
#   "Florida",
#   "Georgia",
#   "Hawaii",
#   "Idaho",
#   "Illinois",
#   "Indiana",
#   "Iowa",
#   "Kansas",
#   "Kentucky",
#   "Louisiana",
#   "Maine",
#   "Maryland",
#   "Massachusetts",
#   "Michigan",
#   "Minnesota",
#   "Mississippi",
#   "Missouri",
#   "Montana",
#   "Nebraska",
#   "Nevada",
#   "New Hampshire",
#   "New Jersey",
#   "New Mexico",
#   "New York",
#   "North Carolina",
#   "North Dakota",
#   "Ohio",
#   "Oklahoma",
#   "Oregon",
#   "Pennsylvania",
#   "Puerto Rico",
#   "Rhode Island",
#   "South Carolina",
#   "South Dakota",
#   "Tennessee",
#   "Texas",
#   "Utah",
#   "Vermont",
#   "Virginia",
#   "Washington",
#   "West Virginia",
#   "Wisconsin",
#   "Wyoming"
# )
# 
# #### End #### 
# 
# #### Assign order of tables ####
# 
# AMP.SECTOR3 <- AMP.SECTOR3 %>% mutate(
#   `Target name` = as.factor(`Target name`), 
#   `Row name` = as.factor(`Row name`), 
#   `Measure name` = as.factor(`Measure name`), 
#   `Sector value` = as.factor(`Sector value`),
#   `Row value` = as.factor(`Row value`)
# ) %>% mutate(
#   `Target name` = factor(`Target name`, levels=levels.TargetName), 
#   `Row name` = factor(`Row name`, levels=levels.RowName), 
#   `Measure name` = factor(`Measure name`, levels=levels.MeasureName), 
#   `Sector value` = factor(`Sector value`, levels=levels.SectorValue),
#   `Row value` = factor(`Row value`, levels=levels.RowValue)
# ) %>% arrange(
#   `Target name`, 
#   `Row name`, 
#   `Measure name`, 
#   `Sector value`,
#   `Row value`
# )
# 
# DIST.SECTOR3 <- DIST.SECTOR3 %>% mutate(
#   `Distribution name` = as.factor(`Distribution name`), 
#   `Row name` = as.factor(`Row name`), 
#   `Sector value` = as.factor(`Sector value`),
#   `Category name` = as.factor(`Category name`), 
#   `Row value` = as.factor(`Row value`)
# ) %>% mutate(
#   `Distribution name` = factor(`Distribution name`, levels=levels.DistributionName), 
#   `Row name` = factor(`Row name`, levels=levels.RowName), 
#   `Sector value` = factor(`Sector value`, levels=levels.SectorValue),
#   `Category name` = factor(`Category name`, levels=levels.CategoryName), 
#   `Row value` = factor(`Row value`, levels=levels.RowValue)
# ) %>% arrange(
#   `Distribution name`, 
#   `Row name`, 
#   `Sector value`,
#   `Category name`, 
#   `Row value`
# )
# 
# AMP.INSTSTAT.2Y <- AMP.INSTSTAT.2Y %>% mutate(
#   `Target name` = as.factor(`Target name`), 
#   `Row name` = as.factor(`Row name`), 
#   `Measure name` = as.factor(`Measure name`), 
#   `State` = as.factor(`State`),
#   `Row value` = as.factor(`Row value`)
# ) %>% mutate(
#   `Target name` = factor(`Target name`, levels=levels.TargetName), 
#   `Row name` = factor(`Row name`, levels=levels.RowName), 
#   `Measure name` = factor(`Measure name`, levels=levels.MeasureName), 
#   `State` = factor(`State`, levels=levels.State),
#   `Row value` = factor(`Row value`, levels=levels.RowValue)
# ) %>% arrange(
#   `Target name`, 
#   `Row name`, 
#   `Measure name`, 
#   `State`,
#   `Row value`
# )
# 
# AMP.INSTSTAT.4Y <- AMP.INSTSTAT.4Y %>% mutate(
#   `Target name` = as.factor(`Target name`), 
#   `Row name` = as.factor(`Row name`), 
#   `Measure name` = as.factor(`Measure name`), 
#   `State` = as.factor(`State`),
#   `Row value` = as.factor(`Row value`)
# ) %>% mutate(
#   `Target name` = factor(`Target name`, levels=levels.TargetName), 
#   `Row name` = factor(`Row name`, levels=levels.RowName), 
#   `Measure name` = factor(`Measure name`, levels=levels.MeasureName), 
#   `State` = factor(`State`, levels=levels.State),
#   `Row value` = factor(`State`, levels=levels.RowValue)
# ) %>% arrange(
#   `Target name`, 
#   `Row name`, 
#   `Measure name`, 
#   `State`,
#   `Row value`
# )
# 
# #### End #### 
# 
# #### Print table IDs ####
# 
# # output1 <- AMP.SECTOR3 %>% filter(
# #   duplicated(`Table ID 3`)==FALSE
# # )
# # output2 <- DIST.SECTOR3 %>% filter(
# #   duplicated(`Table ID 2`)==FALSE
# # )
# # output3 <- AMP.INSTSTAT.2Y %>% filter(
# #   duplicated(`Table ID 3`)==FALSE
# # )
# # output4 <- AMP.INSTSTAT.4Y %>% filter(
# #   duplicated(`Table ID 3`)==FALSE
# # )
# # 
# # write.csv(output1, "output1.csv", row.names=FALSE)
# # write.csv(output2, "output2.csv", row.names=FALSE)
# # write.csv(output3, "output3.csv", row.names=FALSE)
# # write.csv(output4, "output4.csv", row.names=FALSE)
# 
# #### End #### 






