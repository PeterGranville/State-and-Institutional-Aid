
#### Setup ####

library(readxl)
library(scales)
library(tidyverse)
library(RColorBrewer)

setwd("/Users/peter_granville/Net Price Equity/IPEDS Data")

#### End #### 

##########################################
#### NPSAS Data 1                     ####
##########################################

#### Write function to process data ####

processDatalab <- function(
    filename, 
    startRow, 
    endRow, 
    keepWhichCol, 
    rowVarName, 
    colVarName, 
    addlVar, 
    addlVarValue, 
    addlVarName
){
  
  tempDF <- read.csv(
    filename, 
    header=FALSE, 
    skip = startRow-1, 
    nrows = endRow - startRow + 1
  ) 
  
  if(keepWhichCol==1){
    tempDF <- tempDF %>% select(`V1`, `V2`)
  }
  if(keepWhichCol==2){
    tempDF <- tempDF %>% select(`V1`, `V3`)
  }
  
  names(tempDF)[1] <- rowVarName
  names(tempDF)[2] <- colVarName

  if(addlVar==TRUE){
    tempDF <- tempDF %>% mutate(
      `Additional variable` = rep(addlVarValue)
    )
    names(tempDF)[3] <- addlVarName
  }
  
  return(tempDF)
  
}

#### End #### 

#### Figure 2 ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_fvxsws.csv", 
    startRow = 12, 
    endRow = 15, 
    keepWhichCol = 1, 
    rowVarName = "Income quartile", 
    colVarName = "Net price after grants as a share of income", 
    addlVar = FALSE, 
    addlVarValue = "NA", 
    addlVarName = "NA"
  )
) %>% mutate(
  `Net price after grants as a share of income` = `Net price after grants as a share of income`/ 100
) %>% mutate(
  `Income quartile` = factor(`Income quartile`, levels=c(
    "Bottom quartile", 
    "Lower-middle quartile", 
    "Upper-middle quartile", 
    "Top quartile"
  ))
) %>% mutate(
  `For Label` = percent(`Net price after grants as a share of income`, accuracy=1)
) %>% mutate(
  `For Label`=ifelse(
    `For Label`=="100%", "100+%", `For Label`
  )
)

ggplot(
  data=forPlot, 
  mapping=aes(
    x=`Net price after grants as a share of income`, y=`Income quartile`
  )
) + geom_bar(
  stat="identity", 
  width=0.3
) + scale_x_continuous(
  labels=percent_format(accuracy=1), 
  limits=c(0, 1.01), 
  breaks=c(0, 0.5, 1)
) + scale_fill_brewer(
  palette = "Dark2"
) + geom_text(
  aes(label = `For Label`), 
  nudge_x = 0.05
) + theme(text = element_text(size = 13)) + scale_y_discrete(labels = label_wrap(12))

#### End #### 

#### Figure 4 ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_xrtupd.csv", 
    startRow = 14, 
    endRow = 17, 
    keepWhichCol = 2, 
    rowVarName = "Income quartile", 
    colVarName = "Share with grants exceeding need", 
    addlVar = FALSE, 
    addlVarValue = "NA", 
    addlVarName = "NA"
  )
) %>% mutate(
  `Share with grants exceeding need` = `Share with grants exceeding need`/ 100
) %>% mutate(
  `Income quartile` = factor(`Income quartile`, levels=c(
    "Bottom quartile", 
    "Lower-middle quartile", 
    "Upper-middle quartile", 
    "Top quartile"
  ))
) %>% mutate(
  `For Label` = percent(`Share with grants exceeding need`, accuracy=0.1)
)

ggplot(
  data=forPlot, 
  mapping=aes(
    x=`Share with grants exceeding need`, y=`Income quartile`
  )
) + geom_bar(
  stat="identity", 
  width=0.3
) + scale_x_continuous(
  labels=percent_format(accuracy=1), 
  limits=c(0, 1), 
  breaks=c(0, 0.5, 1)
) + scale_fill_brewer(
  palette = "Dark2"
) + geom_text(
  aes(label = `For Label`), 
  nudge_x = 0.05
) + theme(text = element_text(size = 13)) + scale_y_discrete(labels = label_wrap(12))

#### End #### 

#### Figure 5 ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

forPlot1 <- rbind(
  processDatalab(
    filename = "PowerStats_egctjm.csv", 
    startRow = 53, 
    endRow = 54, 
    keepWhichCol = 2, 
    rowVarName = "Pell recipient status", 
    colVarName = "Share receiving grants", 
    addlVar = TRUE, 
    addlVarValue = "Public 2-year", 
    addlVarName = "Sector"
  ), 
  processDatalab(
    filename = "PowerStats_egctjm.csv", 
    startRow = 92, 
    endRow = 93, 
    keepWhichCol = 2, 
    rowVarName = "Pell recipient status", 
    colVarName = "Share receiving grants", 
    addlVar = TRUE, 
    addlVarValue = "Public 4-year", 
    addlVarName = "Sector"
  )
) %>% mutate(
  `Grant type` = rep("State non-need/ merit-based grants")
) 

forPlot2 <- rbind(
  processDatalab(
    filename = "PowerStats_lfuxgh.csv", 
    startRow = 53, 
    endRow = 54, 
    keepWhichCol = 2, 
    rowVarName = "Pell recipient status", 
    colVarName = "Share receiving grants", 
    addlVar = TRUE, 
    addlVarValue = "Public 2-year", 
    addlVarName = "Sector"
  ), 
  processDatalab(
    filename = "PowerStats_lfuxgh.csv", 
    startRow = 92, 
    endRow = 93, 
    keepWhichCol = 2, 
    rowVarName = "Pell recipient status", 
    colVarName = "Share receiving grants", 
    addlVar = TRUE, 
    addlVarValue = "Public 4-year", 
    addlVarName = "Sector"
  )
) %>% mutate(
  `Grant type` = rep("State need-based grants")
) 

forPlot <- rbind(
  forPlot1, forPlot2
) %>% mutate(
  `Pell recipient status` = ifelse(
    `Pell recipient status`=="0 <= X <= 0", "Non-recipient", "Pell recipient"
  )
) %>% mutate(
  `Pell recipient status` = factor(`Pell recipient status`, levels=c(
    "Non-recipient", 
    "Pell recipient"
  ))
) %>% mutate(
  `Share receiving grants` = `Share receiving grants` / 100
) %>% mutate(
  `For Label` = percent(`Share receiving grants`, accuracy=0.1)
)
rm(forPlot1, forPlot2)

ggplot(
  data=forPlot, 
  mapping=aes(
    x=`Share receiving grants`, y=`Grant type`, fill=`Pell recipient status`
  )
) + geom_bar(
  stat="identity",
  position=position_dodge(width=0.55),
  width=0.3, 
) + scale_x_continuous(
  labels=percent_format(accuracy=1), 
  limits=c(0, 1), 
  breaks=c(0, 0.5, 1)
) + scale_fill_brewer(
  palette = "Dark2", 
  guide = guide_legend(reverse = TRUE)
) + facet_wrap(
  `Sector` ~ ., ncol=1, strip.position=c("top")
) + geom_text(
  aes(label = `For Label`), 
  position=position_dodge(width=0.55), 
  hjust=-0.2
) + theme(text = element_text(size = 13)) + scale_y_discrete(labels = label_wrap(20))

#### End #### 

#### Figure 7 ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_pmtxyr.csv", 
    startRow = 94, 
    endRow = 95, 
    keepWhichCol = 1, 
    rowVarName = "In-state status", 
    colVarName = "Average institutional grant", 
    addlVar = TRUE, 
    addlVarValue = "Very selective", 
    addlVarName = "Institutional selectivity"
  ), 
  processDatalab(
    filename = "PowerStats_pmtxyr.csv", 
    startRow = 135, 
    endRow = 136, 
    keepWhichCol = 1, 
    rowVarName = "In-state status", 
    colVarName = "Average institutional grant", 
    addlVar = TRUE, 
    addlVarValue = "Moderately selective", 
    addlVarName = "Institutional selectivity"
  ), 
  processDatalab(
    filename = "PowerStats_pmtxyr.csv", 
    startRow = 176, 
    endRow = 177, 
    keepWhichCol = 1, 
    rowVarName = "In-state status", 
    colVarName = "Average institutional grant", 
    addlVar = TRUE, 
    addlVarValue = "Minimally selective", 
    addlVarName = "Institutional selectivity"
  ), 
  processDatalab(
    filename = "PowerStats_pmtxyr.csv", 
    startRow = 217, 
    endRow = 218, 
    keepWhichCol = 1, 
    rowVarName = "In-state status", 
    colVarName = "Average institutional grant", 
    addlVar = TRUE, 
    addlVarValue = "Open admission", 
    addlVarName = "Institutional selectivity"
  )
) %>% mutate(
  `Institutional selectivity` = factor(`Institutional selectivity`, levels=c(
    "Open admission", 
    "Minimally selective", 
    "Moderately selective", 
    "Very selective"
  ))
) %>% mutate(
  `In-state status` = ifelse(
    `In-state status`=="Yes", "In-state", "Out-of-state" 
  )
) %>% mutate(
  `For Label` = dollar(`Average institutional grant`, accuracy=1)
)

ggplot(
  data=forPlot, 
  mapping=aes(
    y=`Average institutional grant`, x=`Institutional selectivity`, fill=`In-state status`
  )
) + geom_bar(
  stat="identity",
  position=position_dodge(width=0.55),
  width=0.3
) + scale_y_continuous(
  labels=dollar_format(accuracy=1), 
  limits=c(0, 9500)
) + scale_fill_brewer(
  palette = "Dark2"
) + geom_text(
  aes(label = `For Label`), 
  position=position_dodge(width=0.55), 
  vjust=-0.5
) + theme(text = element_text(size = 13))

#### End #### 

##########################################
#### NPSAS Data 2                     ####
##########################################

#### Write function to load data ####

loadNPSAS <- function(
    filename, 
    startRow, 
    endRow, 
    inState, 
    selectivity
){
  
  tempDF <- read.csv(
    filename, 
    header=FALSE, 
    skip=startRow - 1, 
    nrows=endRow - startRow + 1
  ) %>% rename(
    `Poverty bracket` = `V1`, 
    `Average Pell Grant` = `V2`, 
    `Average state grant` = `V3`, 
    `Average institutional grant` = `V4` 
  ) %>% mutate(
    `In-state status` = rep(inState), 
    `Selectivity` = rep(selectivity)
  )
  
  return(tempDF)
  
}

#### End #### 

#### Load data ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

data1 <- rbind(
  
  # Overall
  loadNPSAS(
    filename = "PowerStats_fawbhe.csv", 
    startRow = 12, 
    endRow = 36, 
    inState = "Overall", 
    selectivity = "Overall"
  ), 
  loadNPSAS(
    filename = "PowerStats_fawbhe.csv", 
    startRow = 314, 
    endRow = 338, 
    inState = "Overall", 
    selectivity = "Very selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_fawbhe.csv", 
    startRow = 465, 
    endRow = 489, 
    inState = "Overall", 
    selectivity = "Moderately selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_fawbhe.csv", 
    startRow = 616, 
    endRow = 640, 
    inState = "Overall", 
    selectivity = "Minimally selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_fawbhe.csv", 
    startRow = 767, 
    endRow = 791, 
    inState = "Overall", 
    selectivity = "Open admission"
  ),
  
  # In-state
  loadNPSAS(
    filename = "PowerStats_dcyugz.csv", 
    startRow = 12, 
    endRow = 36, 
    inState = "In-state", 
    selectivity = "Overall"
  ), 
  loadNPSAS(
    filename = "PowerStats_dcyugz.csv", 
    startRow = 314, 
    endRow = 338, 
    inState = "In-state", 
    selectivity = "Very selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_dcyugz.csv", 
    startRow = 465, 
    endRow = 489, 
    inState = "In-state", 
    selectivity = "Moderately selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_dcyugz.csv", 
    startRow = 616, 
    endRow = 640, 
    inState = "In-state", 
    selectivity = "Minimally selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_dcyugz.csv", 
    startRow = 767, 
    endRow = 791, 
    inState = "In-state", 
    selectivity = "Open admission"
  ), 
  
  # Out-of-state
  loadNPSAS(
    filename = "PowerStats_prmhsm.csv", 
    startRow = 12, 
    endRow = 36, 
    inState = "Out-of-state", 
    selectivity = "Overall"
  ), 
  loadNPSAS(
    filename = "PowerStats_prmhsm.csv", 
    startRow = 314, 
    endRow = 338, 
    inState = "Out-of-state", 
    selectivity = "Very selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_prmhsm.csv", 
    startRow = 465, 
    endRow = 489, 
    inState = "Out-of-state", 
    selectivity = "Moderately selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_prmhsm.csv", 
    startRow = 616, 
    endRow = 640, 
    inState = "Out-of-state", 
    selectivity = "Minimally selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_prmhsm.csv", 
    startRow = 767, 
    endRow = 791, 
    inState = "Out-of-state", 
    selectivity = "Open admission"
  )
)

#### End #### 

#### Format data ####

data1 <- data1 %>% mutate(
  `Average Pell Grant` = gsub(" !!", "", `Average Pell Grant`), 
  `Average state grant` = gsub(" !!", "", `Average state grant`), 
  `Average institutional grant` = gsub(" !!", "", `Average institutional grant`)
) %>% mutate(
  `Average Pell Grant` = gsub(" !", "", `Average Pell Grant`), 
  `Average state grant` = gsub(" !", "", `Average state grant`), 
  `Average institutional grant` = gsub(" !", "", `Average institutional grant`)
) %>% mutate(
  `Average Pell Grant` = gsub("#", "0", `Average Pell Grant`), 
  `Average state grant` = gsub("#", "0", `Average state grant`), 
  `Average institutional grant` = gsub("#", "0", `Average institutional grant`)
) %>% mutate(
  `Average Pell Grant` = as.numeric(`Average Pell Grant`), 
  `Average state grant` = as.numeric(`Average state grant`), 
  `Average institutional grant` = as.numeric(`Average institutional grant`)
)

data1$`Poverty bracket`[data1$`Poverty bracket`=="0 <= X <= 40"] <- "40"
data1$`Poverty bracket`[data1$`Poverty bracket`=="41 <= X <= 80"] <- "80"
data1$`Poverty bracket`[data1$`Poverty bracket`=="81 <= X <= 120"] <- "120"
data1$`Poverty bracket`[data1$`Poverty bracket`=="121 <= X <= 160"] <- "160"
data1$`Poverty bracket`[data1$`Poverty bracket`=="161 <= X <= 200"] <- "200"
data1$`Poverty bracket`[data1$`Poverty bracket`=="201 <= X <= 240"] <- "240"
data1$`Poverty bracket`[data1$`Poverty bracket`=="241 <= X <= 280"] <- "280"
data1$`Poverty bracket`[data1$`Poverty bracket`=="281 <= X <= 320"] <- "320"
data1$`Poverty bracket`[data1$`Poverty bracket`=="321 <= X <= 360"] <- "360"
data1$`Poverty bracket`[data1$`Poverty bracket`=="361 <= X <= 400"] <- "400"
data1$`Poverty bracket`[data1$`Poverty bracket`=="401 <= X <= 440"] <- "440"
data1$`Poverty bracket`[data1$`Poverty bracket`=="441 <= X <= 480"] <- "480"
data1$`Poverty bracket`[data1$`Poverty bracket`=="481 <= X <= 520"] <- "520"
data1$`Poverty bracket`[data1$`Poverty bracket`=="521 <= X <= 560"] <- "560"
data1$`Poverty bracket`[data1$`Poverty bracket`=="561 <= X <= 600"] <- "600"
data1$`Poverty bracket`[data1$`Poverty bracket`=="601 <= X <= 640"] <- "640"
data1$`Poverty bracket`[data1$`Poverty bracket`=="641 <= X <= 680"] <- "680"
data1$`Poverty bracket`[data1$`Poverty bracket`=="681 <= X <= 720"] <- "720"
data1$`Poverty bracket`[data1$`Poverty bracket`=="721 <= X <= 760"] <- "760"
data1$`Poverty bracket`[data1$`Poverty bracket`=="761 <= X <= 800"] <- "800"
data1$`Poverty bracket`[data1$`Poverty bracket`=="801 <= X <= 840"] <- "840"
data1$`Poverty bracket`[data1$`Poverty bracket`=="841 <= X <= 880"] <- "880"
data1$`Poverty bracket`[data1$`Poverty bracket`=="881 <= X <= 920"] <- "920"
data1$`Poverty bracket`[data1$`Poverty bracket`=="921 <= X <= 960"] <- "960"
data1$`Poverty bracket`[data1$`Poverty bracket`=="961 <= X <= 1000"] <- "1000"

data1 <- data1 %>% pivot_longer(
  cols=c(`Average Pell Grant`, `Average state grant`, `Average institutional grant`), 
  names_to="Grant type", 
  values_to="Amount"
) %>% mutate(
  `Poverty bracket` = as.numeric(`Poverty bracket`)
) %>% mutate(
  `Grant type` = factor(`Grant type`, levels=c(
    "Average Pell Grant", 
    "Average state grant", 
    "Average institutional grant"
  )), 
  `Selectivity` = factor(`Selectivity`, levels=c(
    "Overall", 
    "Open admission", 
    "Minimally selective", 
    "Moderately selective", 
    "Very selective"
  )), 
  `In-state status` = factor(`In-state status`, levels=c(
    "Overall", 
    "In-state", 
    "Out-of-state"
  ))
)

data1 <- data1 %>% filter(
  `Poverty bracket` <= 800
) %>% mutate(
  `Grant type` = factor(`Grant type`, levels=rev(levels(`Grant type`)))
)

#### End #### 

#### Figure 3 ####

data2 <- data1 %>% filter(
  `In-state status`=="Overall", 
  `Selectivity`=="Overall", 
  `Poverty bracket` <= 610
) %>% mutate(
  `Poverty bracket` = `Poverty bracket` / 100
)

ggplot(
  data=data2, 
  mapping=aes(
    x=`Poverty bracket`, y=`Amount`, fill=`Grant type`
  )
) + geom_bar(
  position="stack", stat="identity"
) + scale_y_continuous(
  labels=dollar_format(accuracy=1)
) + scale_fill_brewer(
  palette = "Dark2"
) + labs(
  x="Family income as share of federal poverty line"
) + scale_x_continuous(
  labels=percent_format(accuracy=1)
)

#### End #### 

#### Figure 6 ####

data3 <- data1 %>% filter(
  `In-state status`=="Overall", 
  `Selectivity` %in% c("Open admission", "Very selective"), 
  `Poverty bracket` <= 610
) %>% mutate(
  `Poverty bracket` = `Poverty bracket` / 100
)

ggplot(
  data=data3, 
  mapping=aes(
    x=`Poverty bracket`, y=`Amount`, fill=`Grant type`
  )
) + geom_bar(
  position="stack", stat="identity"
) + facet_grid(
  . ~ `Selectivity`
) + scale_y_continuous(
  labels=dollar_format(accuracy=1)
) + scale_fill_brewer(
  palette = "Dark2"
) + labs(
  x="Family income as share of federal poverty line"
) + scale_x_continuous(
  labels=percent_format(accuracy=1)
)

#### End #### 

##########################################
#### Trends in Student Aid: Over time ####
##########################################

#### Load Fig SA-1 data ####

setwd("/Users/peter_granville/Net Price Equity")

sa1 <- read_excel(
  "Trends-in-Student-Aid-2024-excel-data.xlsx", 
  sheet="Table SA-1_ALL", 
  col_names=TRUE, 
  skip=1, 
  n_max=28
) %>% select(
  -(`research.collegeboard.org/trends`)
) %>% rename(
  `23-24` = `23-24 (Preliminary)`
) %>% mutate(
  `Name` = ifelse(
    is.na(`...1`), 
    `...2`, 
    `...1`
  )
) %>% select(
  -(`...1`), -(`...2`)
) %>% filter(
  `Name` %in% c(
    "Total Federal Grants", 
    "STATE GRANTS",
    "INSTITUTIONAL GRANTS"
  )
) %>% mutate(
  `Name` = ifelse(
    `Name`=="Total Federal Grants", 
    "Federal grants",
    `Name`
  )
) %>% mutate(
  `Name` = ifelse(
    `Name`=="STATE GRANTS", 
    "State grants",
    `Name`
  )
) %>% mutate(
  `Name` = ifelse(
    `Name`=="INSTITUTIONAL GRANTS", 
    "Institutional grants",
    `Name`
  )
)

#### End #### 

#### Figure 1 ####

sa1 <- sa1 %>% pivot_longer(
  cols=c(
    `70-71`,
    `71-72`,
    `72-73`,
    `73-74`,
    `74-75`,
    `75-76`,
    `76-77`,
    `77-78`,
    `78-79`,
    `79-80`,
    `80-81`,
    `81-82`,
    `82-83`,
    `83-84`,
    `84-85`,
    `85-86`,
    `86-87`,
    `87-88`,
    `88-89`,
    `89-90`,
    `90-91`,
    `91-92`,
    `92-93`,
    `93-94`,
    `94-95`,
    `95-96`,
    `96-97`,
    `97-98`,
    `98-99`,
    `99-00`,
    `00-01`,
    `01-02`,
    `02-03`,
    `03-04`,
    `04-05`,
    `05-06`,
    `06-07`,
    `07-08`,
    `08-09`,
    `09-10`,
    `10-11`,
    `11-12`,
    `12-13`,
    `13-14`,
    `14-15`,
    `15-16`,
    `16-17`,
    `17-18`,
    `18-19`,
    `19-20`,
    `20-21`,
    `21-22`,
    `22-23`,
    `23-24`
  ), 
  names_to="Year", 
  values_to="Amount"
) %>% mutate(
  `Amount` = `Amount` * 1000000
) %>% mutate(
  `Year` = factor(`Year`, levels=c(
    "70-71",
    "71-72",
    "72-73",
    "73-74",
    "74-75",
    "75-76",
    "76-77",
    "77-78",
    "78-79",
    "79-80",
    "80-81",
    "81-82",
    "82-83",
    "83-84",
    "84-85",
    "85-86",
    "86-87",
    "87-88",
    "88-89",
    "89-90",
    "90-91",
    "91-92",
    "92-93",
    "93-94",
    "94-95",
    "95-96",
    "96-97",
    "97-98",
    "98-99",
    "99-00",
    "00-01",
    "01-02",
    "02-03",
    "03-04",
    "04-05",
    "05-06",
    "06-07",
    "07-08",
    "08-09",
    "09-10",
    "10-11",
    "11-12",
    "12-13",
    "13-14",
    "14-15",
    "15-16",
    "16-17",
    "17-18",
    "18-19",
    "19-20",
    "20-21",
    "21-22",
    "22-23",
    "23-24"
  ))
)

sa1 <- sa1 %>% filter(
  (`Year` %in% c(
    "70-71",
    "71-72", 
    "72-73", 
    "73-74",
    "74-75", 
    "75-76", 
    "76-77", 
    "77-78",
    "78-79", 
    "79-80"
  ))==FALSE
)

ggplot(
  data=sa1, 
  mapping=aes(
    x=`Year`,
    y=`Amount`, 
    group=`Name`, 
    color=`Name`
  )
) + geom_point() + geom_line() + scale_y_continuous(
  labels=dollar_format(accuracy=1)
) + scale_x_discrete(
  breaks=c("70-71", "80-81", "90-91", "00-01", "10-11", "20-21")
) + labs(
  color="Grant type", 
  y="Amount (2023 USD)"
) + scale_color_brewer(
  palette = "Dark2"
)

#### End #### 
