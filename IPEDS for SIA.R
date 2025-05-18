
#### Setup ####

library(scales)
library(tidyverse)
library(RColorBrewer)

setwd("/Users/peter_granville/Net Price Equity/IPEDS Data")

#### End #### 

##########################################
#### Load data                        ####
##########################################

#### Write function to load financial aid data ####

loadFinAid <- function(hdName, sfaName, yearName){
  
  print(paste("Running", yearName, sep=" "))
  
  sfaSelect <- c(
    "UNITID",  # Unique identification number of the institution
    "SCUGFFN", # Total number of full-time first-time degree/certificate seeking undergraduates - financial aid cohort
    "FGRNT_P", # Percent of full-time first-time undergraduates  receiving federal grant aid
    "SGRNT_P", # Percent of full-time first-time undergraduates receiving state/local grant aid
    "IGRNT_P", # Percent of full-time first-time undergraduates receiving institutional grant aid
    "AGRNT_P", # Percent of full-time first-time undergraduates receiving federal, state, local or institutional grant aid
    "FGRNT_T", # Total amount of Federal grant aid received by full-time first-time undergraduates
    "SGRNT_T", # Total amount of state/local grant aid received by full-time first-time undergraduates
    "IGRNT_T"  # Total amount of institutional grant aid received by full-time first-time undergraduates
  )
  
  hdSelect <- c(
    "UNITID", 
    "INSTNM", 
    "STABBR",
    "SECTOR"
  )
  
  if(yearName==2009){hdSelect <- tolower(hdSelect)}
  
  sfa <- read.csv(sfaName, header=TRUE) %>% select(all_of(sfaSelect))
  hd <- read.csv(hdName, header=TRUE) %>% select(all_of(hdSelect))
  
  if(yearName==2009){names(hd) <- toupper(names(hd))}
  
  finAid <- left_join(x=hd, y=sfa, by="UNITID") %>% mutate(`Year` = rep(yearName))
  
  return(finAid)
  rm(sfa, hd, finAid, sfaSelect, hdSelect)
  
}

#### End #### 

#### Run data pull ####

finAid <- rbind(
  loadFinAid(hdName="hd2009.csv", sfaName="sfa0809_rv.csv", yearName=2009), 
  loadFinAid(hdName="hd2010.csv", sfaName="sfa0910_rv.csv", yearName=2010), 
  loadFinAid(hdName="hd2011.csv", sfaName="sfa1011_rv.csv", yearName=2011), 
  loadFinAid(hdName="hd2012.csv", sfaName="sfa1112_rv.csv", yearName=2012), 
  loadFinAid(hdName="hd2013.csv", sfaName="sfa1213_rv.csv", yearName=2013), 
  loadFinAid(hdName="hd2014.csv", sfaName="sfa1314_rv.csv", yearName=2014), 
  loadFinAid(hdName="hd2015.csv", sfaName="sfa1415_rv.csv", yearName=2015), 
  loadFinAid(hdName="hd2016.csv", sfaName="sfa1516_rv.csv", yearName=2016), 
  loadFinAid(hdName="hd2017.csv", sfaName="sfa1617_rv.csv", yearName=2017), 
  loadFinAid(hdName="hd2018.csv", sfaName="sfa1718_rv.csv", yearName=2018), 
  loadFinAid(hdName="hd2019.csv", sfaName="sfa1819_rv.csv", yearName=2019), 
  loadFinAid(hdName="hd2020.csv", sfaName="sfa1920_rv.csv", yearName=2020), 
  loadFinAid(hdName="hd2021.csv", sfaName="sfa2021_rv.csv", yearName=2021), 
  loadFinAid(hdName="hd2022.csv", sfaName="sfa2122_rv.csv", yearName=2022), 
  loadFinAid(hdName="hd2023.csv", sfaName="sfa2223.csv", yearName=2023)
)

finAid <- finAid %>% mutate(
  `FGRNT_P` = `FGRNT_P` / 100, 
  `SGRNT_P` = `SGRNT_P` / 100, 
  `IGRNT_P` = `IGRNT_P` / 100, 
  `AGRNT_P` = `AGRNT_P` / 100
) %>% filter(
  is.na(`IGRNT_T`)==FALSE # When one SFA variable is NA, so are all the rest, so you only need one filter  
)

#### End #### 

##########################################
#### Shares receiving grant aid       ####
##########################################

#### Shares receiving grants by type (overall) ####

agg1 <- finAid %>% pivot_longer(
  cols=c(`FGRNT_P`, `SGRNT_P`, `IGRNT_P`, `AGRNT_P`), 
  names_to="Grant type", 
  values_to="Percentage"
) %>% group_by(
  `Year`, `Grant type`
) %>% summarize(
  `Share receiving grants` = weighted.mean(w=`SCUGFFN`, x=`Percentage`)
)

agg1$`Grant type`[agg1$`Grant type`=="FGRNT_P"] <- "Federal grant aid"
agg1$`Grant type`[agg1$`Grant type`=="SGRNT_P"] <- "State/local grant aid"
agg1$`Grant type`[agg1$`Grant type`=="IGRNT_P"] <- "Institutional grant aid"
agg1$`Grant type`[agg1$`Grant type`=="AGRNT_P"] <- "Federal, state, local or institutional grant aid"

#### End #### 

#### Shares receiving grants by type (by sector) ####

agg2 <- finAid %>% pivot_longer(
  cols=c(`FGRNT_P`, `SGRNT_P`, `IGRNT_P`, `AGRNT_P`), 
  names_to="Grant type", 
  values_to="Percentage"
) %>% group_by(
  `SECTOR`, `Year`, `Grant type`
) %>% summarize(
  `Share receiving grants` = weighted.mean(w=`SCUGFFN`, x=`Percentage`)
)

agg2$`Grant type`[agg2$`Grant type`=="FGRNT_P"] <- "Federal grant aid"
agg2$`Grant type`[agg2$`Grant type`=="SGRNT_P"] <- "State/local grant aid"
agg2$`Grant type`[agg2$`Grant type`=="IGRNT_P"] <- "Institutional grant aid"
agg2$`Grant type`[agg2$`Grant type`=="AGRNT_P"] <- "Federal, state, local or institutional grant aid"

#### End #### 

#### Shares receiving grants by type (by state, public four-years) ####

agg3 <- finAid %>% pivot_longer(
  cols=c(`FGRNT_P`, `SGRNT_P`, `IGRNT_P`, `AGRNT_P`), 
  names_to="Grant type", 
  values_to="Percentage"
) %>% filter(
  `SECTOR`==1
) %>% group_by(
  `STABBR`, `Year`, `Grant type`
) %>% summarize(
  `Share receiving grants` = weighted.mean(w=`SCUGFFN`, x=`Percentage`)
)

agg3$`Grant type`[agg3$`Grant type`=="FGRNT_P"] <- "Federal grant aid"
agg3$`Grant type`[agg3$`Grant type`=="SGRNT_P"] <- "State/local grant aid"
agg3$`Grant type`[agg3$`Grant type`=="IGRNT_P"] <- "Institutional grant aid"
agg3$`Grant type`[agg3$`Grant type`=="AGRNT_P"] <- "Federal, state, local or institutional grant aid"

#### End #### 

#### Shares receiving grants by type (by state, public two-years) ####

agg4 <- finAid %>% pivot_longer(
  cols=c(`FGRNT_P`, `SGRNT_P`, `IGRNT_P`, `AGRNT_P`), 
  names_to="Grant type", 
  values_to="Percentage"
) %>% filter(
  `SECTOR`==4
) %>% group_by(
  `STABBR`, `Year`, `Grant type`
) %>% summarize(
  `Share receiving grants` = weighted.mean(w=`SCUGFFN`, x=`Percentage`)
)

agg4$`Grant type`[agg4$`Grant type`=="FGRNT_P"] <- "Federal grant aid"
agg4$`Grant type`[agg4$`Grant type`=="SGRNT_P"] <- "State/local grant aid"
agg4$`Grant type`[agg4$`Grant type`=="IGRNT_P"] <- "Institutional grant aid"
agg4$`Grant type`[agg4$`Grant type`=="AGRNT_P"] <- "Federal, state, local or institutional grant aid"

#### End #### 

#### Write function to visualize shares ####

vizShares <- function(data1, facetVar, plotTitle){
  
  data1 <- data1 %>% mutate(
    `Grant type` = factor(`Grant type`, levels=c(
      "Federal grant aid",
      "State/local grant aid",
      "Institutional grant aid",
      "Federal, state, local or institutional grant aid"
    ))
  )
  
  if(facetVar=="SECTOR"){
    data1$`SECTOR`[data1$`SECTOR`==1] <- "Public four-years"
    data1$`SECTOR`[data1$`SECTOR`==2] <- "Nonprofit four-years"
    data1$`SECTOR`[data1$`SECTOR`==3] <- "For-profit four-years"
    data1$`SECTOR`[data1$`SECTOR`==4] <- "Public two-years"
    data1$`SECTOR`[data1$`SECTOR`==5] <- "Nonprofit two-years"
    data1$`SECTOR`[data1$`SECTOR`==6] <- "For-profit two-years"
    data1$`SECTOR`[data1$`SECTOR`==7] <- "Public less-than-two-years"
    data1$`SECTOR`[data1$`SECTOR`==8] <- "Nonprofit less-than-two-years"
    data1$`SECTOR`[data1$`SECTOR`==9] <- "For-profit less-than-two-years"
    data1 <- data1 %>% mutate(
      `SECTOR` = factor(`SECTOR`, levels=c(
        "Public four-years",
        "Nonprofit four-years",
        "For-profit four-years",
        "Public two-years",
        "Nonprofit two-years",
        "For-profit two-years",
        "Public less-than-two-years",
        "Nonprofit less-than-two-years",
        "For-profit less-than-two-years"
      ))
    ) %>% rename(
      `For Facet` = `SECTOR`
    )
  }
  if(facetVar=="STABBR"){
    data1 <- data1 %>% rename(
      `For Facet` = `STABBR`
    ) %>% filter(
      (`For Facet` %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE
    )
  }
  
  plot1 <- ggplot(
    data=data1, 
    mapping=aes(
      x=`Year`, y=`Share receiving grants`, grouping=`Grant type`, color=`Grant type`
    ) 
  ) + geom_point() + geom_line() + scale_y_continuous(
    labels=percent_format(accuracy=1), limits=c(0, 1), breaks=c(0, 0.25, 0.5, 0.75, 1)
  ) + scale_x_continuous(
    breaks=c(2009, 2016, 2023)
  ) + ggtitle(
    plotTitle
  ) + theme(
    legend.position="bottom"
  ) + scale_color_brewer(
    palette = "Dark2"
  )
  if(facetVar != "None"){
    plot1 <- plot1 + facet_wrap(`For Facet` ~ .)
  }
  
  return(plot1)

}

#### End #### 

#### Run plots ####

vizShares(data1=agg1, facetVar="None", plotTitle="Shares of FTFT undergraduates receiving grants, by grant type")
vizShares(data1=agg2, facetVar="SECTOR", plotTitle="Shares of FTFT undergraduates receiving grants, by grant type and sector")
vizShares(data1=agg3, facetVar="STABBR", plotTitle="Shares of FTFT undergraduates receiving grants, by grant type and state (public four-year sector)")
vizShares(data1=agg4, facetVar="STABBR", plotTitle="Shares of FTFT undergraduates receiving grants, by grant type and state (public two-year sector)")

#### End #### 

#### Special plot: 2009 vs 2023, share receiving grants by type (agg2) ####

agg20 <- agg2 %>% filter(
  `SECTOR` %in% c(1, 2), 
  `Year` %in% c(2009, 2023), 
  `Grant type` != "Federal, state, local or institutional grant aid"
) %>% rename(
  `Sector` = `SECTOR`
) %>% mutate(
  `Sector` = ifelse(
    `Sector`==1, "Public four-year", "Nonprofit four-year"
  )
) %>% mutate(
  `Year` = factor(`Year`)
) %>% mutate(
  `Grant type` = gsub(" aid", "s", `Grant type`)
) %>% mutate(
  `Grant type` = factor(`Grant type`, levels=c(
    "Federal grants", 
    "State/local grants", 
    "Institutional grants"
  ))
) %>% mutate(
  `Sector` = factor(`Sector`, levels=c(
    "Public four-year", 
    "Nonprofit four-year"
  ))
) %>% mutate(
  `For Label` = percent(`Share receiving grants`, accuracy=1)
)

ggplot(data=agg20, 
       mapping=aes(
         x=`Year`, 
         y=`Share receiving grants`, 
         fill=`Grant type`
       )
) + geom_bar(
  stat="identity", 
  width=0.6,
  position=position_dodge(width=0.75)
) + facet_wrap(
  `Sector` ~ .
) + scale_y_continuous(
  labels=percent_format(accuracy=1)
) + scale_fill_manual(
  values=c("#756fb3", "#d95f01", "#1c9e78")
) + geom_text(
  aes(label = `For Label`), 
  position=position_dodge(width=0.75), 
  vjust=-0.3
) + theme(text = element_text(size = 13)) 

#### End #### 

#### Special analysis: 2009 vs 2023, P4Y shares receiving any grants by state (agg3) ####

agg30 <- agg3 %>% filter(
  `Year` %in% c(2009, 2023), 
  `Grant type` == "Federal, state, local or institutional grant aid"
) %>% pivot_wider(
  id_cols=c(`STABBR`), 
  names_from = `Year`, 
  values_from=`Share receiving grants`
) %>% filter(
  (`STABBR` %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE
) %>% mutate(
  `Increase?` = ifelse(`2023` > `2009`, "Increase", "No increase"), 
  `Margin` = `2023` - `2009`
)

#### End #### 

#### Special analysis: 2009 vs 2023, P2Y shares receiving any grants by state (agg4) ####

agg40 <- agg4 %>% filter(
  `Year` %in% c(2009, 2023), 
  `Grant type` == "Federal, state, local or institutional grant aid"
) %>% pivot_wider(
  id_cols=c(`STABBR`), 
  names_from = `Year`, 
  values_from=`Share receiving grants`
) %>% filter(
  (`STABBR` %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE
) %>% mutate(
  `Increase?` = ifelse(`2023` > `2009`, "Increase", "No increase"), 
  `Margin` = `2023` - `2009`
)

#### End #### 

#### Special analysis: 2009 vs 2023, P4Y shares receiving Pell grants by state (agg3) ####

agg31 <- agg3 %>% filter(
  `Year` %in% c(2009, 2023), 
  `Grant type` == "Federal grant aid"
) %>% pivot_wider(
  id_cols=c(`STABBR`), 
  names_from = `Year`, 
  values_from=`Share receiving grants`
) %>% filter(
  (`STABBR` %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE
) %>% mutate(
  `Increase?` = ifelse(`2023` > `2009`, "Increase", "No increase"), 
  `Margin` = `2023` - `2009`
)

#### End #### 

#### Special analysis: 2009 vs 2023, P2Y shares receiving Pell grants by state (agg4) ####

agg41 <- agg4 %>% filter(
  `Year` %in% c(2009, 2023), 
  `Grant type` == "Federal grant aid"
) %>% pivot_wider(
  id_cols=c(`STABBR`), 
  names_from = `Year`, 
  values_from=`Share receiving grants`
) %>% filter(
  (`STABBR` %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE
) %>% mutate(
  `Increase?` = ifelse(`2023` > `2009`, "Increase", "No increase"), 
  `Margin` = `2023` - `2009`
)

#### End #### 

#### Special plot: Share receiving grants (moved legend to side) #### 

agg10 <- agg1 %>% mutate(
  `Grant type` = ifelse(
    `Grant type` == "Federal, state, local or institutional grant aid", "Any grant aid", `Grant type`
  )
) %>% mutate(
  `Grant type` = gsub(" aid", "s", `Grant type`)
) %>% mutate(
  `Grant type` = factor(`Grant type`, levels=c(
    "Federal grants", 
    "State/local grants", 
    "Institutional grants", 
    "Any grants"
  ))
)

ggplot(
  data=agg10, 
  mapping=aes(
    x=`Year`, y=`Share receiving grants`, grouping=`Grant type`, color=`Grant type`
  ) 
) + geom_point() + geom_line() + scale_y_continuous(
  labels=percent_format(accuracy=1), limits=c(0, 1), breaks=c(0, 0.25, 0.5, 0.75, 1)
) + scale_x_continuous(
  breaks=c(2009, 2016, 2023)
) + theme(
  legend.position="bottom"
) + scale_color_manual(
  values=c("#756fb3", "#d95f01", "#1c9e78", "#e72989")
) + theme(legend.position="right")

#### End #### 

##########################################
#### Total grant aid                  ####
##########################################

#### Share of total grant aid by source (overall) ####

agg5 <- aggregate(
  data=finAid, cbind(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`) ~ `Year`, FUN=sum
) %>% mutate(
  `Total grants` = `FGRNT_T` + `SGRNT_T` + `IGRNT_T`
) %>% mutate(
  `Federal grants` = `FGRNT_T` / `Total grants`, 
  `State/local grants` = `SGRNT_T` / `Total grants`, 
  `Institutional grants` = `IGRNT_T` / `Total grants`
) %>% select(
  `Year`,
  `Federal grants`, 
  `State/local grants`, 
  `Institutional grants`
) %>% pivot_longer(
  cols=c(
    `Federal grants`, 
    `State/local grants`, 
    `Institutional grants`
  ), 
  names_to="Grant type", 
  values_to="Share"
)

#### End #### 

#### Share of total grant aid by source (by sector) ####

agg6 <- aggregate(
  data=finAid, cbind(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`) ~ `Year` + `SECTOR`, FUN=sum
) %>% mutate(
  `Total grants` = `FGRNT_T` + `SGRNT_T` + `IGRNT_T`
) %>% mutate(
  `Federal grants` = `FGRNT_T` / `Total grants`, 
  `State/local grants` = `SGRNT_T` / `Total grants`, 
  `Institutional grants` = `IGRNT_T` / `Total grants`
) %>% select(
  `Year`,
  `SECTOR`,
  `Federal grants`, 
  `State/local grants`, 
  `Institutional grants`
) %>% pivot_longer(
  cols=c(
    `Federal grants`, 
    `State/local grants`, 
    `Institutional grants`
  ), 
  names_to="Grant type", 
  values_to="Share"
)

#### End #### 

#### Share of total grant aid by source (by state, public four-years) ####

agg7 <- finAid %>% filter(`SECTOR`==1)
agg7 <- aggregate(
  data=agg7, cbind(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`) ~ `Year` + `STABBR`, FUN=sum
) %>% mutate(
  `Total grants` = `FGRNT_T` + `SGRNT_T` + `IGRNT_T`
) %>% mutate(
  `Federal grants` = `FGRNT_T` / `Total grants`, 
  `State/local grants` = `SGRNT_T` / `Total grants`, 
  `Institutional grants` = `IGRNT_T` / `Total grants`
) %>% select(
  `Year`,
  `STABBR`,
  `Federal grants`, 
  `State/local grants`, 
  `Institutional grants`
) %>% pivot_longer(
  cols=c(
    `Federal grants`, 
    `State/local grants`, 
    `Institutional grants`
  ), 
  names_to="Grant type", 
  values_to="Share"
)

#### End #### 

#### Share of total grant aid by source (by state, public two-years) ####

agg8 <- finAid %>% filter(`SECTOR`==4)
agg8 <- aggregate(
  data=agg8, cbind(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`) ~ `Year` + `STABBR`, FUN=sum
) %>% mutate(
  `Total grants` = `FGRNT_T` + `SGRNT_T` + `IGRNT_T`
) %>% mutate(
  `Federal grants` = `FGRNT_T` / `Total grants`, 
  `State/local grants` = `SGRNT_T` / `Total grants`, 
  `Institutional grants` = `IGRNT_T` / `Total grants`
) %>% select(
  `Year`,
  `STABBR`,
  `Federal grants`, 
  `State/local grants`, 
  `Institutional grants`
) %>% pivot_longer(
  cols=c(
    `Federal grants`, 
    `State/local grants`, 
    `Institutional grants`
  ), 
  names_to="Grant type", 
  values_to="Share"
)

#### End #### 

#### Write function to visualize shares ####

vizTotals <- function(data1, facetVar, plotTitle){
  
  data1 <- data1 %>% mutate(
    `Grant type` = factor(`Grant type`, levels=c(
      "Federal grants",
      "State/local grants",
      "Institutional grants"
    ))
  )
  
  if(facetVar=="SECTOR"){
    data1$`SECTOR`[data1$`SECTOR`==1] <- "Public four-years"
    data1$`SECTOR`[data1$`SECTOR`==2] <- "Nonprofit four-years"
    data1$`SECTOR`[data1$`SECTOR`==3] <- "For-profit four-years"
    data1$`SECTOR`[data1$`SECTOR`==4] <- "Public two-years"
    data1$`SECTOR`[data1$`SECTOR`==5] <- "Nonprofit two-years"
    data1$`SECTOR`[data1$`SECTOR`==6] <- "For-profit two-years"
    data1$`SECTOR`[data1$`SECTOR`==7] <- "Public less-than-two-years"
    data1$`SECTOR`[data1$`SECTOR`==8] <- "Nonprofit less-than-two-years"
    data1$`SECTOR`[data1$`SECTOR`==9] <- "For-profit less-than-two-years"
    data1 <- data1 %>% mutate(
      `SECTOR` = factor(`SECTOR`, levels=c(
        "Public four-years",
        "Nonprofit four-years",
        "For-profit four-years",
        "Public two-years",
        "Nonprofit two-years",
        "For-profit two-years",
        "Public less-than-two-years",
        "Nonprofit less-than-two-years",
        "For-profit less-than-two-years"
      ))
    ) %>% rename(
      `For Facet` = `SECTOR`
    )
  }
  if(facetVar=="STABBR"){
    data1 <- data1 %>% rename(
      `For Facet` = `STABBR`
    ) %>% filter(
      (`For Facet` %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE
    )
  }
  
  plot1 <- ggplot(
    data=data1, 
    mapping=aes(
      x=`Year`, y=`Share`, fill=`Grant type`
    ) 
  ) + geom_bar(
    position="stack", stat="identity"
  ) + scale_y_continuous(
    labels=percent_format(accuracy=1), limits=c(0, 1), breaks=c(0, 0.25, 0.5, 0.75, 1)
  ) + scale_x_continuous(
    breaks=c(2009, 2016, 2023)
  ) + ggtitle(
    plotTitle
  ) + theme(
    legend.position="bottom"
  ) + scale_fill_brewer(
    palette = "Dark2"
  )
  if(facetVar != "None"){
    plot1 <- plot1 + facet_wrap(`For Facet` ~ .)
  }
  
  return(plot1)
  
}

#### End #### 

#### Run plots ####

vizTotals(data1=agg5, facetVar="None", plotTitle="Shares of total grants to FTFT undergraduates, by grant type")
vizTotals(data1=agg6, facetVar="SECTOR", plotTitle="Shares of total grants to FTFT undergraduates, by grant type and sector")
vizTotals(data1=agg7, facetVar="STABBR", plotTitle="Shares of total grants to FTFT undergraduates, by grant type and state (public four-year sector)")
vizTotals(data1=agg8, facetVar="STABBR", plotTitle="Shares of total grants to FTFT undergraduates, by grant type and state (public two-year sector)")

#### End #### 

#### Special plot: Specific states ####

# agg70 <- agg7 %>% filter(
#   `STABBR` %in% c("CA", "NC", "NY")
# ) %>% mutate(
#   `Grant type` = factor(`Grant type`, levels=c(
#     "Federal grants",
#     "State/local grants",
#     "Institutional grants"
#   ))
# )
# 
# ggplot(
#   data=agg70, 
#   mapping=aes(
#     x=`Year`, y=`Share`, fill=`Grant type`
#   ) 
# ) + geom_bar(
#   position="stack", stat="identity"
# ) + scale_y_continuous(
#   labels=percent_format(accuracy=1), limits=c(0, 1), breaks=c(0, 0.25, 0.5, 0.75, 1)
# ) + scale_x_continuous(
#   breaks=c(2009, 2016, 2023)
# ) + theme(
#   legend.position="bottom"
# ) + scale_fill_brewer(
#   palette = "Dark2"
# ) + facet_wrap(`STABBR` ~ .)

#### End #### 

#### Special plot: Specific sectors ####

agg60 <- agg6 %>% filter(
  `SECTOR` %in% c(1, 4)
) %>% mutate(
  `SECTOR` = ifelse(
    `SECTOR`==1, "Public four-year", "Community college"
  ) 
) %>% mutate(
  `SECTOR` = factor(`SECTOR`, levels=c(
    "Public four-year", "Community college"
  )), 
  `Grant type` = factor(`Grant type`, levels=c(
    "Federal grants", 
    "State/local grants", 
    "Institutional grants"
  ))
)

ggplot(
  data=agg60, 
  mapping=aes(
    x=`Year`, y=`Share`, fill=forcats::fct_rev(`Grant type`)
  ) 
) + geom_bar(
  position="stack", stat="identity"
) + facet_wrap(
  `SECTOR` ~ .
) + scale_y_continuous(
  labels=percent_format(accuracy=1), limits=c(0, 1), breaks=c(0, 0.25, 0.5, 0.75, 1)
) + scale_x_continuous(
  breaks=c(2009, 2016, 2023)
) + theme(
  legend.position="bottom"
) + scale_fill_brewer(
  palette = "Dark2"
) + labs(
  fill="Grant type", 
  y="Share of total grant aid"
)

#### End #### 

##########################################
#### Change since 2009                ####
##########################################

#### Share of total grant aid by source (overall) ####

agg9 <- aggregate(
  data=finAid, cbind(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`, `SCUGFFN`) ~ `Year`, FUN=sum
) %>% pivot_longer(
  cols=c(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`, `SCUGFFN`), 
  names_to="Variable", 
  values_to="Total"
) %>% pivot_wider(
  id_cols=c(`Variable`), 
  names_from=`Year`, 
  values_from=`Total`
) %>% mutate(
  `Baseline` = `2009`
) %>% mutate(
  `2009` = (`2009` - `Baseline`) / `Baseline`,
  `2010` = (`2010` - `Baseline`) / `Baseline`, 
  `2011` = (`2011` - `Baseline`) / `Baseline`, 
  `2012` = (`2012` - `Baseline`) / `Baseline`, 
  `2013` = (`2013` - `Baseline`) / `Baseline`, 
  `2014` = (`2014` - `Baseline`) / `Baseline`, 
  `2015` = (`2015` - `Baseline`) / `Baseline`, 
  `2016` = (`2016` - `Baseline`) / `Baseline`, 
  `2017` = (`2017` - `Baseline`) / `Baseline`, 
  `2018` = (`2018` - `Baseline`) / `Baseline`, 
  `2019` = (`2019` - `Baseline`) / `Baseline`, 
  `2020` = (`2020` - `Baseline`) / `Baseline`, 
  `2021` = (`2021` - `Baseline`) / `Baseline`, 
  `2022` = (`2022` - `Baseline`) / `Baseline`, 
  `2023` = (`2023` - `Baseline`) / `Baseline`
) %>% select(
  -`Baseline`
) %>% pivot_longer(
  cols=c(
    `2009`,
    `2010`, 
    `2011`, 
    `2012`, 
    `2013`, 
    `2014`, 
    `2015`, 
    `2016`, 
    `2017`, 
    `2018`, 
    `2019`, 
    `2020`, 
    `2021`, 
    `2022`, 
    `2023`
  ), 
  names_to="Year", 
  values_to="Change since 2009"
)

agg9$`Variable`[agg9$`Variable`=="FGRNT_T"] <- "Federal grants"
agg9$`Variable`[agg9$`Variable`=="IGRNT_T"] <- "Institutional grants"
agg9$`Variable`[agg9$`Variable`=="SGRNT_T"] <- "State/local grants"
agg9$`Variable`[agg9$`Variable`=="SCUGFFN"] <- "FTFT students"

#### End #### 

#### Share of total grant aid by source (by sector) ####

agg10 <- aggregate(
  data=finAid, cbind(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`, `SCUGFFN`) ~ `Year` + `SECTOR`, FUN=sum
) %>% pivot_longer(
  cols=c(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`, `SCUGFFN`), 
  names_to="Variable", 
  values_to="Total"
) %>% pivot_wider(
  id_cols=c(`Variable`, `SECTOR`), 
  names_from=`Year`, 
  values_from=`Total`
) %>% mutate(
  `Baseline` = `2009`
) %>% mutate(
  `2009` = (`2009` - `Baseline`) / `Baseline`,
  `2010` = (`2010` - `Baseline`) / `Baseline`, 
  `2011` = (`2011` - `Baseline`) / `Baseline`, 
  `2012` = (`2012` - `Baseline`) / `Baseline`, 
  `2013` = (`2013` - `Baseline`) / `Baseline`, 
  `2014` = (`2014` - `Baseline`) / `Baseline`, 
  `2015` = (`2015` - `Baseline`) / `Baseline`, 
  `2016` = (`2016` - `Baseline`) / `Baseline`, 
  `2017` = (`2017` - `Baseline`) / `Baseline`, 
  `2018` = (`2018` - `Baseline`) / `Baseline`, 
  `2019` = (`2019` - `Baseline`) / `Baseline`, 
  `2020` = (`2020` - `Baseline`) / `Baseline`, 
  `2021` = (`2021` - `Baseline`) / `Baseline`, 
  `2022` = (`2022` - `Baseline`) / `Baseline`, 
  `2023` = (`2023` - `Baseline`) / `Baseline`
) %>% select(
  -`Baseline`
) %>% pivot_longer(
  cols=c(
    `2009`,
    `2010`, 
    `2011`, 
    `2012`, 
    `2013`, 
    `2014`, 
    `2015`, 
    `2016`, 
    `2017`, 
    `2018`, 
    `2019`, 
    `2020`, 
    `2021`, 
    `2022`, 
    `2023`
  ), 
  names_to="Year", 
  values_to="Change since 2009"
)

agg10$`Variable`[agg10$`Variable`=="FGRNT_T"] <- "Federal grants"
agg10$`Variable`[agg10$`Variable`=="IGRNT_T"] <- "Institutional grants"
agg10$`Variable`[agg10$`Variable`=="SGRNT_T"] <- "State/local grants"
agg10$`Variable`[agg10$`Variable`=="SCUGFFN"] <- "FTFT students"

#### End #### 

#### Share of total grant aid by source (by state, public four-years) ####

agg11 <- finAid %>% filter(`SECTOR`==1)
agg11 <- aggregate(
  data=agg11, cbind(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`, `SCUGFFN`) ~ `Year` + `STABBR`, FUN=sum
) %>% pivot_longer(
  cols=c(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`, `SCUGFFN`), 
  names_to="Variable", 
  values_to="Total"
) %>% pivot_wider(
  id_cols=c(`Variable`, `STABBR`), 
  names_from=`Year`, 
  values_from=`Total`
) %>% mutate(
  `Baseline` = `2009`
) %>% mutate(
  `2009` = (`2009` - `Baseline`) / `Baseline`,
  `2010` = (`2010` - `Baseline`) / `Baseline`, 
  `2011` = (`2011` - `Baseline`) / `Baseline`, 
  `2012` = (`2012` - `Baseline`) / `Baseline`, 
  `2013` = (`2013` - `Baseline`) / `Baseline`, 
  `2014` = (`2014` - `Baseline`) / `Baseline`, 
  `2015` = (`2015` - `Baseline`) / `Baseline`, 
  `2016` = (`2016` - `Baseline`) / `Baseline`, 
  `2017` = (`2017` - `Baseline`) / `Baseline`, 
  `2018` = (`2018` - `Baseline`) / `Baseline`, 
  `2019` = (`2019` - `Baseline`) / `Baseline`, 
  `2020` = (`2020` - `Baseline`) / `Baseline`, 
  `2021` = (`2021` - `Baseline`) / `Baseline`, 
  `2022` = (`2022` - `Baseline`) / `Baseline`, 
  `2023` = (`2023` - `Baseline`) / `Baseline`
) %>% select(
  -`Baseline`
) %>% pivot_longer(
  cols=c(
    `2009`,
    `2010`, 
    `2011`, 
    `2012`, 
    `2013`, 
    `2014`, 
    `2015`, 
    `2016`, 
    `2017`, 
    `2018`, 
    `2019`, 
    `2020`, 
    `2021`, 
    `2022`, 
    `2023`
  ), 
  names_to="Year", 
  values_to="Change since 2009"
)

agg11$`Variable`[agg11$`Variable`=="FGRNT_T"] <- "Federal grants"
agg11$`Variable`[agg11$`Variable`=="IGRNT_T"] <- "Institutional grants"
agg11$`Variable`[agg11$`Variable`=="SGRNT_T"] <- "State/local grants"
agg11$`Variable`[agg11$`Variable`=="SCUGFFN"] <- "FTFT students"

#### End #### 

#### Share of total grant aid by source (by state, public two-years) ####

agg12 <- finAid %>% filter(`SECTOR`==4)
agg12 <- aggregate(
  data=agg12, cbind(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`, `SCUGFFN`) ~ `Year` + `STABBR`, FUN=sum
) %>% pivot_longer(
  cols=c(`FGRNT_T`, `SGRNT_T`, `IGRNT_T`, `SCUGFFN`), 
  names_to="Variable", 
  values_to="Total"
) %>% pivot_wider(
  id_cols=c(`Variable`, `STABBR`), 
  names_from=`Year`, 
  values_from=`Total`
) %>% mutate(
  `Baseline` = `2009`
) %>% mutate(
  `2009` = (`2009` - `Baseline`) / `Baseline`,
  `2010` = (`2010` - `Baseline`) / `Baseline`, 
  `2011` = (`2011` - `Baseline`) / `Baseline`, 
  `2012` = (`2012` - `Baseline`) / `Baseline`, 
  `2013` = (`2013` - `Baseline`) / `Baseline`, 
  `2014` = (`2014` - `Baseline`) / `Baseline`, 
  `2015` = (`2015` - `Baseline`) / `Baseline`, 
  `2016` = (`2016` - `Baseline`) / `Baseline`, 
  `2017` = (`2017` - `Baseline`) / `Baseline`, 
  `2018` = (`2018` - `Baseline`) / `Baseline`, 
  `2019` = (`2019` - `Baseline`) / `Baseline`, 
  `2020` = (`2020` - `Baseline`) / `Baseline`, 
  `2021` = (`2021` - `Baseline`) / `Baseline`, 
  `2022` = (`2022` - `Baseline`) / `Baseline`, 
  `2023` = (`2023` - `Baseline`) / `Baseline`
) %>% select(
  -`Baseline`
) %>% pivot_longer(
  cols=c(
    `2009`,
    `2010`, 
    `2011`, 
    `2012`, 
    `2013`, 
    `2014`, 
    `2015`, 
    `2016`, 
    `2017`, 
    `2018`, 
    `2019`, 
    `2020`, 
    `2021`, 
    `2022`, 
    `2023`
  ), 
  names_to="Year", 
  values_to="Change since 2009"
)

agg12$`Variable`[agg12$`Variable`=="FGRNT_T"] <- "Federal grants"
agg12$`Variable`[agg12$`Variable`=="IGRNT_T"] <- "Institutional grants"
agg12$`Variable`[agg12$`Variable`=="SGRNT_T"] <- "State/local grants"
agg12$`Variable`[agg12$`Variable`=="SCUGFFN"] <- "FTFT students"

#### End #### 

#### Write function to visualize shares ####

vizChange <- function(data1, facetVar, plotTitle){
  
  data1 <- data1 %>% mutate(
    `Variable` = factor(`Variable`, levels=c(
      "Federal grants",
      "State/local grants",
      "Institutional grants",
      "FTFT students"
    ))
  )
  
  if(facetVar=="SECTOR"){
    data1$`SECTOR`[data1$`SECTOR`==1] <- "Public four-years"
    data1$`SECTOR`[data1$`SECTOR`==2] <- "Nonprofit four-years"
    data1$`SECTOR`[data1$`SECTOR`==3] <- "For-profit four-years"
    data1$`SECTOR`[data1$`SECTOR`==4] <- "Public two-years"
    data1$`SECTOR`[data1$`SECTOR`==5] <- "Nonprofit two-years"
    data1$`SECTOR`[data1$`SECTOR`==6] <- "For-profit two-years"
    data1$`SECTOR`[data1$`SECTOR`==7] <- "Public less-than-two-years"
    data1$`SECTOR`[data1$`SECTOR`==8] <- "Nonprofit less-than-two-years"
    data1$`SECTOR`[data1$`SECTOR`==9] <- "For-profit less-than-two-years"
    data1 <- data1 %>% mutate(
      `SECTOR` = factor(`SECTOR`, levels=c(
        "Public four-years",
        "Nonprofit four-years",
        "For-profit four-years",
        "Public two-years",
        "Nonprofit two-years",
        "For-profit two-years",
        "Public less-than-two-years",
        "Nonprofit less-than-two-years",
        "For-profit less-than-two-years"
      ))
    ) %>% rename(
      `For Facet` = `SECTOR`
    )
  }
  if(facetVar=="STABBR"){
    data1 <- data1 %>% rename(
      `For Facet` = `STABBR`
    ) %>% filter(
      (`For Facet` %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE
    )
  }
  
  data1 <- data1 %>% mutate(`Year` = as.numeric(`Year`))
  
  plot1 <- ggplot(
    data=data1, 
    mapping=aes(
      x=`Year`, y=`Change since 2009`, grouping=`Variable`, color=`Variable`
    ) 
  ) + geom_point() + geom_line() + scale_y_continuous(
    labels=percent_format(accuracy=1), limits=c(-2, 6)
  ) + scale_x_continuous(
    breaks=c(2009, 2016, 2023)
  ) + ggtitle(
    plotTitle
  ) + theme(
    legend.position="bottom"
  ) + scale_color_brewer(
    palette = "Dark2"
  )
  if(facetVar != "None"){
    plot1 <- plot1 + facet_wrap(`For Facet` ~ .)
  }
  
  return(plot1)
  
}

#### End #### 

#### Run plots ####

vizChange(data1=agg9, facetVar="None", plotTitle="Change over time in total grants to FTFT undergraduates, by grant type")
vizChange(data1=agg10, facetVar="SECTOR", plotTitle="Change over time in total grants to FTFT undergraduates, by grant type and sector")
vizChange(data1=agg11, facetVar="STABBR", plotTitle="Change over time in total grants to FTFT undergraduates, by grant type and state (public four-year sector)")
vizChange(data1=agg12, facetVar="STABBR", plotTitle="Change over time in total grants to FTFT undergraduates, by grant type and state (public two-year sector)")

#### End #### 

#### Special plot: Public four years, overall ####

agg100 <- agg10 %>% filter(
  `SECTOR`==1
) %>% mutate(
  `Year` = as.numeric(`Year`)
) %>% mutate(
  `Variable` = ifelse(
    `Variable`=="FTFT students", "First-time full-time students", `Variable`
  )
) %>% mutate(
  `Variable` = factor(`Variable`, levels=c(
    "Federal grants", 
    "State/local grants", 
    "Institutional grants", 
    "First-time full-time students"
  ))
) %>% rename(
  `Total` = `Variable`
)

ggplot(
  data=agg100, 
  mapping=aes(
    x=`Year`, y=`Change since 2009`, grouping=`Total`, color=`Total`
  ) 
) + geom_point(size=2.5) + geom_line(size=1.5) + scale_y_continuous(
  labels=percent_format(accuracy=1), limits=c(-0.1, 2.2)
) + scale_x_continuous(
  breaks=c(2009, 2016, 2023)
) + theme(
  legend.position="bottom"
) + scale_color_manual(
  values=c("#756fb3", "#d95f01", "#1c9e78", "#e72989")
) + theme(text = element_text(size = 13)) 

#### End #### 

#### Special plot: Public 2- and 4-years ####

agg101 <- agg10 %>% filter(
  `SECTOR` %in% c(1, 4)
) %>% mutate(
  `Year` = as.numeric(`Year`)
) %>% mutate(
  `Variable` = ifelse(
    `Variable`=="FTFT students", "First-time full-time students", `Variable`
  )
) %>% mutate(
  `Variable` = factor(`Variable`, levels=c(
    "Federal grants", 
    "State/local grants", 
    "Institutional grants", 
    "First-time full-time students"
  ))
) %>% rename(
  `Total` = `Variable`
) %>% mutate(
  `SECTOR` = ifelse(
    `SECTOR` == 1, "Public four-year", "Community college"
  )
) %>% mutate(
  `SECTOR` = factor(`SECTOR`, levels=c(
    "Public four-year", "Community college"
  ))
)

ggplot(
  data=agg101, 
  mapping=aes(
    x=`Year`, y=`Change since 2009`, grouping=`Total`, color=`Total`
  ) 
) + geom_point(size=2.5) + geom_line(size=1.5) + scale_y_continuous(
  labels=percent_format(accuracy=1), limits=c(-0.5, 2.2)
) + facet_grid(
  .~`SECTOR`
) + scale_x_continuous(
  breaks=c(2009, 2016, 2023)
) + theme(
  legend.position="bottom"
) + scale_color_manual(
  values=c("#756fb3", "#d95f01", "#1c9e78", "#e72989")
) + theme(text = element_text(size = 13)) 

#### End #### 

##########################################
#### Charts for report                ####
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

#### Share of undergraduates receiving federal Pell Grant, by income quartile ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_ofheti.csv", 
    startRow = 14, 
    endRow = 17, 
    keepWhichCol = 2, 
    rowVarName = "Income quartile", 
    colVarName = "Share receiving Pell Grant", 
    addlVar = FALSE, 
    addlVarValue = "NA", 
    addlVarName = "NA"
  )
) %>% mutate(
  `Share receiving Pell Grant` = `Share receiving Pell Grant`/ 100
) %>% mutate(
  `Income quartile` = factor(`Income quartile`, levels=c(
    "Bottom quartile", 
    "Lower-middle quartile", 
    "Upper-middle quartile", 
    "Top quartile"
  ))
) %>% mutate(
  `For Label` = percent(`Share receiving Pell Grant`, accuracy=0.1)
)

ggplot(
  data=forPlot, 
  mapping=aes(
    x=`Share receiving Pell Grant`, y=`Income quartile`
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

#### Shares of public four-year students receiving federal Pell Grant and state grants, by income quartile ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_ofheti.csv", 112, 115
# "PowerStats_zstora.csv", 112, 115

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_ofheti.csv", 
    startRow = 112, 
    endRow = 115, 
    keepWhichCol = 2, 
    rowVarName = "Income quartile", 
    colVarName = "Share receiving grant", 
    addlVar = TRUE, 
    addlVarValue = "Pell Grant", 
    addlVarName = "Grant type"
  ), 
  processDatalab(
    filename = "PowerStats_zstora.csv", 
    startRow = 112, 
    endRow = 115, 
    keepWhichCol = 2, 
    rowVarName = "Income quartile", 
    colVarName = "Share receiving grant", 
    addlVar = TRUE, 
    addlVarValue = "State grant", 
    addlVarName = "Grant type"
  )
) %>% mutate(
  `Share receiving grant` = `Share receiving grant`/ 100
) %>% mutate(
  `Income quartile` = factor(`Income quartile`, levels=c(
    "Bottom quartile", 
    "Lower-middle quartile", 
    "Upper-middle quartile", 
    "Top quartile"
  ))
) %>% mutate(
  `For Label` = percent(`Share receiving grant`, accuracy=0.1)
)

ggplot(
  data=forPlot, 
  mapping=aes(
    y=`Share receiving grant`, x=`Income quartile`, fill=`Grant type`
  )
) + geom_bar(
  stat="identity",
  position=position_dodge(width=0.55),
  width=0.3
) + scale_y_continuous(
  labels=percent_format(accuracy=1), 
  limits=c(0, 1), 
  breaks=c(0, 0.5, 1)
) + scale_fill_brewer(
  palette = "Dark2"
) + geom_text(
  aes(label = `For Label`), 
  position=position_dodge(width=0.55), 
  vjust=-0.5
) + theme(text = element_text(size = 13))

#### End #### 

#### Share of undergraduates at public four-year institutions receiving state grants, by institutional selectivity ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_vkdamq.csv", 123, 126

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_vkdamq.csv", 
    startRow = 123, 
    endRow = 126, 
    keepWhichCol = 2, 
    rowVarName = "Institution selectivity", 
    colVarName = "Share receiving state grants", 
    addlVar = FALSE, 
    addlVarValue = "NA", 
    addlVarName = "NA"
  )
) %>% mutate(
  `Institution selectivity` = factor(`Institution selectivity`, levels=c(
    "Open admission", 
    "Minimally selective", 
    "Moderately selective", 
    "Very selective"
  ))
) %>% mutate(
  `Share receiving state grants` = `Share receiving state grants` / 100
) %>% mutate(
  `For Label` = percent(`Share receiving state grants`, accuracy=0.1)
)

ggplot(
  data=forPlot, 
  mapping=aes(
    x=`Share receiving state grants`, y=`Institution selectivity`
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
) + theme(text = element_text(size = 13)) + scale_y_discrete(labels = label_wrap(8))

#### End #### 

#### Average institutional grant for public four-year students by in-state status and institution selectivity ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_pmtxyr.csv", 94, 95
# "PowerStats_pmtxyr.csv", 135, 136
# "PowerStats_pmtxyr.csv", 176, 177
# "PowerStats_pmtxyr.csv", 217, 218

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

#### Share of students receiving state grant awards, by need/merit basis, sector, and student’s Pell status ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_egctjm.csv", 53, 54
# "PowerStats_egctjm.csv", 92, 93
# "PowerStats_lfuxgh.csv", 53, 54
# "PowerStats_lfuxgh.csv", 92, 93

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

#### Share of students receiving institutional grant awards, by need/merit basis, sector, and student’s Pell status ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_rfbogb.csv", 53, 54
# "PowerStats_rfbogb.csv", 92, 93
# "PowerStats_pwplau.csv", 53, 54
# "PowerStats_pwplau.csv", 92, 93

forPlot1 <- rbind(
  processDatalab(
    filename = "PowerStats_rfbogb.csv", 
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
    filename = "PowerStats_rfbogb.csv", 
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
  `Grant type` = rep("Institutional non-need/ merit-based grants")
)

forPlot2 <- rbind(
  processDatalab(
    filename = "PowerStats_pwplau.csv", 
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
    filename = "PowerStats_pwplau.csv", 
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
  `Grant type` = rep("Institutional need-based grants")
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
) + theme(text = element_text(size = 13)) + scale_y_discrete(labels = label_wrap(25))

#### End #### 

#### Share of undergraduates with grants exceeding federal need, by income quartile ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_xrtupd.csv", 14, 17

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

#### Public four-year institutions’ median net student budget after all grants as a percentage of income, by family income quartile ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_npijyx.csv", 104, 107

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_npijyx.csv", 
    startRow = 104, 
    endRow = 107, 
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
  limits=c(0, 1.1), 
  breaks=c(0, 0.5, 1)
) + scale_fill_brewer(
  palette = "Dark2"
) + geom_text(
  aes(label = `For Label`), 
  nudge_x = 0.05
) + theme(text = element_text(size = 13)) + scale_y_discrete(labels = label_wrap(12))

#### End #### 

#### Public four-year institutions’ median net student budget after all grants as a percentage of income, by race/ethnicity ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_pjdnlj.csv", 134, 140

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_pjdnlj.csv", 
    startRow = 134, 
    endRow = 140, 
    keepWhichCol = 1, 
    rowVarName = "Race/ethnicity", 
    colVarName = "Net price after grants as a share of income", 
    addlVar = FALSE, 
    addlVarValue = "NA", 
    addlVarName = "NA"
  )
) %>% mutate(
  `Net price after grants as a share of income` = as.numeric(`Net price after grants as a share of income`) / 100
) %>% filter(
  is.na(`Net price after grants as a share of income`)==FALSE
) %>% mutate(
  `Race/ethnicity` = ifelse(
    `Race/ethnicity`=="American Indian or Alaska Native", "Native American", `Race/ethnicity`
  )
) %>% mutate(
  `Race/ethnicity` = factor(`Race/ethnicity`, levels=rev(c(
    "White", 
    "Hispanic or Latino", 
    "Black or African American", 
    "Asian", 
    "Native American",
    "More than one race"
  )))
) %>% mutate(
  `For Label` = percent(`Net price after grants as a share of income`, accuracy=1)
)

ggplot(
  data=forPlot, 
  mapping=aes(
    x=`Net price after grants as a share of income`, y=`Race/ethnicity`
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
) + theme(text = element_text(size = 13)) + scale_y_discrete(labels = label_wrap(17))

#### End #### 

#### Public four-year institutions’ median net student budget after all grants as a percentage of income, by parents’ highest education level ####

setwd("/Users/peter_granville/Net Price Equity/Other NPSAS")

# "PowerStats_seydvg.csv", 94, 96

forPlot <- rbind(
  processDatalab(
    filename = "PowerStats_seydvg.csv", 
    startRow = 94, 
    endRow = 96, 
    keepWhichCol = 1, 
    rowVarName = "Parents' highest education level", 
    colVarName = "Net price after grants as a share of income", 
    addlVar = FALSE, 
    addlVarValue = "NA", 
    addlVarName = "NA"
  )
) %>% mutate(
  `Net price after grants as a share of income` = `Net price after grants as a share of income` / 100
) %>% mutate(
  `Parents' highest education level` = ifelse(
    `Parents' highest education level`=="Middle school/junior high", "Middle school", `Parents' highest education level`
  )
) %>% mutate(
  `Parents' highest education level` = factor(`Parents' highest education level`, levels=rev(c(
    "College or beyond", 
    "High school",
    "Middle school"
  )))
) %>% mutate(
  `For Label` = percent(`Net price after grants as a share of income`, accuracy=1)
)

ggplot(
  data=forPlot, 
  mapping=aes(
    x=`Net price after grants as a share of income`, y=`Parents' highest education level`
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
) + theme(text = element_text(size = 13))  + scale_y_discrete(labels = label_wrap(17))

#### End #### 

##########################################
#### Special NPSAS run (20% FPL)      ####
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
    filename = "PowerStats_gyjbkq.csv", 
    startRow = 12, 
    endRow = 60, 
    inState = "Overall", 
    selectivity = "Overall"
  ), 
  loadNPSAS(
    filename = "PowerStats_gyjbkq.csv", 
    startRow = 554, 
    endRow = 602, 
    inState = "Overall", 
    selectivity = "Very selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_gyjbkq.csv", 
    startRow = 825, 
    endRow = 873, 
    inState = "Overall", 
    selectivity = "Moderately selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_gyjbkq.csv", 
    startRow = 1096, 
    endRow = 1144, 
    inState = "Overall", 
    selectivity = "Minimally selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_gyjbkq.csv", 
    startRow = 1367, 
    endRow = 1415, 
    inState = "Overall", 
    selectivity = "Open admission"
  ),
  
  # In-state
  loadNPSAS(
    filename = "PowerStats_hdrwqq.csv", 
    startRow = 12, 
    endRow = 60, 
    inState = "In-state", 
    selectivity = "Overall"
  ), 
  loadNPSAS(
    filename = "PowerStats_hdrwqq.csv", 
    startRow = 554, 
    endRow = 602, 
    inState = "In-state", 
    selectivity = "Very selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_hdrwqq.csv", 
    startRow = 825, 
    endRow = 873, 
    inState = "In-state", 
    selectivity = "Moderately selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_hdrwqq.csv", 
    startRow = 1096, 
    endRow = 1144, 
    inState = "In-state", 
    selectivity = "Minimally selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_hdrwqq.csv", 
    startRow = 1367, 
    endRow = 1415, 
    inState = "In-state", 
    selectivity = "Open admission"
  ), 
  
  # Out-of-state
  loadNPSAS(
    filename = "PowerStats_uctspm.csv", 
    startRow = 12, 
    endRow = 60, 
    inState = "Out-of-state", 
    selectivity = "Overall"
  ), 
  loadNPSAS(
    filename = "PowerStats_uctspm.csv", 
    startRow = 554, 
    endRow = 602, 
    inState = "Out-of-state", 
    selectivity = "Very selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_uctspm.csv", 
    startRow = 825, 
    endRow = 873, 
    inState = "Out-of-state", 
    selectivity = "Moderately selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_uctspm.csv", 
    startRow = 1096, 
    endRow = 1144, 
    inState = "Out-of-state", 
    selectivity = "Minimally selective"
  ), 
  loadNPSAS(
    filename = "PowerStats_uctspm.csv", 
    startRow = 1367, 
    endRow = 1415, 
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

data1$`Poverty bracket`[data1$`Poverty bracket`=="0 <= X <= 20"] <- "20"
data1$`Poverty bracket`[data1$`Poverty bracket`=="21 <= X <= 40"] <- "40"
data1$`Poverty bracket`[data1$`Poverty bracket`=="41 <= X <= 60"] <- "60"
data1$`Poverty bracket`[data1$`Poverty bracket`=="61 <= X <= 80"] <- "80"
data1$`Poverty bracket`[data1$`Poverty bracket`=="81 <= X <= 100"] <- "100"

data1$`Poverty bracket`[data1$`Poverty bracket`=="101 <= X <= 120"] <- "120"
data1$`Poverty bracket`[data1$`Poverty bracket`=="121 <= X <= 140"] <- "140"
data1$`Poverty bracket`[data1$`Poverty bracket`=="141 <= X <= 160"] <- "160"
data1$`Poverty bracket`[data1$`Poverty bracket`=="161 <= X <= 180"] <- "180"
data1$`Poverty bracket`[data1$`Poverty bracket`=="181 <= X <= 200"] <- "200"

data1$`Poverty bracket`[data1$`Poverty bracket`=="201 <= X <= 220"] <- "220"
data1$`Poverty bracket`[data1$`Poverty bracket`=="221 <= X <= 240"] <- "240"
data1$`Poverty bracket`[data1$`Poverty bracket`=="241 <= X <= 260"] <- "260"
data1$`Poverty bracket`[data1$`Poverty bracket`=="261 <= X <= 280"] <- "280"
data1$`Poverty bracket`[data1$`Poverty bracket`=="281 <= X <= 300"] <- "300"

data1$`Poverty bracket`[data1$`Poverty bracket`=="301 <= X <= 320"] <- "320"
data1$`Poverty bracket`[data1$`Poverty bracket`=="321 <= X <= 340"] <- "340"
data1$`Poverty bracket`[data1$`Poverty bracket`=="341 <= X <= 360"] <- "360"
data1$`Poverty bracket`[data1$`Poverty bracket`=="361 <= X <= 380"] <- "380"
data1$`Poverty bracket`[data1$`Poverty bracket`=="381 <= X <= 400"] <- "400"

data1$`Poverty bracket`[data1$`Poverty bracket`=="401 <= X <= 420"] <- "420"
data1$`Poverty bracket`[data1$`Poverty bracket`=="421 <= X <= 440"] <- "440"
data1$`Poverty bracket`[data1$`Poverty bracket`=="441 <= X <= 460"] <- "460"
data1$`Poverty bracket`[data1$`Poverty bracket`=="461 <= X <= 480"] <- "480"
data1$`Poverty bracket`[data1$`Poverty bracket`=="481 <= X <= 500"] <- "500"

data1$`Poverty bracket`[data1$`Poverty bracket`=="501 <= X <= 520"] <- "520"
data1$`Poverty bracket`[data1$`Poverty bracket`=="521 <= X <= 540"] <- "540"
data1$`Poverty bracket`[data1$`Poverty bracket`=="541 <= X <= 560"] <- "560"
data1$`Poverty bracket`[data1$`Poverty bracket`=="561 <= X <= 580"] <- "580"
data1$`Poverty bracket`[data1$`Poverty bracket`=="581 <= X <= 600"] <- "600"

data1$`Poverty bracket`[data1$`Poverty bracket`=="601 <= X <= 620"] <- "620"
data1$`Poverty bracket`[data1$`Poverty bracket`=="621 <= X <= 640"] <- "640"
data1$`Poverty bracket`[data1$`Poverty bracket`=="641 <= X <= 660"] <- "660"
data1$`Poverty bracket`[data1$`Poverty bracket`=="661 <= X <= 680"] <- "680"
data1$`Poverty bracket`[data1$`Poverty bracket`=="681 <= X <= 700"] <- "700"

data1$`Poverty bracket`[data1$`Poverty bracket`=="701 <= X <= 720"] <- "720"
data1$`Poverty bracket`[data1$`Poverty bracket`=="721 <= X <= 740"] <- "740"
data1$`Poverty bracket`[data1$`Poverty bracket`=="741 <= X <= 760"] <- "760"
data1$`Poverty bracket`[data1$`Poverty bracket`=="761 <= X <= 780"] <- "780"
data1$`Poverty bracket`[data1$`Poverty bracket`=="781 <= X <= 800"] <- "800"

data1$`Poverty bracket`[data1$`Poverty bracket`=="801 <= X <= 820"] <- "820"
data1$`Poverty bracket`[data1$`Poverty bracket`=="821 <= X <= 840"] <- "840"
data1$`Poverty bracket`[data1$`Poverty bracket`=="841 <= X <= 860"] <- "860"
data1$`Poverty bracket`[data1$`Poverty bracket`=="861 <= X <= 880"] <- "880"
data1$`Poverty bracket`[data1$`Poverty bracket`=="881 <= X <= 900"] <- "900"

data1$`Poverty bracket`[data1$`Poverty bracket`=="901 <= X <= 920"] <- "920"
data1$`Poverty bracket`[data1$`Poverty bracket`=="921 <= X <= 940"] <- "940"
data1$`Poverty bracket`[data1$`Poverty bracket`=="941 <= X <= 960"] <- "960"
data1$`Poverty bracket`[data1$`Poverty bracket`=="961 <= X <= 980"] <- "980"
data1$`Poverty bracket`[data1$`Poverty bracket`=="981 <= X <= 1000"] <- "1000"

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

#### End #### 

#### Visualize data ####

data2 <- data1 %>% filter(
  `Poverty bracket` <= 750
) %>% mutate(
  `Grant type` = factor(`Grant type`, levels=rev(levels(`Grant type`)))
)

ggplot(
  data=data2, 
  mapping=aes(
     x=`Poverty bracket`, y=`Amount`, fill=`Grant type`
  )
) + geom_bar(
  position="stack", stat="identity"
) + facet_grid(
  `In-state status` ~ `Selectivity`
) + scale_y_continuous(
  labels=dollar_format(accuracy=1)
) + scale_fill_brewer(
  palette = "Dark2"
)

#### End #### 

##########################################
#### Special NPSAS run (40% FPL)      ####
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

#### End #### 

#### Visualize data ####

data2 <- data1 %>% filter(
  `Poverty bracket` <= 800
) %>% mutate(
  `Grant type` = factor(`Grant type`, levels=rev(levels(`Grant type`)))
)

ggplot(
  data=data2, 
  mapping=aes(
    x=`Poverty bracket`, y=`Amount`, fill=`Grant type`
  )
) + geom_bar(
  position="stack", stat="identity"
) + facet_grid(
  `In-state status` ~ `Selectivity`
) + scale_y_continuous(
  labels=dollar_format(accuracy=1)
) + scale_fill_brewer(
  palette = "Dark2"
)

#### End #### 

#### Special plot: Very selective vs Open admission ####

data20 <- data2 %>% filter(
  `In-state status`=="Overall", 
  `Selectivity` %in% c("Open admission", "Very selective"), 
  `Poverty bracket` <= 610
) %>% mutate(
  `Poverty bracket` = `Poverty bracket` / 100
)

ggplot(
  data=data20, 
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

#### Special plot: In-state vs Out of state ####

data21 <- data2 %>% filter(
  `In-state status` %in% c("In-state", "Out-of-state"), 
  `Selectivity`=="Overall", 
  `Poverty bracket` <= 610
) %>% mutate(
  `Poverty bracket` = `Poverty bracket` / 100
)

ggplot(
  data=data21, 
  mapping=aes(
    x=`Poverty bracket`, y=`Amount`, fill=`Grant type`
  )
) + geom_bar(
  position="stack", stat="identity"
) + facet_grid(
  . ~ `In-state status`
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

#### Special plot: Overall ####

data22 <- data2 %>% filter(
  `In-state status`=="Overall", 
  `Selectivity`=="Overall", 
  `Poverty bracket` <= 610
) %>% mutate(
  `Poverty bracket` = `Poverty bracket` / 100
)

ggplot(
  data=data22, 
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

test <- data22 %>% filter(
  `In-state status` == "Overall", 
  `Selectivity` == "Overall"
) %>% pivot_wider(
  id_cols=c(`Poverty bracket`), 
  names_from=`Grant type`, 
  values_from=`Amount`
) %>% mutate(
  `State and institutional` = `Average state grant` + `Average institutional grant`
)

#### End #### 

