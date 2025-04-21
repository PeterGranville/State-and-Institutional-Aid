
library(tidyverse)

setwd("/Users/peter_granville/Net Price Equity/Financial-Aid-Distributions")

boundsOG <- read.csv(
  "Axis-Bounds (Overall).csv", header=TRUE, check.names=FALSE
) %>% rename(
  `Lower bound for axis (overall)` = `Lower bound for axis`, 
  `Upper bound for axis (overall)` = `Upper bound for axis`
)

boundsIS <- read.csv(
  "Axis-Bounds (In-State).csv", header=TRUE, check.names=FALSE
) %>% rename(
  `Lower bound for axis (in-state)` = `Lower bound for axis`, 
  `Upper bound for axis (in-state)` = `Upper bound for axis`
)

bounds <- full_join(
  x=boundsOG, y=boundsIS, by="Target name"
) %>% mutate(
  `Lower bound for axis` = pmin(
    `Lower bound for axis (overall)`, 
    `Lower bound for axis (in-state)`
  ), 
  `Upper bound for axis` = pmax(
    `Upper bound for axis (overall)`, 
    `Upper bound for axis (in-state)`
  )
) %>% select(
  -(`Lower bound for axis (overall)`), 
  -(`Lower bound for axis (in-state)`),
  -(`Upper bound for axis (overall)`), 
  -(`Upper bound for axis (in-state)`)
)

write.csv(bounds, "Axis-Bounds.csv", row.names=FALSE)
rm(bounds, boundsOG, boundsIS)