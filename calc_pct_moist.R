#dry mass and wet mass

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)
library(tidyverse)

getwd()
setwd("Desktop/stripe_2024/data")

tares_ww <- read_csv("stripe24_tares_ww.csv")
dry <- read_csv("stripe24_dry_mass.csv")

wdtare <- merge(tares_ww, dry ,by= "BarcodeID" )


#adding columns for tared wet mass, tared dry mass, % moisture
wt <- wdtare$wetmass.tared.g <- (wdtare$wet.mass.g - wdtare$tare.mass.g)
dt <- wdtare$drymass.tared.g <- (wdtare$dry.mass.g - wdtare$tare.mass.g)
wdtare$pct_moisture <- ((wt-dt)/wt)

write.csv(wdtare,"stripe24_pct_moist.csv", row.names = FALSE)

