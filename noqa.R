# STRIPE 24
# NOT QA'D YET, JUST VISUALIZING

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(ggsignif)

#pull up data
getwd()
setwd("..")
setwd("~/Desktop/USGS/Data/not_qad")

noqa <- read_csv("noqa_stripe24.csv")


setwd("..")
setwd("stripe_2024/data")
meta <- read_csv("stripe_24_meta.csv")

#JOIN DATASETS

joined_noqa <- merge(noqa, meta ,by= "BarcodeID" )

joined_noqa$date <- mdy(joined_noqa$date)



  
#CALCULATING WET THG NG/G
wet_noqa <- merge(joined_noqa, wdtare ,by= "BarcodeID" )

# Cw = Cd X [(100 - %H) / 100]

cd = wet_noqa$thg.ng.g 
h = wet_noqa$pct_moisture
  
wet_noqa$thg.wet.ng.g <-   cd * ((100 - h) / 100)

ymd(wet_noqa$date)

#dry weight vs time
dryc_v_date <- ggplot(wet_noqa, aes(date, thg.ng.g),color= wet_noqa$size)+
  geom_point(data= wet_noqa, 
             aes(x=date , 
                 y= thg.ng.g ), color= wet_noqa$size)+
  labs(title = "ZM THg Concentration 2024", 
       x = "Date", y = "THg Wet ng/g")+
  geom_smooth()+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)
dryc_v_date

#wet weight vs time
wetc_v_date <- ggplot(wet_noqa, aes(date, thg.wet.ng.g))+
  geom_point(data= wet_noqa, 
                        aes(x=date , 
                            y= thg.wet.ng.g), color= wet_noqa$size)
wetc_v_date+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+
  geom_label(x= turnover, y=121, label="Turnover")+
legend("topleft",
       legend = c("Group 1", "Group 2"),
       col = 1:2, 
       pch = 16)

#filter by size
#6-13 mm

wet_noqa %>% filter(size == "6-13 mm") -> small

small_wetc <- ggplot(small, aes(date, thg.wet.ng.g))+
  geom_point(data= wet_noqa, 
             aes(x=date , 
                 y= thg.wet.ng.g))

small_wetc+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+
labs(title = "6-13 mm", 
     x = "Date", y = "THg Wet ng/g")+
  geom_label(x= turnover, y=121, label="Turnover")+
  labs(title = "6-13 mm", 
       x = "Date", y = "THg Wet ng/g")
  

#13-20

turnover <- as.numeric(as.Date("2024-10-23"))

wet_noqa %>% filter(size == "13-20 mm") -> large

large_wetc <- ggplot(large, aes(date, thg.wet.ng.g))+
  geom_point(data= large, 
             aes(x=date , 
                 y= thg.wet.ng.g))
large_wetc+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+
  labs(title = "13-20 mm", 
       x = "Date", y = "THg Wet ng/g")+
geom_label(x= turnover, y=121, label="Turnover")




