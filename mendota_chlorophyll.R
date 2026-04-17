
#ntl chlorophyll data

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)
library(tidyverse)

#pull up data
setwd("Desktop/ntl_data")

chlorophyll <- read_csv("chlorophyll.csv")

#only mendota depth 0-2 m
chlorophyll %>% filter(lakeid == "ME") -> chlorophyll
chlorophyll %>% filter(depth_range_m == "0-2") -> chlorophyll


#after 2010
chlorophyll_10 <- subset(chlorophyll, year4>2010)

#plotting

chl_v_date <- ggplot(chlorophyll_10, aes(sampledate, tri_chl_spec))
chl_v_date + 
  geom_line(data= , 
            aes(x=sampledate , 
                y= tri_chl_spec))+
  geom_hline(yintercept = 13, 
             color = "red", linetype = "dashed", size = 1)



