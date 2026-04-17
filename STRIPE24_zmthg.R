#STRIPE 2024 Zebra Mussel THg

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(ggsignif)

#pull up data
setwd("~/Desktop/USGS/Data/")

zmthg <- read_csv("STRIPE_24_zmthg_averaged.csv")


setwd("~/Desktop/stripe_2024/data")
meta <- read_csv("stripe_24_meta.csv")

#JOIN DATASETS

joined_zmthg <- merge(zmthg, meta ,by= "BarcodeID" )

joined_zmthg$date <- mdy(joined_zmthg$date)


#dry mass and wet mass

setwd("~/Desktop/stripe_2024/data")

tares_ww <- read_csv("stripe24_tares_ww.csv")
dry <- read_csv("stripe24_dry_mass.csv")

wdtare <- merge(tares_ww, dry ,by= "BarcodeID" )


#adding columns for tared wet mass, tared dry mass, % moisture
wt <- wdtare$wetmass.tared.g <- (wdtare$wet.mass.g - wdtare$tare.mass.g)
dt <- wdtare$drymass.tared.g <- (wdtare$dry.mass.g - wdtare$tare.mass.g)
wdtare$pct_moisture <- ((wt-dt)/wt)

write.csv(wdtare,"stripe24_pct_moist.csv", row.names = FALSE)

#CALCULATING WET THG NG/G
wet_zmthg <- merge(joined_zmthg, wdtare ,by= "BarcodeID" )

# Cw = Cd X [(100 - %H) / 100]

cd = wet_zmthg$thg.dry.ng.g
h = wet_zmthg$pct_moisture

wet_zmthg$thg.wet.ng.g <- NA

wet_zmthg$thg.wet.ng.g <-   cd * ((100 - h) / 100)

ymd(wet_zmthg$date)

#establish turnover date
turnover <- as.numeric(as.Date("2024-10-23"))

#dry THg vs time
dryc_v_date <- ggplot(wet_zmthg, aes(date, thg.dry.ng.g, color= size))+
  geom_point(data= wet_zmthg, 
             aes(x=date , 
                 y= thg.dry.ng.g ))+
  labs(title = "ZM THg Dry Concentration 2024", 
       x = "Date", y = "THg Dry ng/g")+
  geom_smooth()+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)
dryc_v_date

#wet weight vs time
wetc_v_date <- ggplot(wet_zmthg, aes(date, thg.wet.ng.g, color= size))+
  geom_point(data= wet_zmthg, 
             aes(x=date , 
                 y= thg.wet.ng.g, color= size))
wetc_v_date+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  labs(title = "ZM THg Wet Concentration 2024", 
       x = "Date", y = "THg Wet ng/g")
 
# making csv for wet thg concentration

getwd()
write.csv(wet_zmthg,"wet_zmthg.csv", 
          row.names = FALSE)

  #annotate("text", label = "Turnover",
        # x = turnover,
        # y = 115)

#geom_label(x= turnover, y=115, label="Turnover")
  
#manual colors

wetc_v_date <- ggplot(wet_zmthg, aes(date, thg.wet.ng.g))+
  geom_point(data= wet_zmthg, 
             aes(x=date , 
                 y= thg.wet.ng.g), color= wet_zmthg$size)+ 
  scale_color_manual(values=c("#E69F00", "#56B4E9"))
  

wetc_v_date+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+
  geom_label(x= turnover, y=115, label="Turnover")

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

#fitting a curve

#plotting setup
par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots

#setup
RMSE <- data.frame("kth_order" = NA, 
                   "RMSE" = NA) # empty data frame to store RMSE
vals <- list("x" <- seq(min(wet_zmthg$date), 
                        max(wet_zmthg$thg.wet.ng.g), 
                        by = 0.01)) # set up vector used for prediction

#should max be y value or x?

k <- c(1, 2, 3, 5, 9, 14) # k-th order


for (i in 1:length(k)) {
  # build models
  model <- lm(thg.wet.ng.g ~ poly(date, k[i]), data = wet_zmthg)
  
  #setup
  RMSE <- data.frame("kth_order" = NA, "RMSE" = NA) # empty data frame to store RMSE
  vals <- list("x" <- seq(min(wet_zmthg$date), max(wet_zmthg$thg.wet.ng.g), by = 0.01)) # set up vector used for prediction
  
  # calculate RMSE and store it for further usage
  RMSE[i, 1] <- k[i] # store k-th order
  RMSE[i, 2] <- sqrt(sum((fitted(model) - wet_zmthg$thg.wet.ng.g)^2) / length(wet_zmthg$thg.wet.ng.g)) # calculate RMSE
  
  # predict
  predictions <- predict(model, newdata = vals)
  # plot
  plot(wet_zmthg$date, wet_zmthg$thg.wet.ng.g, pch = 16, col = "blue",
       ylim = c(min(wet_zmthg$thg.wet.ng.g) * 1.3, max(wet_zmthg$thg.wet.ng.g) * 1.3))
  lines(vals[[1]], predictions, lwd = 2, col = "red")
  text(x = 0.8, y = 0.95, paste0("k = ", k[i], ", RMSE = ", round(RMSE[i, 2], 3))) # annotate the plot
}






