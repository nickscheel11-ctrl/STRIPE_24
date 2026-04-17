#spear and me

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)

#reading data in
setwd("Desktop/stripe_2024/data")
spear_and_me <- read_csv("spear_and_me.csv")

#shape file
sf <- ggplot()+ geom_sf(data = mendota_outline, linewidth = 1, col = "gray40")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(panel.background = element_blank())

#map 
dens <- data.frame(
  location = c("boathouse", "maple_bluff", "cfl_dock",
               "gov_island", "wally_bauman", "fox_bluff", "marshall_park"),

  zm_dens = c(476, 449, 3619, 1444, 912, 0, 49  ))

sf + geom_point(data= dens, color= "red", 
                aes(x=lon, y=lat, size=zm_dens))+ 
  labs(size = "Zebra Mussels/m²")


# dens v date
spear_and_me$date <- mdy(spear_and_me$date)

Dens_v_date <- ggplot(spear_and_me, aes(date, avg_dens_m2, color= location)) +
  geom_point(data= spear_and_me, aes(x=date , y= avg_dens_m2, color= location ))+
  geom_line()+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
 
  labs(title = "ZM Density", 
       x = "Date", y = "Density (individuals/m2)")

Dens_v_date

ggsave("zm_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#CFL
spear_and_me %>% filter(location == "cfl")-> cfl

cfl$date<-mdy(cfl$date)

cfl_plot <- ggplot(cfl, aes(date, avg_dens_m2)) +
  geom_point(data= cfl, aes(x=date , y= avg_dens_m2))+
  geom_line()+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  
  labs(title = "ZM Dens", 
       x = "Date", y = "Dens (ind/m2)")

cfl_plot


