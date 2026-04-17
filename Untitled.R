
library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)

#reading data in
setwd("~/Desktop/stripe_2024/data")
ysi <- read_csv("stripe_ysi_24_1m.csv")

# temperature vs date
ysi$Date <- as.Date(mdy(ysi$Date))

#daynum
ysi$daynum<- yday(ysi$Date)


temp_v_date <- ggplot(ysi, aes(daynum, Temp.C)) +
  geom_line(data= ysi, aes(x=daynum , y= Temp.C ))+
  geom_point(data= ysi, aes(x=daynum , y= Temp.C ))+
  geom_vline(xintercept = 297, 
             color = "red", linetype = "dashed", size = 1)+
 # annotate("text", x = 297, y = 18, label = "Turnover",
  #         angle = 90, vjust = 1.5)+
  labs(title = "Water Temp at 1m", 
       x = "Day Number", y = "Temperature (C)")+
  geom_vline(xintercept = 245, 
             color = "red", linetype = "dashed", size = 1)+
  
 # annotate("text", x = 245, y = 18, label = "End of Summer", 
    #       angle = 90, vjust = -0.5)+

labs(title = "Water Temperature at 1m During Sampling Events", x = "Day of Year", y = "Temp (C)") +
  geom_vline(xintercept = 297, color = "red", linetype = "dashed") +
  annotate("text", x = 270, y = 24, label = "Early Fall", angle = 0, vjust = -0.5) +
  geom_vline(xintercept = 245, color = "red", linetype = "dashed") +
  annotate("text", x = 220, y = 24, label = "Summer", angle = 0, vjust = -0.5)+
  annotate("text", x = 320, y = 24, label = "Late Fall", angle = 0, vjust = -0.5)

temp_v_date

ggsave("temp_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#DO v date

DO_v_date <- ggplot(ysi, aes(Date, DO.percent)) +
  geom_point(data= ysi, aes(x=Date , y= DO.percent))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "%DO at 1m", 
       x = "Date", y = "% DO")

DO_v_date

ggsave("DO_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#DO mg.L 
DOmgl_v_date <- ggplot(ysi, aes(Date, DO.mg.L)) +
  geom_point(data= ysi, aes(x=Date , y= DO.mg.L))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "DO.mg.L at 1m", 
       x = "Date", y = "DO.mg.L")

DOmgl_v_date

ggsave("DOmgl_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#pH v Date
pH_v_date <- ggplot(ysi, aes(Date, pH)) +
  geom_point(data= ysi, aes(x=Date , y= pH))+
  geom_line(data= ysi, aes(x=Date , y= pH))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "pH at 1m", 
       x = "Date", y = "pH")

pH_v_date

ggsave("pH_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#SPC v Date
spc_v_date <- ggplot(ysi, aes(Date, SPC.uS.cm)) +
  geom_point(data= ysi, aes(x=Date , y= SPC.uS.cm))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "SPC at 1m", 
       x = "Date", y = "SPC.uS.cm")


spc_v_date

ggsave("pH_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')


#join ysi and zmthg

zm_temp <- merge(wet_zmthg, ysi , by.x= "date", by.y = "Date", all.x = TRUE )

write.csv(zm_temp,"thg_temp_2024.csv", row.names = FALSE)

wet_zmthg$daynum<- yday(wet_zmthg$date)

citation("tidyverse")
