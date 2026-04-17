# Package ID: knb-lter-ntl.392.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake Mendota, Wisconsin, USA, Zebra Mussel Veliger Water Column Density 2016-2019.
# Data set creator:  Michael Spear - Center for Limnology, University of Wisconsin-Madison 
# Data set creator:  Petra Wakker - Center for Limnology, University of Wisconsin-Madison 
# Data set creator:  Jake Vander Zanden - Center for Limnology, University of Wisconsin-Madison 
# Contact:  Jake Vander Zanden -  Center for Limnology, University of Wisconsin-Madison  - mjvanderzand@wisc.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")
	      

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/392/2/c20adca8cecac33770b0d659da7267ff" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "date",     
                    "year",     
                    "transect",     
                    "replicate",     
                    "depth",     
                    "initial_scan",     
                    "vol_sample",     
                    "mean_count_ml",     
                    "vol_watercolumn",     
                    "density_m3"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt1$year)=="factor") dt1$year <-as.numeric(levels(dt1$year))[as.integer(dt1$year) ]               
if (class(dt1$year)=="character") dt1$year <-as.numeric(dt1$year)
if (class(dt1$transect)!="factor") dt1$transect<- as.factor(dt1$transect)
if (class(dt1$replicate)=="factor") dt1$replicate <-as.numeric(levels(dt1$replicate))[as.integer(dt1$replicate) ]               
if (class(dt1$replicate)=="character") dt1$replicate <-as.numeric(dt1$replicate)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$initial_scan)!="factor") dt1$initial_scan<- as.factor(dt1$initial_scan)
if (class(dt1$vol_sample)=="factor") dt1$vol_sample <-as.numeric(levels(dt1$vol_sample))[as.integer(dt1$vol_sample) ]               
if (class(dt1$vol_sample)=="character") dt1$vol_sample <-as.numeric(dt1$vol_sample)
if (class(dt1$mean_count_ml)=="factor") dt1$mean_count_ml <-as.numeric(levels(dt1$mean_count_ml))[as.integer(dt1$mean_count_ml) ]               
if (class(dt1$mean_count_ml)=="character") dt1$mean_count_ml <-as.numeric(dt1$mean_count_ml)
if (class(dt1$vol_watercolumn)=="factor") dt1$vol_watercolumn <-as.numeric(levels(dt1$vol_watercolumn))[as.integer(dt1$vol_watercolumn) ]               
if (class(dt1$vol_watercolumn)=="character") dt1$vol_watercolumn <-as.numeric(dt1$vol_watercolumn)
if (class(dt1$density_m3)=="factor") dt1$density_m3 <-as.numeric(levels(dt1$density_m3))[as.integer(dt1$density_m3) ]               
if (class(dt1$density_m3)=="character") dt1$density_m3 <-as.numeric(dt1$density_m3)
                
# Convert Missing Values to NA for non-dates
                
dt1$year <- ifelse((trimws(as.character(dt1$year))==trimws("NA")),NA,dt1$year)               
suppressWarnings(dt1$year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$year))==as.character(as.numeric("NA"))),NA,dt1$year))
dt1$replicate <- ifelse((trimws(as.character(dt1$replicate))==trimws("NA")),NA,dt1$replicate)               
suppressWarnings(dt1$replicate <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$replicate))==as.character(as.numeric("NA"))),NA,dt1$replicate))
dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NA")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NA"))),NA,dt1$depth))
dt1$vol_sample <- ifelse((trimws(as.character(dt1$vol_sample))==trimws("NA")),NA,dt1$vol_sample)               
suppressWarnings(dt1$vol_sample <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$vol_sample))==as.character(as.numeric("NA"))),NA,dt1$vol_sample))
dt1$mean_count_ml <- ifelse((trimws(as.character(dt1$mean_count_ml))==trimws("NA")),NA,dt1$mean_count_ml)               
suppressWarnings(dt1$mean_count_ml <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_count_ml))==as.character(as.numeric("NA"))),NA,dt1$mean_count_ml))
dt1$vol_watercolumn <- ifelse((trimws(as.character(dt1$vol_watercolumn))==trimws("NA")),NA,dt1$vol_watercolumn)               
suppressWarnings(dt1$vol_watercolumn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$vol_watercolumn))==as.character(as.numeric("NA"))),NA,dt1$vol_watercolumn))
dt1$density_m3 <- ifelse((trimws(as.character(dt1$density_m3))==trimws("NA")),NA,dt1$density_m3)               
suppressWarnings(dt1$density_m3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$density_m3))==as.character(as.numeric("NA"))),NA,dt1$density_m3))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(date)
summary(year)
summary(transect)
summary(replicate)
summary(depth)
summary(initial_scan)
summary(vol_sample)
summary(mean_count_ml)
summary(vol_watercolumn)
summary(density_m3) 
                # Get more details on character variables
                 
summary(as.factor(dt1$transect)) 
summary(as.factor(dt1$initial_scan))
detach(dt1)               

library(stringr)
library(ggplot2)
library(dplyr)

#plot 2016

 dt1 %>% filter(year == "2016") -> Y2016

p1 <- ggplot(Y2016, aes(date, density_m3))+
  geom_point(data= Y2016, 
             aes(x=date , 
                 y= density_m3, color= transect))+
 labs(title = "Veliger Density 2016", 
       x = "Date", y = "Veligers/ m3")+
  scale_color_manual(values=c("orange", "#56B4E9", "purple"))+
  stat_summary(aes(x=date, y=density_m3, color=transect),  fun = mean, 
                   geom= "line")
  p1
  #geom_smooth(se= FALSE, aes(color= transect))
  
  #geom_line(aes(group = transect, color = transect))


p1

  geom_smooth()+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)
  
  #plot 2017
  
  dt1 %>% filter(year == "2017") -> Y2017
  
 p2 <-  ggplot(Y2017, aes(date, density_m3))+
    geom_point(data= Y2017, 
               aes(x=date , 
                   y= density_m3, color= transect))+
    labs(title = "Veliger Density 2017", 
         x = "Date", y = "Veligers/ m3")+
    scale_color_manual(values=c("orange", "#56B4E9", "purple"))+ 
   stat_summary(aes(x=date, y=density_m3, color=transect),  
                fun.data = mean_se, 
                geom= "line")
  
  p2 
  #plot 2018

  dt1 %>% filter(year == "2018") -> Y2018
  
  p3 <- ggplot(Y2018, aes(date, density_m3))+
    geom_point(data= Y2018, 
               aes(x=date , 
                   y= density_m3, color= transect))+
    labs(title = "Veliger Density 2018", 
         x = "Date", y = "Veligers/ m3")+
    scale_color_manual(values=c("orange", "#56B4E9", "purple"))+ 
    stat_summary(aes(x=date, y=density_m3, color=transect),  
                 fun.data = mean_se, 
                 geom= "line")

  p3
  
  
  #plot 2019
  dt1 %>% filter(year == "2019") -> Y2019
  
  p4 <- ggplot(Y2019, aes(date, density_m3))+
    geom_point(data= Y2019, 
               aes(x=date , 
                   y= density_m3, color= transect))+
    labs(title = "Veliger Density 2019", 
         x = "Date", y = "Veligers/ m3")+
    scale_color_manual(values=c("orange", "#56B4E9", "purple"))+ 
    stat_summary(aes(x=date, y=density_m3, color=transect),  
                 fun.data = mean_se, 
                 geom= "line")
  
  p4
#cowplot
  library(cowplot)
  
  plot_grid (p1, p2, p3, p4, labels="AUTO")

