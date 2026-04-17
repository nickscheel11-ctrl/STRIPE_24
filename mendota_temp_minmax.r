# Package ID: knb-lter-ntl.439.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Multiparameter Water Quality Data -- CFL Pier, Lake Mendota..
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")
	      

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/439/1/a92a4cf4ae52d4a318c258976431bc54" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "sampledate",     
                    "depth",     
                    "wtemp",     
                    "do_raw",     
                    "do_sat",     
                    "chlor_rfu",     
                    "phyco_rfu",     
                    "ph",     
                    "spec_cond",     
                    "turbidity",     
                    "fdom_rfu",     
                    "flag_wtemp",     
                    "flag_do_raw",     
                    "flag_do_sat",     
                    "flag_chlor_rfu",     
                    "flag_phyco_rfu",     
                    "flag_ph",     
                    "flag_spec_cond",     
                    "flag_turbidity",     
                    "flag_fdom_rfu"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$do_raw)=="factor") dt1$do_raw <-as.numeric(levels(dt1$do_raw))[as.integer(dt1$do_raw) ]               
if (class(dt1$do_raw)=="character") dt1$do_raw <-as.numeric(dt1$do_raw)
if (class(dt1$do_sat)=="factor") dt1$do_sat <-as.numeric(levels(dt1$do_sat))[as.integer(dt1$do_sat) ]               
if (class(dt1$do_sat)=="character") dt1$do_sat <-as.numeric(dt1$do_sat)
if (class(dt1$chlor_rfu)=="factor") dt1$chlor_rfu <-as.numeric(levels(dt1$chlor_rfu))[as.integer(dt1$chlor_rfu) ]               
if (class(dt1$chlor_rfu)=="character") dt1$chlor_rfu <-as.numeric(dt1$chlor_rfu)
if (class(dt1$phyco_rfu)=="factor") dt1$phyco_rfu <-as.numeric(levels(dt1$phyco_rfu))[as.integer(dt1$phyco_rfu) ]               
if (class(dt1$phyco_rfu)=="character") dt1$phyco_rfu <-as.numeric(dt1$phyco_rfu)
if (class(dt1$ph)=="factor") dt1$ph <-as.numeric(levels(dt1$ph))[as.integer(dt1$ph) ]               
if (class(dt1$ph)=="character") dt1$ph <-as.numeric(dt1$ph)
if (class(dt1$spec_cond)=="factor") dt1$spec_cond <-as.numeric(levels(dt1$spec_cond))[as.integer(dt1$spec_cond) ]               
if (class(dt1$spec_cond)=="character") dt1$spec_cond <-as.numeric(dt1$spec_cond)
if (class(dt1$turbidity)=="factor") dt1$turbidity <-as.numeric(levels(dt1$turbidity))[as.integer(dt1$turbidity) ]               
if (class(dt1$turbidity)=="character") dt1$turbidity <-as.numeric(dt1$turbidity)
if (class(dt1$fdom_rfu)=="factor") dt1$fdom_rfu <-as.numeric(levels(dt1$fdom_rfu))[as.integer(dt1$fdom_rfu) ]               
if (class(dt1$fdom_rfu)=="character") dt1$fdom_rfu <-as.numeric(dt1$fdom_rfu)
if (class(dt1$flag_wtemp)!="factor") dt1$flag_wtemp<- as.factor(dt1$flag_wtemp)
if (class(dt1$flag_do_raw)!="factor") dt1$flag_do_raw<- as.factor(dt1$flag_do_raw)
if (class(dt1$flag_do_sat)!="factor") dt1$flag_do_sat<- as.factor(dt1$flag_do_sat)
if (class(dt1$flag_chlor_rfu)!="factor") dt1$flag_chlor_rfu<- as.factor(dt1$flag_chlor_rfu)
if (class(dt1$flag_phyco_rfu)!="factor") dt1$flag_phyco_rfu<- as.factor(dt1$flag_phyco_rfu)
if (class(dt1$flag_ph)!="factor") dt1$flag_ph<- as.factor(dt1$flag_ph)
if (class(dt1$flag_spec_cond)!="factor") dt1$flag_spec_cond<- as.factor(dt1$flag_spec_cond)
if (class(dt1$flag_turbidity)!="factor") dt1$flag_turbidity<- as.factor(dt1$flag_turbidity)
if (class(dt1$flag_fdom_rfu)!="factor") dt1$flag_fdom_rfu<- as.factor(dt1$flag_fdom_rfu)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(depth)
summary(wtemp)
summary(do_raw)
summary(do_sat)
summary(chlor_rfu)
summary(phyco_rfu)
summary(ph)
summary(spec_cond)
summary(turbidity)
summary(fdom_rfu)
summary(flag_wtemp)
summary(flag_do_raw)
summary(flag_do_sat)
summary(flag_chlor_rfu)
summary(flag_phyco_rfu)
summary(flag_ph)
summary(flag_spec_cond)
summary(flag_turbidity)
summary(flag_fdom_rfu) 
                # Get more details on character variables
                 
summary(as.factor(dt1$flag_wtemp)) 
summary(as.factor(dt1$flag_do_raw)) 
summary(as.factor(dt1$flag_do_sat)) 
summary(as.factor(dt1$flag_chlor_rfu)) 
summary(as.factor(dt1$flag_phyco_rfu)) 
summary(as.factor(dt1$flag_ph)) 
summary(as.factor(dt1$flag_spec_cond)) 
summary(as.factor(dt1$flag_turbidity)) 
summary(as.factor(dt1$flag_fdom_rfu))
detach(dt1)               
        
	      

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/439/1/296381f8d86b43284277db78710ccc7c" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "sampledate",     
                    "hour",     
                    "depth",     
                    "wtemp",     
                    "do_raw",     
                    "do_sat",     
                    "chlor_rfu",     
                    "phyco_rfu",     
                    "ph",     
                    "spec_cond",     
                    "turbidity",     
                    "fdom_rfu",     
                    "flag_wtemp",     
                    "flag_do_raw",     
                    "flag_do_sat",     
                    "flag_chlor_rfu",     
                    "flag_phyco_rfu",     
                    "flag_ph",     
                    "flag_spec_cond",     
                    "flag_turbidity",     
                    "flag_fdom_rfu"    ), check.names=TRUE)
               
unlink(infile2)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                                                   
# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$sampledate != "",]) == length(tmp2sampledate[!is.na(tmp2sampledate)])){dt2$sampledate <- tmp2sampledate } else {print("Date conversion failed for dt2$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt2$hour)=="factor") dt2$hour <-as.numeric(levels(dt2$hour))[as.integer(dt2$hour) ]               
if (class(dt2$hour)=="character") dt2$hour <-as.numeric(dt2$hour)
if (class(dt2$depth)=="factor") dt2$depth <-as.numeric(levels(dt2$depth))[as.integer(dt2$depth) ]               
if (class(dt2$depth)=="character") dt2$depth <-as.numeric(dt2$depth)
if (class(dt2$wtemp)=="factor") dt2$wtemp <-as.numeric(levels(dt2$wtemp))[as.integer(dt2$wtemp) ]               
if (class(dt2$wtemp)=="character") dt2$wtemp <-as.numeric(dt2$wtemp)
if (class(dt2$do_raw)=="factor") dt2$do_raw <-as.numeric(levels(dt2$do_raw))[as.integer(dt2$do_raw) ]               
if (class(dt2$do_raw)=="character") dt2$do_raw <-as.numeric(dt2$do_raw)
if (class(dt2$do_sat)=="factor") dt2$do_sat <-as.numeric(levels(dt2$do_sat))[as.integer(dt2$do_sat) ]               
if (class(dt2$do_sat)=="character") dt2$do_sat <-as.numeric(dt2$do_sat)
if (class(dt2$chlor_rfu)=="factor") dt2$chlor_rfu <-as.numeric(levels(dt2$chlor_rfu))[as.integer(dt2$chlor_rfu) ]               
if (class(dt2$chlor_rfu)=="character") dt2$chlor_rfu <-as.numeric(dt2$chlor_rfu)
if (class(dt2$phyco_rfu)=="factor") dt2$phyco_rfu <-as.numeric(levels(dt2$phyco_rfu))[as.integer(dt2$phyco_rfu) ]               
if (class(dt2$phyco_rfu)=="character") dt2$phyco_rfu <-as.numeric(dt2$phyco_rfu)
if (class(dt2$ph)=="factor") dt2$ph <-as.numeric(levels(dt2$ph))[as.integer(dt2$ph) ]               
if (class(dt2$ph)=="character") dt2$ph <-as.numeric(dt2$ph)
if (class(dt2$spec_cond)=="factor") dt2$spec_cond <-as.numeric(levels(dt2$spec_cond))[as.integer(dt2$spec_cond) ]               
if (class(dt2$spec_cond)=="character") dt2$spec_cond <-as.numeric(dt2$spec_cond)
if (class(dt2$turbidity)=="factor") dt2$turbidity <-as.numeric(levels(dt2$turbidity))[as.integer(dt2$turbidity) ]               
if (class(dt2$turbidity)=="character") dt2$turbidity <-as.numeric(dt2$turbidity)
if (class(dt2$fdom_rfu)=="factor") dt2$fdom_rfu <-as.numeric(levels(dt2$fdom_rfu))[as.integer(dt2$fdom_rfu) ]               
if (class(dt2$fdom_rfu)=="character") dt2$fdom_rfu <-as.numeric(dt2$fdom_rfu)
if (class(dt2$flag_wtemp)!="factor") dt2$flag_wtemp<- as.factor(dt2$flag_wtemp)
if (class(dt2$flag_do_raw)!="factor") dt2$flag_do_raw<- as.factor(dt2$flag_do_raw)
if (class(dt2$flag_do_sat)!="factor") dt2$flag_do_sat<- as.factor(dt2$flag_do_sat)
if (class(dt2$flag_chlor_rfu)!="factor") dt2$flag_chlor_rfu<- as.factor(dt2$flag_chlor_rfu)
if (class(dt2$flag_phyco_rfu)!="factor") dt2$flag_phyco_rfu<- as.factor(dt2$flag_phyco_rfu)
if (class(dt2$flag_ph)!="factor") dt2$flag_ph<- as.factor(dt2$flag_ph)
if (class(dt2$flag_spec_cond)!="factor") dt2$flag_spec_cond<- as.factor(dt2$flag_spec_cond)
if (class(dt2$flag_turbidity)!="factor") dt2$flag_turbidity<- as.factor(dt2$flag_turbidity)
if (class(dt2$flag_fdom_rfu)!="factor") dt2$flag_fdom_rfu<- as.factor(dt2$flag_fdom_rfu)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(hour)
summary(depth)
summary(wtemp)
summary(do_raw)
summary(do_sat)
summary(chlor_rfu)
summary(phyco_rfu)
summary(ph)
summary(spec_cond)
summary(turbidity)
summary(fdom_rfu)
summary(flag_wtemp)
summary(flag_do_raw)
summary(flag_do_sat)
summary(flag_chlor_rfu)
summary(flag_phyco_rfu)
summary(flag_ph)
summary(flag_spec_cond)
summary(flag_turbidity)
summary(flag_fdom_rfu) 
                # Get more details on character variables
                 
summary(as.factor(dt2$flag_wtemp)) 
summary(as.factor(dt2$flag_do_raw)) 
summary(as.factor(dt2$flag_do_sat)) 
summary(as.factor(dt2$flag_chlor_rfu)) 
summary(as.factor(dt2$flag_phyco_rfu)) 
summary(as.factor(dt2$flag_ph)) 
summary(as.factor(dt2$flag_spec_cond)) 
summary(as.factor(dt2$flag_turbidity)) 
summary(as.factor(dt2$flag_fdom_rfu))
detach(dt2)               
        
	      

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/439/1/a30f5d7d2c266c6b520dc88fd9056b25" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")

                   
 dt3 <-read.csv(infile3,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "sampledate",     
                    "sampletime",     
                    "depth",     
                    "wtemp",     
                    "do_raw",     
                    "do_sat",     
                    "chlor_rfu",     
                    "phyco_rfu",     
                    "ph",     
                    "spec_cond",     
                    "turbidity",     
                    "fdom_rfu",     
                    "flag_wtemp",     
                    "flag_do_raw",     
                    "flag_do_sat",     
                    "flag_chlor_rfu",     
                    "flag_phyco_rfu",     
                    "flag_ph",     
                    "flag_spec_cond",     
                    "flag_turbidity",     
                    "flag_fdom_rfu"    ), check.names=TRUE)
               
unlink(infile3)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                                                   
# attempting to convert dt3$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3sampledate<-as.Date(dt3$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt3[dt3$sampledate != "",]) == length(tmp3sampledate[!is.na(tmp3sampledate)])){dt3$sampledate <- tmp3sampledate } else {print("Date conversion failed for dt3$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt3$depth)=="factor") dt3$depth <-as.numeric(levels(dt3$depth))[as.integer(dt3$depth) ]               
if (class(dt3$depth)=="character") dt3$depth <-as.numeric(dt3$depth)
if (class(dt3$wtemp)=="factor") dt3$wtemp <-as.numeric(levels(dt3$wtemp))[as.integer(dt3$wtemp) ]               
if (class(dt3$wtemp)=="character") dt3$wtemp <-as.numeric(dt3$wtemp)
if (class(dt3$do_raw)=="factor") dt3$do_raw <-as.numeric(levels(dt3$do_raw))[as.integer(dt3$do_raw) ]               
if (class(dt3$do_raw)=="character") dt3$do_raw <-as.numeric(dt3$do_raw)
if (class(dt3$do_sat)=="factor") dt3$do_sat <-as.numeric(levels(dt3$do_sat))[as.integer(dt3$do_sat) ]               
if (class(dt3$do_sat)=="character") dt3$do_sat <-as.numeric(dt3$do_sat)
if (class(dt3$chlor_rfu)=="factor") dt3$chlor_rfu <-as.numeric(levels(dt3$chlor_rfu))[as.integer(dt3$chlor_rfu) ]               
if (class(dt3$chlor_rfu)=="character") dt3$chlor_rfu <-as.numeric(dt3$chlor_rfu)
if (class(dt3$phyco_rfu)=="factor") dt3$phyco_rfu <-as.numeric(levels(dt3$phyco_rfu))[as.integer(dt3$phyco_rfu) ]               
if (class(dt3$phyco_rfu)=="character") dt3$phyco_rfu <-as.numeric(dt3$phyco_rfu)
if (class(dt3$ph)=="factor") dt3$ph <-as.numeric(levels(dt3$ph))[as.integer(dt3$ph) ]               
if (class(dt3$ph)=="character") dt3$ph <-as.numeric(dt3$ph)
if (class(dt3$spec_cond)=="factor") dt3$spec_cond <-as.numeric(levels(dt3$spec_cond))[as.integer(dt3$spec_cond) ]               
if (class(dt3$spec_cond)=="character") dt3$spec_cond <-as.numeric(dt3$spec_cond)
if (class(dt3$turbidity)=="factor") dt3$turbidity <-as.numeric(levels(dt3$turbidity))[as.integer(dt3$turbidity) ]               
if (class(dt3$turbidity)=="character") dt3$turbidity <-as.numeric(dt3$turbidity)
if (class(dt3$fdom_rfu)=="factor") dt3$fdom_rfu <-as.numeric(levels(dt3$fdom_rfu))[as.integer(dt3$fdom_rfu) ]               
if (class(dt3$fdom_rfu)=="character") dt3$fdom_rfu <-as.numeric(dt3$fdom_rfu)
if (class(dt3$flag_wtemp)!="factor") dt3$flag_wtemp<- as.factor(dt3$flag_wtemp)
if (class(dt3$flag_do_raw)!="factor") dt3$flag_do_raw<- as.factor(dt3$flag_do_raw)
if (class(dt3$flag_do_sat)!="factor") dt3$flag_do_sat<- as.factor(dt3$flag_do_sat)
if (class(dt3$flag_chlor_rfu)!="factor") dt3$flag_chlor_rfu<- as.factor(dt3$flag_chlor_rfu)
if (class(dt3$flag_phyco_rfu)!="factor") dt3$flag_phyco_rfu<- as.factor(dt3$flag_phyco_rfu)
if (class(dt3$flag_ph)!="factor") dt3$flag_ph<- as.factor(dt3$flag_ph)
if (class(dt3$flag_spec_cond)!="factor") dt3$flag_spec_cond<- as.factor(dt3$flag_spec_cond)
if (class(dt3$flag_turbidity)!="factor") dt3$flag_turbidity<- as.factor(dt3$flag_turbidity)
if (class(dt3$flag_fdom_rfu)!="factor") dt3$flag_fdom_rfu<- as.factor(dt3$flag_fdom_rfu)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(sampletime)
summary(depth)
summary(wtemp)
summary(do_raw)
summary(do_sat)
summary(chlor_rfu)
summary(phyco_rfu)
summary(ph)
summary(spec_cond)
summary(turbidity)
summary(fdom_rfu)
summary(flag_wtemp)
summary(flag_do_raw)
summary(flag_do_sat)
summary(flag_chlor_rfu)
summary(flag_phyco_rfu)
summary(flag_ph)
summary(flag_spec_cond)
summary(flag_turbidity)
summary(flag_fdom_rfu) 
                # Get more details on character variables
                 
summary(as.factor(dt3$flag_wtemp)) 
summary(as.factor(dt3$flag_do_raw)) 
summary(as.factor(dt3$flag_do_sat)) 
summary(as.factor(dt3$flag_chlor_rfu)) 
summary(as.factor(dt3$flag_phyco_rfu)) 
summary(as.factor(dt3$flag_ph)) 
summary(as.factor(dt3$flag_spec_cond)) 
summary(as.factor(dt3$flag_turbidity)) 
summary(as.factor(dt3$flag_fdom_rfu))
detach(dt3)               


#================================================================
#my analyses
#================================================================

library(lubridate)

dt2$sampledate %>% as.Date()
dt2$sampledate %>% yday() -> dt2$daynum
  
dt2 %>% filter(daynum > 181) -> sampdates
year_of_interest <- 2024
dt2_2024 <- dt2 %>% filter(year(sampledate) == year_of_interest)


ggplot(dt2_2024, aes(daynum, wtemp))+
geom_point()


#================================================================
#my analyses
#================================================================


library(dplyr)
library(lubridate)
library(ggplot2)

# Convert sampledate to Date and extract day number
dt2$sampledate <- as.Date(dt2$sampledate)
dt2$daynum <- yday(dt2$sampledate)

# Filter data for 2024
dt2_2024 <- dt2 %>% filter(year(sampledate) == 2024)

# Get max and min temperature for each day
daily_extremes <- dt2_2024 %>%
  group_by(daynum) %>%
  summarise(
    max_temp = max(wtemp, na.rm = TRUE),
    min_temp = min(wtemp, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(max_temp, min_temp), names_to = "type", values_to = "wtemp")

# Plot
ggplot(daily_extremes, aes(x = daynum, y = wtemp, color = type)) +
  geom_point() +
  labs(title = "Daily High and Low Water Temperature at 0.5m Depth", 
       x = "Day of Year", y = "Water Temperature (C)")+
  geom_vline(xintercept = 297, color = "red", linetype = "dashed") +
  annotate("text", x = 270, y = 16, label = "Early Fall", angle = 0, vjust = -0.5) +
  geom_vline(xintercept = 245, color = "red", linetype = "dashed") +
  annotate("text", x = 220, y = 16, label = "Summer", angle = 0, vjust = -0.5)+
  annotate("text", x = 307, y = 16, label = "Late Fall", angle = 0, vjust = -0.5)


