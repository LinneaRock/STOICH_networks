# Pull data from EDI

# Package ID: knb-lter-nwt.169.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Streamflow data for outlet from Navajo meadow, 1994 - 2014.
# Data set creator:  Nelson Caine -  
# Data set creator:    - Niwot Ridge LTER 
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/169/1/71f3218e99ce4a704345855850e167d2" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "LTER_site",     
                 "local_site",     
                 "date",     
                 "discharge",     
                 "notes"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$LTER_site)!="factor") dt1$LTER_site<- as.factor(dt1$LTER_site)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$discharge)=="factor") dt1$discharge <-as.numeric(levels(dt1$discharge))[as.integer(dt1$discharge) ]               
if (class(dt1$discharge)=="character") dt1$discharge <-as.numeric(dt1$discharge)
if (class(dt1$notes)!="factor") dt1$notes<- as.factor(dt1$notes)

# Convert Missing Values to NA for non-dates

dt1$discharge <- ifelse((trimws(as.character(dt1$discharge))==trimws("NaN")),NA,dt1$discharge)               
suppressWarnings(dt1$discharge <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$discharge))==as.character(as.numeric("NaN"))),NA,dt1$discharge))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(local_site)
summary(date)
summary(discharge)
summary(notes) 
# Get more details on character variables

summary(as.factor(dt1$LTER_site)) 
summary(as.factor(dt1$local_site)) 
summary(as.factor(dt1$notes))
detach(dt1)               




# keep data we want ####
str(dt1)

ARIKAREE_Discharge <- dt1 |>
  mutate(location = 'Glacier') |> # this is discharge we'll match with arikaree nutrients -- but they are not actually collected at same location - make note in methods
  rename(discharge_vol_cm = discharge)|> # discharge in volume cubic meters
  select(local_site,location,date,discharge_vol_cm)

write.csv(ARIKAREE_Discharge, 'Data/DataPullEDI/knb-lter-nwt.169.1_ARIKAREE_Discharge.csv')
