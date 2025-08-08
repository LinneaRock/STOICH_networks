# Call in raw data from EDI

# Package ID: knb-lter-nwt.104.14 Cataloging System:https://pasta.edirepository.org.
# Data set title: Stream water chemistry data for Arikaree cirque, 1984 - ongoing..
# Data set creator:  T. Nelson (Nel) Caine -  
# Data set creator:  Jennifer F. Morse -  
# Data set creator:  Samuel Yevak -  
# Data set creator:  Henry Brandes -  
# Data set creator:    - Niwot Ridge LTER 
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/104/14/1f6d60d3f8fb0b902358b3b6da579f67" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "local_site",     
                 "year",     
                 "date",     
                 "time",     
                 "pH",     
                 "cond",     
                 "ANC",     
                 "acid",     
                 "alkal",     
                 "H.plus.",     
                 "NH3",     
                 "NH4.plus.",     
                 "Ca.plus..plus.",     
                 "Mg.plus..plus.",     
                 "Na.plus.",     
                 "K.plus.",     
                 "Cl.hyphen.",     
                 "NO3.hyphen.",     
                 "SO4.hyphen..hyphen.",     
                 "PO4.hyphen..hyphen..hyphen.",     
                 "Si",     
                 "cat_sum",     
                 "an_sum",     
                 "chg_bal",     
                 "TN",     
                 "TDN",     
                 "PN",     
                 "DON",     
                 "IN",     
                 "TP",     
                 "TDP",     
                 "PP",     
                 "DOP",     
                 "IP",     
                 "d18O",     
                 "d18O_sdev",     
                 "dDeut",     
                 "dD_sdev",     
                 "D_excess",     
                 "Trit",     
                 "T_sdev",     
                 "TOC",     
                 "DOC",     
                 "POC",     
                 "comments"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$LTER_site)!="factor") dt1$LTER_site<- as.factor(dt1$LTER_site)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$cond)=="factor") dt1$cond <-as.numeric(levels(dt1$cond))[as.integer(dt1$cond) ]               
if (class(dt1$cond)=="character") dt1$cond <-as.numeric(dt1$cond)
if (class(dt1$ANC)=="factor") dt1$ANC <-as.numeric(levels(dt1$ANC))[as.integer(dt1$ANC) ]               
if (class(dt1$ANC)=="character") dt1$ANC <-as.numeric(dt1$ANC)
if (class(dt1$acid)=="factor") dt1$acid <-as.numeric(levels(dt1$acid))[as.integer(dt1$acid) ]               
if (class(dt1$acid)=="character") dt1$acid <-as.numeric(dt1$acid)
if (class(dt1$alkal)=="factor") dt1$alkal <-as.numeric(levels(dt1$alkal))[as.integer(dt1$alkal) ]               
if (class(dt1$alkal)=="character") dt1$alkal <-as.numeric(dt1$alkal)
if (class(dt1$H.plus.)=="factor") dt1$H.plus. <-as.numeric(levels(dt1$H.plus.))[as.integer(dt1$H.plus.) ]               
if (class(dt1$H.plus.)=="character") dt1$H.plus. <-as.numeric(dt1$H.plus.)
if (class(dt1$NH3)=="factor") dt1$NH3 <-as.numeric(levels(dt1$NH3))[as.integer(dt1$NH3) ]               
if (class(dt1$NH3)=="character") dt1$NH3 <-as.numeric(dt1$NH3)
if (class(dt1$NH4.plus.)=="factor") dt1$NH4.plus. <-as.numeric(levels(dt1$NH4.plus.))[as.integer(dt1$NH4.plus.) ]               
if (class(dt1$NH4.plus.)=="character") dt1$NH4.plus. <-as.numeric(dt1$NH4.plus.)
if (class(dt1$Ca.plus..plus.)=="factor") dt1$Ca.plus..plus. <-as.numeric(levels(dt1$Ca.plus..plus.))[as.integer(dt1$Ca.plus..plus.) ]               
if (class(dt1$Ca.plus..plus.)=="character") dt1$Ca.plus..plus. <-as.numeric(dt1$Ca.plus..plus.)
if (class(dt1$Mg.plus..plus.)=="factor") dt1$Mg.plus..plus. <-as.numeric(levels(dt1$Mg.plus..plus.))[as.integer(dt1$Mg.plus..plus.) ]               
if (class(dt1$Mg.plus..plus.)=="character") dt1$Mg.plus..plus. <-as.numeric(dt1$Mg.plus..plus.)
if (class(dt1$Na.plus.)=="factor") dt1$Na.plus. <-as.numeric(levels(dt1$Na.plus.))[as.integer(dt1$Na.plus.) ]               
if (class(dt1$Na.plus.)=="character") dt1$Na.plus. <-as.numeric(dt1$Na.plus.)
if (class(dt1$K.plus.)=="factor") dt1$K.plus. <-as.numeric(levels(dt1$K.plus.))[as.integer(dt1$K.plus.) ]               
if (class(dt1$K.plus.)=="character") dt1$K.plus. <-as.numeric(dt1$K.plus.)
if (class(dt1$Cl.hyphen.)=="factor") dt1$Cl.hyphen. <-as.numeric(levels(dt1$Cl.hyphen.))[as.integer(dt1$Cl.hyphen.) ]               
if (class(dt1$Cl.hyphen.)=="character") dt1$Cl.hyphen. <-as.numeric(dt1$Cl.hyphen.)
if (class(dt1$NO3.hyphen.)=="factor") dt1$NO3.hyphen. <-as.numeric(levels(dt1$NO3.hyphen.))[as.integer(dt1$NO3.hyphen.) ]               
if (class(dt1$NO3.hyphen.)=="character") dt1$NO3.hyphen. <-as.numeric(dt1$NO3.hyphen.)
if (class(dt1$SO4.hyphen..hyphen.)=="factor") dt1$SO4.hyphen..hyphen. <-as.numeric(levels(dt1$SO4.hyphen..hyphen.))[as.integer(dt1$SO4.hyphen..hyphen.) ]               
if (class(dt1$SO4.hyphen..hyphen.)=="character") dt1$SO4.hyphen..hyphen. <-as.numeric(dt1$SO4.hyphen..hyphen.)
if (class(dt1$PO4.hyphen..hyphen..hyphen.)=="factor") dt1$PO4.hyphen..hyphen..hyphen. <-as.numeric(levels(dt1$PO4.hyphen..hyphen..hyphen.))[as.integer(dt1$PO4.hyphen..hyphen..hyphen.) ]               
if (class(dt1$PO4.hyphen..hyphen..hyphen.)=="character") dt1$PO4.hyphen..hyphen..hyphen. <-as.numeric(dt1$PO4.hyphen..hyphen..hyphen.)
if (class(dt1$Si)=="factor") dt1$Si <-as.numeric(levels(dt1$Si))[as.integer(dt1$Si) ]               
if (class(dt1$Si)=="character") dt1$Si <-as.numeric(dt1$Si)
if (class(dt1$cat_sum)=="factor") dt1$cat_sum <-as.numeric(levels(dt1$cat_sum))[as.integer(dt1$cat_sum) ]               
if (class(dt1$cat_sum)=="character") dt1$cat_sum <-as.numeric(dt1$cat_sum)
if (class(dt1$an_sum)=="factor") dt1$an_sum <-as.numeric(levels(dt1$an_sum))[as.integer(dt1$an_sum) ]               
if (class(dt1$an_sum)=="character") dt1$an_sum <-as.numeric(dt1$an_sum)
if (class(dt1$chg_bal)=="factor") dt1$chg_bal <-as.numeric(levels(dt1$chg_bal))[as.integer(dt1$chg_bal) ]               
if (class(dt1$chg_bal)=="character") dt1$chg_bal <-as.numeric(dt1$chg_bal)
if (class(dt1$TN)=="factor") dt1$TN <-as.numeric(levels(dt1$TN))[as.integer(dt1$TN) ]               
if (class(dt1$TN)=="character") dt1$TN <-as.numeric(dt1$TN)
if (class(dt1$TDN)=="factor") dt1$TDN <-as.numeric(levels(dt1$TDN))[as.integer(dt1$TDN) ]               
if (class(dt1$TDN)=="character") dt1$TDN <-as.numeric(dt1$TDN)
if (class(dt1$PN)=="factor") dt1$PN <-as.numeric(levels(dt1$PN))[as.integer(dt1$PN) ]               
if (class(dt1$PN)=="character") dt1$PN <-as.numeric(dt1$PN)
if (class(dt1$DON)=="factor") dt1$DON <-as.numeric(levels(dt1$DON))[as.integer(dt1$DON) ]               
if (class(dt1$DON)=="character") dt1$DON <-as.numeric(dt1$DON)
if (class(dt1$IN)=="factor") dt1$IN <-as.numeric(levels(dt1$IN))[as.integer(dt1$IN) ]               
if (class(dt1$IN)=="character") dt1$IN <-as.numeric(dt1$IN)
if (class(dt1$TP)=="factor") dt1$TP <-as.numeric(levels(dt1$TP))[as.integer(dt1$TP) ]               
if (class(dt1$TP)=="character") dt1$TP <-as.numeric(dt1$TP)
if (class(dt1$TDP)=="factor") dt1$TDP <-as.numeric(levels(dt1$TDP))[as.integer(dt1$TDP) ]               
if (class(dt1$TDP)=="character") dt1$TDP <-as.numeric(dt1$TDP)
if (class(dt1$PP)=="factor") dt1$PP <-as.numeric(levels(dt1$PP))[as.integer(dt1$PP) ]               
if (class(dt1$PP)=="character") dt1$PP <-as.numeric(dt1$PP)
if (class(dt1$DOP)=="factor") dt1$DOP <-as.numeric(levels(dt1$DOP))[as.integer(dt1$DOP) ]               
if (class(dt1$DOP)=="character") dt1$DOP <-as.numeric(dt1$DOP)
if (class(dt1$IP)=="factor") dt1$IP <-as.numeric(levels(dt1$IP))[as.integer(dt1$IP) ]               
if (class(dt1$IP)=="character") dt1$IP <-as.numeric(dt1$IP)
if (class(dt1$d18O)=="factor") dt1$d18O <-as.numeric(levels(dt1$d18O))[as.integer(dt1$d18O) ]               
if (class(dt1$d18O)=="character") dt1$d18O <-as.numeric(dt1$d18O)
if (class(dt1$d18O_sdev)=="factor") dt1$d18O_sdev <-as.numeric(levels(dt1$d18O_sdev))[as.integer(dt1$d18O_sdev) ]               
if (class(dt1$d18O_sdev)=="character") dt1$d18O_sdev <-as.numeric(dt1$d18O_sdev)
if (class(dt1$dDeut)=="factor") dt1$dDeut <-as.numeric(levels(dt1$dDeut))[as.integer(dt1$dDeut) ]               
if (class(dt1$dDeut)=="character") dt1$dDeut <-as.numeric(dt1$dDeut)
if (class(dt1$dD_sdev)=="factor") dt1$dD_sdev <-as.numeric(levels(dt1$dD_sdev))[as.integer(dt1$dD_sdev) ]               
if (class(dt1$dD_sdev)=="character") dt1$dD_sdev <-as.numeric(dt1$dD_sdev)
if (class(dt1$D_excess)=="factor") dt1$D_excess <-as.numeric(levels(dt1$D_excess))[as.integer(dt1$D_excess) ]               
if (class(dt1$D_excess)=="character") dt1$D_excess <-as.numeric(dt1$D_excess)
if (class(dt1$Trit)=="factor") dt1$Trit <-as.numeric(levels(dt1$Trit))[as.integer(dt1$Trit) ]               
if (class(dt1$Trit)=="character") dt1$Trit <-as.numeric(dt1$Trit)
if (class(dt1$T_sdev)=="factor") dt1$T_sdev <-as.numeric(levels(dt1$T_sdev))[as.integer(dt1$T_sdev) ]               
if (class(dt1$T_sdev)=="character") dt1$T_sdev <-as.numeric(dt1$T_sdev)
if (class(dt1$TOC)=="factor") dt1$TOC <-as.numeric(levels(dt1$TOC))[as.integer(dt1$TOC) ]               
if (class(dt1$TOC)=="character") dt1$TOC <-as.numeric(dt1$TOC)
if (class(dt1$DOC)=="factor") dt1$DOC <-as.numeric(levels(dt1$DOC))[as.integer(dt1$DOC) ]               
if (class(dt1$DOC)=="character") dt1$DOC <-as.numeric(dt1$DOC)
if (class(dt1$POC)=="factor") dt1$POC <-as.numeric(levels(dt1$POC))[as.integer(dt1$POC) ]               
if (class(dt1$POC)=="character") dt1$POC <-as.numeric(dt1$POC)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)

# Convert Missing Values to NA for non-dates

dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NP")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NP"))),NA,dt1$pH))
dt1$cond <- ifelse((trimws(as.character(dt1$cond))==trimws("NP")),NA,dt1$cond)               
suppressWarnings(dt1$cond <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$cond))==as.character(as.numeric("NP"))),NA,dt1$cond))
dt1$ANC <- ifelse((trimws(as.character(dt1$ANC))==trimws("NP")),NA,dt1$ANC)               
suppressWarnings(dt1$ANC <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$ANC))==as.character(as.numeric("NP"))),NA,dt1$ANC))
dt1$acid <- ifelse((trimws(as.character(dt1$acid))==trimws("NP")),NA,dt1$acid)               
suppressWarnings(dt1$acid <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$acid))==as.character(as.numeric("NP"))),NA,dt1$acid))
dt1$alkal <- ifelse((trimws(as.character(dt1$alkal))==trimws("NP")),NA,dt1$alkal)               
suppressWarnings(dt1$alkal <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$alkal))==as.character(as.numeric("NP"))),NA,dt1$alkal))
dt1$H.plus. <- ifelse((trimws(as.character(dt1$H.plus.))==trimws("NP")),NA,dt1$H.plus.)               
suppressWarnings(dt1$H.plus. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$H.plus.))==as.character(as.numeric("NP"))),NA,dt1$H.plus.))
dt1$NH3 <- ifelse((trimws(as.character(dt1$NH3))==trimws("NP")),NA,dt1$NH3)               
suppressWarnings(dt1$NH3 <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$NH3))==as.character(as.numeric("NP"))),NA,dt1$NH3))
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("NP")),NA,dt1$NH4.plus.)               
suppressWarnings(dt1$NH4.plus. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$NH4.plus.))==as.character(as.numeric("NP"))),NA,dt1$NH4.plus.))
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.143")),NA,dt1$NH4.plus.)               
suppressWarnings(dt1$NH4.plus. <- ifelse(!is.na(as.numeric("<0.143")) & (trimws(as.character(dt1$NH4.plus.))==as.character(as.numeric("<0.143"))),NA,dt1$NH4.plus.))
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.71")),NA,dt1$NH4.plus.)               
suppressWarnings(dt1$NH4.plus. <- ifelse(!is.na(as.numeric("<0.71")) & (trimws(as.character(dt1$NH4.plus.))==as.character(as.numeric("<0.71"))),NA,dt1$NH4.plus.))
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.38")),NA,dt1$NH4.plus.)               
suppressWarnings(dt1$NH4.plus. <- ifelse(!is.na(as.numeric("<0.38")) & (trimws(as.character(dt1$NH4.plus.))==as.character(as.numeric("<0.38"))),NA,dt1$NH4.plus.))
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.22")),NA,dt1$NH4.plus.)               
suppressWarnings(dt1$NH4.plus. <- ifelse(!is.na(as.numeric("<0.22")) & (trimws(as.character(dt1$NH4.plus.))==as.character(as.numeric("<0.22"))),NA,dt1$NH4.plus.))
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("u")),NA,dt1$NH4.plus.)               
suppressWarnings(dt1$NH4.plus. <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$NH4.plus.))==as.character(as.numeric("u"))),NA,dt1$NH4.plus.))
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.1428")),NA,dt1$NH4.plus.)               
suppressWarnings(dt1$NH4.plus. <- ifelse(!is.na(as.numeric("<0.1428")) & (trimws(as.character(dt1$NH4.plus.))==as.character(as.numeric("<0.1428"))),NA,dt1$NH4.plus.))
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.4")),NA,dt1$NH4.plus.)               
suppressWarnings(dt1$NH4.plus. <- ifelse(!is.na(as.numeric("<0.4")) & (trimws(as.character(dt1$NH4.plus.))==as.character(as.numeric("<0.4"))),NA,dt1$NH4.plus.))
dt1$Ca.plus..plus. <- ifelse((trimws(as.character(dt1$Ca.plus..plus.))==trimws("NP")),NA,dt1$Ca.plus..plus.)               
suppressWarnings(dt1$Ca.plus..plus. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$Ca.plus..plus.))==as.character(as.numeric("NP"))),NA,dt1$Ca.plus..plus.))
dt1$Ca.plus..plus. <- ifelse((trimws(as.character(dt1$Ca.plus..plus.))==trimws("<0.449")),NA,dt1$Ca.plus..plus.)               
suppressWarnings(dt1$Ca.plus..plus. <- ifelse(!is.na(as.numeric("<0.449")) & (trimws(as.character(dt1$Ca.plus..plus.))==as.character(as.numeric("<0.449"))),NA,dt1$Ca.plus..plus.))
dt1$Mg.plus..plus. <- ifelse((trimws(as.character(dt1$Mg.plus..plus.))==trimws("NP")),NA,dt1$Mg.plus..plus.)               
suppressWarnings(dt1$Mg.plus..plus. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$Mg.plus..plus.))==as.character(as.numeric("NP"))),NA,dt1$Mg.plus..plus.))
dt1$Mg.plus..plus. <- ifelse((trimws(as.character(dt1$Mg.plus..plus.))==trimws("<0.082")),NA,dt1$Mg.plus..plus.)               
suppressWarnings(dt1$Mg.plus..plus. <- ifelse(!is.na(as.numeric("<0.082")) & (trimws(as.character(dt1$Mg.plus..plus.))==as.character(as.numeric("<0.082"))),NA,dt1$Mg.plus..plus.))
dt1$Na.plus. <- ifelse((trimws(as.character(dt1$Na.plus.))==trimws("NP")),NA,dt1$Na.plus.)               
suppressWarnings(dt1$Na.plus. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$Na.plus.))==as.character(as.numeric("NP"))),NA,dt1$Na.plus.))
dt1$Na.plus. <- ifelse((trimws(as.character(dt1$Na.plus.))==trimws("<0.087")),NA,dt1$Na.plus.)               
suppressWarnings(dt1$Na.plus. <- ifelse(!is.na(as.numeric("<0.087")) & (trimws(as.character(dt1$Na.plus.))==as.character(as.numeric("<0.087"))),NA,dt1$Na.plus.))
dt1$K.plus. <- ifelse((trimws(as.character(dt1$K.plus.))==trimws("NP")),NA,dt1$K.plus.)               
suppressWarnings(dt1$K.plus. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$K.plus.))==as.character(as.numeric("NP"))),NA,dt1$K.plus.))
dt1$K.plus. <- ifelse((trimws(as.character(dt1$K.plus.))==trimws("<0.102")),NA,dt1$K.plus.)               
suppressWarnings(dt1$K.plus. <- ifelse(!is.na(as.numeric("<0.102")) & (trimws(as.character(dt1$K.plus.))==as.character(as.numeric("<0.102"))),NA,dt1$K.plus.))
dt1$Cl.hyphen. <- ifelse((trimws(as.character(dt1$Cl.hyphen.))==trimws("NP")),NA,dt1$Cl.hyphen.)               
suppressWarnings(dt1$Cl.hyphen. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$Cl.hyphen.))==as.character(as.numeric("NP"))),NA,dt1$Cl.hyphen.))
dt1$Cl.hyphen. <- ifelse((trimws(as.character(dt1$Cl.hyphen.))==trimws("<0.846")),NA,dt1$Cl.hyphen.)               
suppressWarnings(dt1$Cl.hyphen. <- ifelse(!is.na(as.numeric("<0.846")) & (trimws(as.character(dt1$Cl.hyphen.))==as.character(as.numeric("<0.846"))),NA,dt1$Cl.hyphen.))
dt1$Cl.hyphen. <- ifelse((trimws(as.character(dt1$Cl.hyphen.))==trimws("<0.28")),NA,dt1$Cl.hyphen.)               
suppressWarnings(dt1$Cl.hyphen. <- ifelse(!is.na(as.numeric("<0.28")) & (trimws(as.character(dt1$Cl.hyphen.))==as.character(as.numeric("<0.28"))),NA,dt1$Cl.hyphen.))
dt1$NO3.hyphen. <- ifelse((trimws(as.character(dt1$NO3.hyphen.))==trimws("NP")),NA,dt1$NO3.hyphen.)               
suppressWarnings(dt1$NO3.hyphen. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$NO3.hyphen.))==as.character(as.numeric("NP"))),NA,dt1$NO3.hyphen.))
dt1$NO3.hyphen. <- ifelse((trimws(as.character(dt1$NO3.hyphen.))==trimws("<0.323")),NA,dt1$NO3.hyphen.)               
suppressWarnings(dt1$NO3.hyphen. <- ifelse(!is.na(as.numeric("<0.323")) & (trimws(as.character(dt1$NO3.hyphen.))==as.character(as.numeric("<0.323"))),NA,dt1$NO3.hyphen.))
dt1$NO3.hyphen. <- ifelse((trimws(as.character(dt1$NO3.hyphen.))==trimws("<0.16")),NA,dt1$NO3.hyphen.)               
suppressWarnings(dt1$NO3.hyphen. <- ifelse(!is.na(as.numeric("<0.16")) & (trimws(as.character(dt1$NO3.hyphen.))==as.character(as.numeric("<0.16"))),NA,dt1$NO3.hyphen.))
dt1$NO3.hyphen. <- ifelse((trimws(as.character(dt1$NO3.hyphen.))==trimws("u")),NA,dt1$NO3.hyphen.)               
suppressWarnings(dt1$NO3.hyphen. <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$NO3.hyphen.))==as.character(as.numeric("u"))),NA,dt1$NO3.hyphen.))
dt1$SO4.hyphen..hyphen. <- ifelse((trimws(as.character(dt1$SO4.hyphen..hyphen.))==trimws("NP")),NA,dt1$SO4.hyphen..hyphen.)               
suppressWarnings(dt1$SO4.hyphen..hyphen. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$SO4.hyphen..hyphen.))==as.character(as.numeric("NP"))),NA,dt1$SO4.hyphen..hyphen.))
dt1$SO4.hyphen..hyphen. <- ifelse((trimws(as.character(dt1$SO4.hyphen..hyphen.))==trimws("<0.833")),NA,dt1$SO4.hyphen..hyphen.)               
suppressWarnings(dt1$SO4.hyphen..hyphen. <- ifelse(!is.na(as.numeric("<0.833")) & (trimws(as.character(dt1$SO4.hyphen..hyphen.))==as.character(as.numeric("<0.833"))),NA,dt1$SO4.hyphen..hyphen.))
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("NP")),NA,dt1$PO4.hyphen..hyphen..hyphen.)               
suppressWarnings(dt1$PO4.hyphen..hyphen..hyphen. <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==as.character(as.numeric("NP"))),NA,dt1$PO4.hyphen..hyphen..hyphen.))
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("<0.063")),NA,dt1$PO4.hyphen..hyphen..hyphen.)               
suppressWarnings(dt1$PO4.hyphen..hyphen..hyphen. <- ifelse(!is.na(as.numeric("<0.063")) & (trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==as.character(as.numeric("<0.063"))),NA,dt1$PO4.hyphen..hyphen..hyphen.))
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("<0.03")),NA,dt1$PO4.hyphen..hyphen..hyphen.)               
suppressWarnings(dt1$PO4.hyphen..hyphen..hyphen. <- ifelse(!is.na(as.numeric("<0.03")) & (trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==as.character(as.numeric("<0.03"))),NA,dt1$PO4.hyphen..hyphen..hyphen.))
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("<0.08")),NA,dt1$PO4.hyphen..hyphen..hyphen.)               
suppressWarnings(dt1$PO4.hyphen..hyphen..hyphen. <- ifelse(!is.na(as.numeric("<0.08")) & (trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==as.character(as.numeric("<0.08"))),NA,dt1$PO4.hyphen..hyphen..hyphen.))
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("u")),NA,dt1$PO4.hyphen..hyphen..hyphen.)               
suppressWarnings(dt1$PO4.hyphen..hyphen..hyphen. <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==as.character(as.numeric("u"))),NA,dt1$PO4.hyphen..hyphen..hyphen.))
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("<0.0387")),NA,dt1$PO4.hyphen..hyphen..hyphen.)               
suppressWarnings(dt1$PO4.hyphen..hyphen..hyphen. <- ifelse(!is.na(as.numeric("<0.0387")) & (trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==as.character(as.numeric("<0.0387"))),NA,dt1$PO4.hyphen..hyphen..hyphen.))
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("<0.0388")),NA,dt1$PO4.hyphen..hyphen..hyphen.)               
suppressWarnings(dt1$PO4.hyphen..hyphen..hyphen. <- ifelse(!is.na(as.numeric("<0.0388")) & (trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==as.character(as.numeric("<0.0388"))),NA,dt1$PO4.hyphen..hyphen..hyphen.))
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("<0.316")),NA,dt1$PO4.hyphen..hyphen..hyphen.)               
suppressWarnings(dt1$PO4.hyphen..hyphen..hyphen. <- ifelse(!is.na(as.numeric("<0.316")) & (trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==as.character(as.numeric("<0.316"))),NA,dt1$PO4.hyphen..hyphen..hyphen.))
dt1$Si <- ifelse((trimws(as.character(dt1$Si))==trimws("NP")),NA,dt1$Si)               
suppressWarnings(dt1$Si <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$Si))==as.character(as.numeric("NP"))),NA,dt1$Si))
dt1$Si <- ifelse((trimws(as.character(dt1$Si))==trimws("<0.0214")),NA,dt1$Si)               
suppressWarnings(dt1$Si <- ifelse(!is.na(as.numeric("<0.0214")) & (trimws(as.character(dt1$Si))==as.character(as.numeric("<0.0214"))),NA,dt1$Si))
dt1$cat_sum <- ifelse((trimws(as.character(dt1$cat_sum))==trimws("NP")),NA,dt1$cat_sum)               
suppressWarnings(dt1$cat_sum <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$cat_sum))==as.character(as.numeric("NP"))),NA,dt1$cat_sum))
dt1$an_sum <- ifelse((trimws(as.character(dt1$an_sum))==trimws("NP")),NA,dt1$an_sum)               
suppressWarnings(dt1$an_sum <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$an_sum))==as.character(as.numeric("NP"))),NA,dt1$an_sum))
dt1$chg_bal <- ifelse((trimws(as.character(dt1$chg_bal))==trimws("NP")),NA,dt1$chg_bal)               
suppressWarnings(dt1$chg_bal <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$chg_bal))==as.character(as.numeric("NP"))),NA,dt1$chg_bal))
dt1$TN <- ifelse((trimws(as.character(dt1$TN))==trimws("NP")),NA,dt1$TN)               
suppressWarnings(dt1$TN <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$TN))==as.character(as.numeric("NP"))),NA,dt1$TN))
dt1$TDN <- ifelse((trimws(as.character(dt1$TDN))==trimws("NP")),NA,dt1$TDN)               
suppressWarnings(dt1$TDN <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$TDN))==as.character(as.numeric("NP"))),NA,dt1$TDN))
dt1$TDN <- ifelse((trimws(as.character(dt1$TDN))==trimws("<0.571")),NA,dt1$TDN)               
suppressWarnings(dt1$TDN <- ifelse(!is.na(as.numeric("<0.571")) & (trimws(as.character(dt1$TDN))==as.character(as.numeric("<0.571"))),NA,dt1$TDN))
dt1$TDN <- ifelse((trimws(as.character(dt1$TDN))==trimws("EQCL")),NA,dt1$TDN)               
suppressWarnings(dt1$TDN <- ifelse(!is.na(as.numeric("EQCL")) & (trimws(as.character(dt1$TDN))==as.character(as.numeric("EQCL"))),NA,dt1$TDN))
dt1$PN <- ifelse((trimws(as.character(dt1$PN))==trimws("NP")),NA,dt1$PN)               
suppressWarnings(dt1$PN <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$PN))==as.character(as.numeric("NP"))),NA,dt1$PN))
dt1$PN <- ifelse((trimws(as.character(dt1$PN))==trimws("u")),NA,dt1$PN)               
suppressWarnings(dt1$PN <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$PN))==as.character(as.numeric("u"))),NA,dt1$PN))
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("NP")),NA,dt1$DON)               
suppressWarnings(dt1$DON <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$DON))==as.character(as.numeric("NP"))),NA,dt1$DON))
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("u")),NA,dt1$DON)               
suppressWarnings(dt1$DON <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$DON))==as.character(as.numeric("u"))),NA,dt1$DON))
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("EQCL")),NA,dt1$DON)               
suppressWarnings(dt1$DON <- ifelse(!is.na(as.numeric("EQCL")) & (trimws(as.character(dt1$DON))==as.character(as.numeric("EQCL"))),NA,dt1$DON))
dt1$IN <- ifelse((trimws(as.character(dt1$IN))==trimws("NP")),NA,dt1$IN)               
suppressWarnings(dt1$IN <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$IN))==as.character(as.numeric("NP"))),NA,dt1$IN))
dt1$IN <- ifelse((trimws(as.character(dt1$IN))==trimws("u")),NA,dt1$IN)               
suppressWarnings(dt1$IN <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$IN))==as.character(as.numeric("u"))),NA,dt1$IN))
dt1$TP <- ifelse((trimws(as.character(dt1$TP))==trimws("NP")),NA,dt1$TP)               
suppressWarnings(dt1$TP <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$TP))==as.character(as.numeric("NP"))),NA,dt1$TP))
dt1$TP <- ifelse((trimws(as.character(dt1$TP))==trimws("u")),NA,dt1$TP)               
suppressWarnings(dt1$TP <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$TP))==as.character(as.numeric("u"))),NA,dt1$TP))
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("NP")),NA,dt1$TDP)               
suppressWarnings(dt1$TDP <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$TDP))==as.character(as.numeric("NP"))),NA,dt1$TDP))
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("<0.0258")),NA,dt1$TDP)               
suppressWarnings(dt1$TDP <- ifelse(!is.na(as.numeric("<0.0258")) & (trimws(as.character(dt1$TDP))==as.character(as.numeric("<0.0258"))),NA,dt1$TDP))
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("<0.04")),NA,dt1$TDP)               
suppressWarnings(dt1$TDP <- ifelse(!is.na(as.numeric("<0.04")) & (trimws(as.character(dt1$TDP))==as.character(as.numeric("<0.04"))),NA,dt1$TDP))
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("<0.01")),NA,dt1$TDP)               
suppressWarnings(dt1$TDP <- ifelse(!is.na(as.numeric("<0.01")) & (trimws(as.character(dt1$TDP))==as.character(as.numeric("<0.01"))),NA,dt1$TDP))
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("EQCL")),NA,dt1$TDP)               
suppressWarnings(dt1$TDP <- ifelse(!is.na(as.numeric("EQCL")) & (trimws(as.character(dt1$TDP))==as.character(as.numeric("EQCL"))),NA,dt1$TDP))
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("u")),NA,dt1$TDP)               
suppressWarnings(dt1$TDP <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$TDP))==as.character(as.numeric("u"))),NA,dt1$TDP))
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("<0.0258")),NA,dt1$TDP)               
suppressWarnings(dt1$TDP <- ifelse(!is.na(as.numeric("<0.0258")) & (trimws(as.character(dt1$TDP))==as.character(as.numeric("<0.0258"))),NA,dt1$TDP))
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("<0.0650")),NA,dt1$TDP)               
suppressWarnings(dt1$TDP <- ifelse(!is.na(as.numeric("<0.0650")) & (trimws(as.character(dt1$TDP))==as.character(as.numeric("<0.0650"))),NA,dt1$TDP))
dt1$PP <- ifelse((trimws(as.character(dt1$PP))==trimws("NP")),NA,dt1$PP)               
suppressWarnings(dt1$PP <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$PP))==as.character(as.numeric("NP"))),NA,dt1$PP))
dt1$PP <- ifelse((trimws(as.character(dt1$PP))==trimws("u")),NA,dt1$PP)               
suppressWarnings(dt1$PP <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$PP))==as.character(as.numeric("u"))),NA,dt1$PP))
dt1$DOP <- ifelse((trimws(as.character(dt1$DOP))==trimws("NP")),NA,dt1$DOP)               
suppressWarnings(dt1$DOP <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$DOP))==as.character(as.numeric("NP"))),NA,dt1$DOP))
dt1$DOP <- ifelse((trimws(as.character(dt1$DOP))==trimws("u")),NA,dt1$DOP)               
suppressWarnings(dt1$DOP <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$DOP))==as.character(as.numeric("u"))),NA,dt1$DOP))
dt1$DOP <- ifelse((trimws(as.character(dt1$DOP))==trimws("EQCL")),NA,dt1$DOP)               
suppressWarnings(dt1$DOP <- ifelse(!is.na(as.numeric("EQCL")) & (trimws(as.character(dt1$DOP))==as.character(as.numeric("EQCL"))),NA,dt1$DOP))
dt1$IP <- ifelse((trimws(as.character(dt1$IP))==trimws("NP")),NA,dt1$IP)               
suppressWarnings(dt1$IP <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$IP))==as.character(as.numeric("NP"))),NA,dt1$IP))
dt1$IP <- ifelse((trimws(as.character(dt1$IP))==trimws("u")),NA,dt1$IP)               
suppressWarnings(dt1$IP <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt1$IP))==as.character(as.numeric("u"))),NA,dt1$IP))
dt1$IP <- ifelse((trimws(as.character(dt1$IP))==trimws("<0.03")),NA,dt1$IP)               
suppressWarnings(dt1$IP <- ifelse(!is.na(as.numeric("<0.03")) & (trimws(as.character(dt1$IP))==as.character(as.numeric("<0.03"))),NA,dt1$IP))
dt1$IP <- ifelse((trimws(as.character(dt1$IP))==trimws("<0.01")),NA,dt1$IP)               
suppressWarnings(dt1$IP <- ifelse(!is.na(as.numeric("<0.01")) & (trimws(as.character(dt1$IP))==as.character(as.numeric("<0.01"))),NA,dt1$IP))
dt1$d18O <- ifelse((trimws(as.character(dt1$d18O))==trimws("NP")),NA,dt1$d18O)               
suppressWarnings(dt1$d18O <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$d18O))==as.character(as.numeric("NP"))),NA,dt1$d18O))
dt1$d18O_sdev <- ifelse((trimws(as.character(dt1$d18O_sdev))==trimws("NP")),NA,dt1$d18O_sdev)               
suppressWarnings(dt1$d18O_sdev <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$d18O_sdev))==as.character(as.numeric("NP"))),NA,dt1$d18O_sdev))
dt1$dDeut <- ifelse((trimws(as.character(dt1$dDeut))==trimws("NP")),NA,dt1$dDeut)               
suppressWarnings(dt1$dDeut <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$dDeut))==as.character(as.numeric("NP"))),NA,dt1$dDeut))
dt1$dD_sdev <- ifelse((trimws(as.character(dt1$dD_sdev))==trimws("NP")),NA,dt1$dD_sdev)               
suppressWarnings(dt1$dD_sdev <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$dD_sdev))==as.character(as.numeric("NP"))),NA,dt1$dD_sdev))
dt1$D_excess <- ifelse((trimws(as.character(dt1$D_excess))==trimws("NP")),NA,dt1$D_excess)               
suppressWarnings(dt1$D_excess <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$D_excess))==as.character(as.numeric("NP"))),NA,dt1$D_excess))
dt1$Trit <- ifelse((trimws(as.character(dt1$Trit))==trimws("NP")),NA,dt1$Trit)               
suppressWarnings(dt1$Trit <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$Trit))==as.character(as.numeric("NP"))),NA,dt1$Trit))
dt1$T_sdev <- ifelse((trimws(as.character(dt1$T_sdev))==trimws("NP")),NA,dt1$T_sdev)               
suppressWarnings(dt1$T_sdev <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$T_sdev))==as.character(as.numeric("NP"))),NA,dt1$T_sdev))
dt1$TOC <- ifelse((trimws(as.character(dt1$TOC))==trimws("NP")),NA,dt1$TOC)               
suppressWarnings(dt1$TOC <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$TOC))==as.character(as.numeric("NP"))),NA,dt1$TOC))
dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("NP")),NA,dt1$DOC)               
suppressWarnings(dt1$DOC <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$DOC))==as.character(as.numeric("NP"))),NA,dt1$DOC))
dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("QNS")),NA,dt1$DOC)               
suppressWarnings(dt1$DOC <- ifelse(!is.na(as.numeric("QNS")) & (trimws(as.character(dt1$DOC))==as.character(as.numeric("QNS"))),NA,dt1$DOC))
dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("<0.04")),NA,dt1$DOC)               
suppressWarnings(dt1$DOC <- ifelse(!is.na(as.numeric("<0.04")) & (trimws(as.character(dt1$DOC))==as.character(as.numeric("<0.04"))),NA,dt1$DOC))
dt1$POC <- ifelse((trimws(as.character(dt1$POC))==trimws("NP")),NA,dt1$POC)               
suppressWarnings(dt1$POC <- ifelse(!is.na(as.numeric("NP")) & (trimws(as.character(dt1$POC))==as.character(as.numeric("NP"))),NA,dt1$POC))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(local_site)
summary(year)
summary(date)
summary(time)
summary(pH)
summary(cond)
summary(ANC)
summary(acid)
summary(alkal)
summary(H.plus.)
summary(NH3)
summary(NH4.plus.)
summary(Ca.plus..plus.)
summary(Mg.plus..plus.)
summary(Na.plus.)
summary(K.plus.)
summary(Cl.hyphen.)
summary(NO3.hyphen.)
summary(SO4.hyphen..hyphen.)
summary(PO4.hyphen..hyphen..hyphen.)
summary(Si)
summary(cat_sum)
summary(an_sum)
summary(chg_bal)
summary(TN)
summary(TDN)
summary(PN)
summary(DON)
summary(IN)
summary(TP)
summary(TDP)
summary(PP)
summary(DOP)
summary(IP)
summary(d18O)
summary(d18O_sdev)
summary(dDeut)
summary(dD_sdev)
summary(D_excess)
summary(Trit)
summary(T_sdev)
summary(TOC)
summary(DOC)
summary(POC)
summary(comments) 
# Get more details on character variables

summary(as.factor(dt1$LTER_site)) 
summary(as.factor(dt1$local_site)) 
summary(as.factor(dt1$comments))
detach(dt1)               

# keep data we want ####
str(dt1)

ARIKAREE_WQ <- dt1 |>
  mutate(location = 'GLACIER') |>
  select(local_site, location, year, date, TN, TDN, PN, DON, IN, TP, TDP, PP, DOP, IP) |> # all units umol/L except IN, which is ueq/L
  filter(!if_all(TN:IP, is.na))

write.csv(ARIKAREE_WQ, 'Data/DataPUllEDI/knb-lter-nwt.104.14_ARIKAREE_WQ.csv')


