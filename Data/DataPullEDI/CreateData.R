#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Format and combine raw data pulled from EDI #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


library(tidyverse)

# nutrients ####
alb_inlet_wq <- read.csv('Data/DataPullEDI/knb-lter-nwt.110.7_ALB_INLET_WQ.csv') |>
  select(-X)
gl5_outlet_wq <- read.csv('Data/DataPullEDI/knb-lter-nwt.109.12_GL5_OUTLET_WQ.csv')|>
  select(-X)
gl4_outlet_wq <- read.csv('Data/DataPullEDI/knb-lter-nwt.108.14_GL4_WQ.csv')|>
  select(-X)
arikaree_wq <- read.csv('Data/DataPullEDI/knb-lter-nwt.104.14_ARIKAREE_WQ.csv')|>
  select(-X)
alb_camp_wq <- read.csv('Data/DataPullEDI/knb-lter-nwt.103.15_ALB_CAMP_WQ.csv')|>
  select(-X)
lakes_streams_wq <- read.csv('Data/DataPullEDI/knb-lter-nwt.10.4_LAKES_STREAMS_WQ.csv')|>
  select(-X, -depth)

gl_network_nuts <- rbind(alb_inlet_wq,gl5_outlet_wq,gl4_outlet_wq,arikaree_wq,alb_camp_wq,lakes_streams_wq) |>
  mutate(local_site=ifelse(local_site=='ALBION','ALB',local_site))
write.csv(gl_network_nuts, 'Data/gl_network_nuts_raw.csv')


# discharge ####
gl4_discharge <- read.csv('Data/DataPullEDI/knb-lter-nwt.105.19_GL4_Discharge.csv')|>
  select(-X)
arik_discharge <- read.csv('Data/DataPullEDI/knb-lter-nwt.169.1_ARIKAREE_Discharge.csv')|>
  select(-X)
alb_camp_discharge <- read.csv('Data/DataPullEDI/knb-lter-nwt.102.19_ALB_CAMP_Discharge.csv')|>
  select(-X)

gl_network_discharge <- rbind(gl4_discharge,arik_discharge,alb_camp_discharge)  |>
  mutate(local_site=ifelse(local_site=='alb','ALB',local_site))
write.csv(gl_network_discharge, 'Data/gl_network_discharge_raw.csv')
