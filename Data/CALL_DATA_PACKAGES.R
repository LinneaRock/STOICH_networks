# call in packages

library(tidyverse)
library(dataRetrieval) # for water year
library(lubridate)
library(scales)
library(ggpubr)
library(multcompView)
library(ggtern)
library(biogas)
library(colorblindr)
library(lme4)
library(ggpubr)
library(plotrix) 
library(GGally)
library(ggmap)
library(leaflet)
#library(nhdplusTools)
library(sf)
library(ggspatial)
library(ggridges)
library(viridis)
library(kdensity)
library(confintr)
library(randomForest)
library(ggcorrplot)
library(matrixStats)
library(trend)
library(broom)
library(mgcv)





# call in data

# ions <- read.csv('Data/ions_outliers_removed.csv') |>
#   mutate(date = as.Date(date)) |>
#   filter(site != 'FLUME') |>
#   mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', 
#                                                                 '7','8','9','10','11','12',
#                                                                 '12a','13','14','15','16'))) |>
#   mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
#   dplyr::select(-X)

nuts <- read.csv('Data/nuts_outliers_removed.csv')|>
  mutate(date = as.Date(date)) |>
  filter(site != 'FLUME',
         eco_type != 'glacier',
         site != 'ALB_CAMP') |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', 
                                                                '7','8','9','10','11','12',
                                                                '12a','13','14','15','16'))) |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  dplyr::select(-X)

stoich <- read.csv('Data/stoich_after_outliers_removed.csv')|>
  mutate(date = as.Date(date)) |>
  filter(site != 'FLUME',
         eco_type != 'glacier',
         site != 'ALB_CAMP') |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', 
                                                                '7','8','9','10','11','12',
                                                                '12a','13','14','15','16'))) |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  dplyr::select(-X)

discharge <- read.csv('Data/discharge_outliers_removed.csv')|>
  mutate(date = as.Date(date)) |>
  filter(site != 'FLUME') |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', 
                                                                '7','8','9','10','11','12',
                                                                '12a','13','14','15','16'))) |>
  dplyr::select(-X)

#### frequency plots outlier removed data ####
ggplot(ions) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free') +
  labs(title = 'outliers removed')


ggplot(nuts) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'outliers removed')

ggplot(discharge) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free') +
  labs(title = 'outliers removed')

ggplot(stoich) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'outliers removed') 


# read in spatial data
ALB_flow <- readRDS('Data/Spatial_Data/ALB_flowline.RDS')
ALB_waterbody <- readRDS('Data/Spatial_Data/ALB_waterbody.RDS')
ALB_LC <- readRDS('Data/Spatial_Data/ALB_LC.RDS')
GL2_flow <- readRDS('Data/Spatial_Data/GL2_flowline.RDS')
GL2_waterbody <- readRDS('Data/Spatial_Data/GL2_waterbody.RDS')
GL2_LC <- readRDS('Data/Spatial_Data/GL2_LC.RDS')
GL3_flow <- readRDS('Data/Spatial_Data/GL3_flowline.RDS')
GL3_waterbody <- readRDS('Data/Spatial_Data/GL3_waterbody.RDS')
GL3_LC <- readRDS('Data/Spatial_Data/GL3_LC.RDS')
GL4_flow <- readRDS('Data/Spatial_Data/GL4_flowline.RDS')
GL4_waterbody <- readRDS('Data/Spatial_Data/GL4_waterbody.RDS')
GL4_LC <- readRDS('Data/Spatial_Data/GL4_LC.RDS')
GL5_flow <- readRDS('Data/Spatial_Data/GL5_flowline.RDS')
GL5_waterbody <- readRDS('Data/Spatial_Data/GL5_waterbody.RDS')
GL5_LC <- readRDS('Data/Spatial_Data/GL5_LC.RDS')

# sampling locations
sites <- read.csv('Data/sites.csv') |>
  drop_na(lat) |>
  st_as_sf(coords=c('long','lat'),crs=4269)