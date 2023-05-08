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
#library(nhdplusTools)
library(sf)
library(ggspatial)
library(ggridges)
library(viridis)
library(kdensity)
library(confintr)
#library(randomForest)
#library(parsnip)
#library(yardstick)
library(ggcorrplot)
library(matrixStats)
library(trend)
library(broom)
library(mgcv)
library(Matrix)





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
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  mutate(date = as.Date(date)) |>
  filter(site != 'FLUME',
         eco_type != 'glacier',
         site != 'ALB_CAMP') |>
  mutate(network_position = factor(network_position, levels = c('2','3','4', '5', '6', 
                                                                '7','8','9','10','11','12',
                                                                '12a','13','14','15'))) |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  dplyr::select(-X)

stoich <- read.csv('Data/stoich_after_outliers_removed.csv')|>
  mutate(date = as.Date(date)) |>
  filter(site != 'FLUME',
         eco_type != 'glacier',
         site != 'ALB_CAMP') |>
  mutate(network_position = factor(network_position, levels = c('2','3','4', '5', '6', 
                                                                '7','8','9','10','11','12',
                                                                '12a','13','14','15'))) |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  dplyr::select(-X)

# discharge <- read.csv('Data/discharge_outliers_removed.csv')|>
#   mutate(date = as.Date(date)) |>
#   filter(site != 'FLUME') |>
#   mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', 
#                                                                 '7','8','9','10','11','12',
#                                                                 '12a','13','14','15','16'))) |>
#   dplyr::select(-X)

#### frequency plots outlier removed data ####
# ggplot(ions) +
#   geom_density(aes(result)) +
#   facet_wrap(.~param, scales = 'free') +
#   labs(title = 'outliers removed')


ggplot(nuts) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'outliers removed')

# ggplot(discharge) +
#   geom_density(aes(result)) +
#   facet_wrap(.~param, scales = 'free') +
#   labs(title = 'outliers removed')

ggplot(stoich) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'outliers removed') 


# read in spatial data
greenlakes_LC <- read_rds('Data/Spatial_Data/greenlakes_landcover.RDS')

# sampling locations
sites <- read.csv('Data/sites.csv') |>
  filter(site != 'FLUME',
         eco_type != 'glacier',
         site != 'ALB_CAMP') |>
  mutate(WS_Group = ifelse(site %in% c('ALB_INLET', 'ALB_LAKE', 'ALB_OUTLET', 'GL1_LAKE'),'ALB',
                           ifelse(site %in% c('GL2_LAKE'),'GL2',
                                  ifelse(site %in% c('GL3_INLET', 'GL3_LAKE', 'GL3_OUTLET'),'GL3',
                                         ifelse(site %in% c('GL4_INLET','GL4_LAKE', 'GL4_OUTLET'),'GL4',
                                                ifelse(site %in% c('GL5_INLET', 'GL5_LAKE', 'GL5_OUTLET'),'GL5', NA)))))) |>
  drop_na(lat) |>
  st_as_sf(coords=c('long','lat'),crs=4269)


# read in distances between locations first 
distances <- read.csv('Data/site_distances_Km.csv', header = TRUE)
rownames(distances) <- distances$X
distances[,1] <- NULL

Correct_Colnames <- function(df) {
  
  delete.columns <- grep("(^X$)|(^X\\.)(\\d+)($)", colnames(df), perl=T)
  
  if (length(delete.columns) > 0) {
    
    row.names(df) <- as.character(df[, grep("^X$", colnames(df))])
    #other data types might apply than character or 
    #introduction of a new separate column might be suitable
    
    df <- df[,-delete.columns]
    
    colnames(df) <- gsub("^X", "",  colnames(df))
    #X might be replaced by different characters, instead of being deleted
  }
  
  return(df)
}

distances <- as.matrix(Correct_Colnames(distances))
distances[lower.tri(distances, diag=TRUE)] <- NA
distances_Km <- as.data.frame(distances) |>
  select(-1) |>
  rownames_to_column('site1') |>
  pivot_longer(2:13, names_to = 'site2', values_to = 'distance_Km') |>
  drop_na()

