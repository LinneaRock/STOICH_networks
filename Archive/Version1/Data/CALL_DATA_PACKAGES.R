# call in packages

library(tidyverse)
library(dataRetrieval) # for water year
library(lubridate)
library(scales)
library(ggpubr)
library(multcompView)
#library(ggtern)
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
library(zyp)
library(broom)
library(mgcv)
library(Matrix)
#library(ggdark) # load as needed for presentation dark-theme figures
library(patchwork)




# call in data  ####
# add in streamflow based seasonal component ####
ave_percentile_days <- read.csv('Data/szn_streamflow/ave_percentile_days.csv') 
ave_percentile_days #166, 194, 229

# this is all the actual dates of season differences for 2009-2020 - but we are now using the average for all years instead
# percentile_days <- read.csv('Data/szn_streamflow/percentile_days.csv') |>
#   mutate(day_20th = as.Date(day_20th),
#          day_50th = as.Date(day_50th),
#          day_80th = as.Date(day_80th)) |>
#   select(-X)

nuts <- read.csv('Data/nuts_outliers_removed.csv')|>
  select(-season) |>
  filter(!param %in% c('TOC_mgL','DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  mutate(Date = as.Date(date)) |>
  filter(site != 'FLUME') |>
         #eco_type != 'glacier',
         #site != 'ALB_CAMP') |>
  # mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', 
  #                                                               '7','8','9','10','11','12',
  #                                                               '12a','13','14','15'))) |>
  mutate(mon = month(date)) |> #and add seasons to the dataframe
  dplyr::select(-X) |>
  addWaterYear() |>
  # join calculated days
  #left_join(percentile_days) |>
  # add estimated average days for 20th, 50th, 80th percentiles
  mutate(origin = as.Date(paste0(waterYear, '-01-01'))) |>
  mutate(day_20th = as.Date(166, origin=origin)-1) |>
 # mutate(day_20th = as.Date(day_20th)) |>
  mutate(day_50th = as.Date(194, origin=origin)-1) |>
 # mutate(day_50th = as.Date(day_50th)) |>
  mutate(day_80th = as.Date(229, origin=origin)-1) |>
 # mutate(day_80th = as.Date(day_80th)) |>
  # mutate(day_20th = ifelse(is.na(day_20th), as.Date(166, origin=origin)-1, day_20th)) |>
  # mutate(day_20th = as.Date(day_20th)) |>
  # mutate(day_50th = ifelse(is.na(day_50th), as.Date(194, origin=origin)-1, day_50th)) |>
  # mutate(day_50th = as.Date(day_50th)) |>
  # mutate(day_80th = ifelse(is.na(day_80th), as.Date(229, origin=origin)-1, day_80th)) |>
  # mutate(day_80th = as.Date(day_80th)) |>
  #  add seasons
  mutate(szn=ifelse(between(Date, day_20th, day_50th), 'spring snowmelt',
                    ifelse(between(Date, day_50th, day_80th), 'falling limb',
                           'baseflow'))) |>
  select(-c(day_80th,day_50th,day_20th, origin))
 # select(-c(day_80th,day_50th,day_20th,day_80th_doy,day_50th_doy,day_20th_doy, origin))


stoich <- read.csv('Data/stoich_outliers_removed.csv')|>
  mutate(Date = as.Date(date)) |>
  select(-season) |>
  filter(site != 'FLUME',
        # eco_type != 'glacier',
         site != 'ALB_CAMP') |>
  mutate(network_position = factor(network_position, levels = c('1', '2','3','4', '5', '6', 
                                                                '7','8','9','10','11','12',
                                                                '12a','13','14','15'))) |>
  mutate(mon = month(date)) |> #and add seasons to the dataframe
  dplyr::select(-X) |>
  addWaterYear() |>
  # join calculated days
  #left_join(percentile_days) |>
  # add estimated average days for 20th, 50th, 80th percentiles
  mutate(origin = as.Date(paste0(waterYear, '-01-01'))) |>
  mutate(day_20th = as.Date(166, origin=origin)-1) |>
  # mutate(day_20th = as.Date(day_20th)) |>
  mutate(day_50th = as.Date(194, origin=origin)-1) |>
  # mutate(day_50th = as.Date(day_50th)) |>
  mutate(day_80th = as.Date(229, origin=origin)-1) |>
  # mutate(day_80th = as.Date(day_80th)) |>
  # mutate(day_20th = ifelse(is.na(day_20th), as.Date(166, origin=origin)-1, day_20th)) |>
  # mutate(day_20th = as.Date(day_20th)) |>
  # mutate(day_50th = ifelse(is.na(day_50th), as.Date(194, origin=origin)-1, day_50th)) |>
  # mutate(day_50th = as.Date(day_50th)) |>
  # mutate(day_80th = ifelse(is.na(day_80th), as.Date(229, origin=origin)-1, day_80th)) |>
  # mutate(day_80th = as.Date(day_80th)) |>
  #  add seasons
  mutate(szn=ifelse(between(Date, day_20th, day_50th), 'spring snowmelt',
                    ifelse(between(Date, day_50th, day_80th), 'falling limb',
                           'baseflow'))) |>
  select(-c(day_80th,day_50th,day_20th, origin))
# select(-c(day_80th,day_50th,day_20th,day_80th_doy,day_50th_doy,day_20th_doy, origin))

discharge <- read.csv('Data/discharge_outliers_removed.csv')|>
  mutate(Date = as.Date(date)) |>
  filter(site != 'FLUME') |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6',
                                                                '7','8','9','10','11','12',
                                                                '12a','13','14','15','16'))) |>
  dplyr::select(-X) |>
  addWaterYear() |>
  # join calculated days
  #left_join(percentile_days) |>
  # add estimated average days for 20th, 50th, 80th percentiles
  mutate(origin = as.Date(paste0(waterYear, '-01-01'))) |>
  mutate(day_20th = as.Date(166, origin=origin)-1) |>
  # mutate(day_20th = as.Date(day_20th)) |>
  mutate(day_50th = as.Date(194, origin=origin)-1) |>
  # mutate(day_50th = as.Date(day_50th)) |>
  mutate(day_80th = as.Date(229, origin=origin)-1) |>
  # mutate(day_80th = as.Date(day_80th)) |>
  # mutate(day_20th = ifelse(is.na(day_20th), as.Date(166, origin=origin)-1, day_20th)) |>
  # mutate(day_20th = as.Date(day_20th)) |>
  # mutate(day_50th = ifelse(is.na(day_50th), as.Date(194, origin=origin)-1, day_50th)) |>
  # mutate(day_50th = as.Date(day_50th)) |>
  # mutate(day_80th = ifelse(is.na(day_80th), as.Date(229, origin=origin)-1, day_80th)) |>
  # mutate(day_80th = as.Date(day_80th)) |>
  #  add seasons
  mutate(szn=ifelse(between(Date, day_20th, day_50th), 'spring snowmelt',
                    ifelse(between(Date, day_50th, day_80th), 'falling limb',
                           'baseflow'))) |>
  select(-c(day_80th,day_50th,day_20th, origin))
# select(-c(day_80th,day_50th,day_20th,day_80th_doy,day_50th_doy,day_20th_doy, origin))

#rm(percentile_days)
rm(ave_percentile_days)





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
         #eco_type != 'glacier',
         site != 'ALB_CAMP') |>
  mutate(WS_Group = ifelse(site %in% c('ALB_INLET', 'ALB_LAKE', 'ALB_OUTLET', 'GL1_LAKE'),'ALB',
                           ifelse(site %in% c('GL2_LAKE'),'GL2',
                                  ifelse(site %in% c('GL3_INLET', 'GL3_LAKE', 'GL3_OUTLET'),'GL3',
                                         ifelse(site %in% c('GL4_INLET','GL4_LAKE', 'GL4_OUTLET'),'GL4',
                                                ifelse(site %in% c('ARIKAREE', 'GL5_INLET', 'GL5_LAKE', 'GL5_OUTLET'),'GL5', NA)))))) |>
  drop_na(lat) |>
  st_as_sf(coords=c('long','lat'),crs=4269) |>
  drop_na(upstream_network_lakes)


# read in distances between locations first 
distances <- readxl::read_xlsx('Data/Results/site_distances_Km.xlsx')
# rownames(distances) <- distances$X
# distances[,1] <- NULL

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

distances <-as.matrix(Correct_Colnames(distances))
#distances[lower.tri(distances, diag=TRUE)] <- NA
distances_Km <- as.data.frame(distances) |>
  rename(site1=1) |>
  pivot_longer(2:16, names_to = 'site2', values_to = 'distance_Km') |>
  drop_na()

rm(distances)
rm(Correct_Colnames)



# get basic stats for each site, parameter ####
get_stats <- function(data) {
  
  tmp <- data |>
    group_by(site, eco_type, network_position, param, szn) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() #|>
  # mutate(CV = (SD/mean) * 100) 
  
  tmp2 <- data |>
    group_by(site, eco_type, network_position, param) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() |>
    mutate(#CV = (SD/mean) * 100,
      szn = 'All data') 
  
  df <- rbind(tmp, tmp2)
  
  rm(tmp)
  rm(tmp2)
  return(df)
  
}

#stats_ions <- get_stats(ions)
stats_nuts <- get_stats(nuts)
stats_stoich <- get_stats(stoich)



# get basic stats for each ecotype, parameter ####
get_stats_eco <- function(data) {
  
  tmp <- data |>
    group_by(eco_type,param, szn) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() #|>
  #mutate(CV = (SE/mean) * 100) 
  
  tmp2 <- data |>
    group_by(eco_type, param) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() |>
    mutate(#CV = (SD/mean) * 100,
      szn = 'All data') 
  
  df <- rbind(tmp, tmp2)
  
  rm(tmp)
  rm(tmp2)
  return(df)
  
}

#eco_stats_ions <- get_stats_eco(ions)
eco_stats_nuts <- get_stats_eco(nuts)
eco_stats_stoich <- get_stats_eco(stoich)

rm(get_stats_eco, get_stats)


# removing GL1 from analyses becuase nothing really interesting showed up and we decided we needed to simplify the MS a lot from the original behemoth I wrote :(


discharge <- discharge |> filter(site!='GL1_LAKE')
distances_Km <- distances_Km |> filter(site1!='GL1_LAKE',
                                       site2!='GL1_LAKE')
nuts <- nuts |> filter(site!='GL1_LAKE')
sites <- sites |> filter(site!='GL1_LAKE')
stats_nuts <- stats_nuts |> filter(site!='GL1_LAKE')
stats_stoich <- stats_stoich |> filter(site!='GL1_LAKE')
stoich <- stoich |> filter(site!='GL1_LAKE')
