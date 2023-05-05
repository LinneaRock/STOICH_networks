#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Let's try to make a beautiful map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)
library(sf)
library(raster)

# Read in shapefiles, NLCD, and NHD data ####
## shapefiles for subwatersheds ####
## Albion (full network watershed)
ALB <- st_transform(st_read('C:/Users/linne/OneDrive - University of Wyoming/Spatial_Data/Niwot/Albion/area-of-interest.shp'), crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) # reproject to NAD83

## Green Lake 3 
GL3 <- st_transform(st_read('C:/Users/linne/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL3/area-of-interest.shp'),crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) # reproject to NAD83

## Green Lake 4 
GL4 <- st_transform(st_read('C:/Users/linne/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL4/area-of-interest.shp'), crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) # reproject to NAD83

## Green Lake 5 
GL5 <- st_transform(st_read('C:/Users/linne/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL5/area-of-interest.shp'), crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) # reproject to NAD83

## NHD lines, waterfeatures ####
# I just need to read in the Albion one to make a map, since that is representative of everything in the watershed :) 
waterfeatures <- readRDS('Data/Spatial_Data/ALB_waterFeatures.RDS')
waterfeatures <- st_transform(waterfeatures, crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

## NLCD raster and color scheme ####
# I just need to read in the Albion one to make a map, since that is representative of everything in the watershed :) 
nlcd <- readRDS('Data/Spatial_Data/ALB_LC.RDS')

nlcd <- spTransform(nlcd, crs=4269)
colors <- readRDS('Data/Spatial_Data/LCcolors.RDS') |>
  arrange(colors$Layer_1) |>
  filter(!Layer_1 %in% c('Devleoped, High Intensity', 'Emergent Herbaceous Wetlands')) |>
  mutate(Layer_1 = factor(Layer_1))
v <- colors$color

## sites ####
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



# Map time!! ####
ggplot() +
  geom_raster(nlcd, mapping=aes(x,y, fill=Layer_1)) +
  scale_fill_manual('', values=as.character(v)) +
  geom_sf(waterfeatures, mapping=aes(),color='#476ba1', fill='#476ba1') +
  geom_sf(ALB, mapping=aes(), color='black', alpha=0) +
  geom_sf(GL5, mapping=aes(), color='black', alpha=0) +
  geom_sf(GL4, mapping=aes(), color='black', alpha=0) +
  geom_sf(GL3, mapping=aes(), color='black', alpha=0) +
  geom_sf(sites, mapping=aes(), color = 'hotpink') +
  theme_bw() +
  labs(x='',y='')

ggsave('Figures/Location_Map.png', height=4.5,width=4.5, units='in',dpi=1200)


