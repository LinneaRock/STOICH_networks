#------------------------#
# Spatial data wrangling 
#------------------------#

library(tidyverse)
library(sf)
library(rgdal)
library(mapview)


# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list_NHD <- ogrListLayers('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHDPLUS_H_1019_HU4_GDB.gdb')
print(fc_list_NHD)


subset(ogrDrivers(), grepl("GDB", name))
fc_list_NLCD <- ogrListLayers('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NLCD_2019')
print(fc_list_NLCD)

sites <- read.csv('Data/sites.csv') |>
  drop_na(lat) |>
  st_as_sf(coords=c('long','lat'),crs=4326)
class(sites)

nlcd <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NLCD_2019', 
                layer='nlcd_2019_land_cover_l48_20210604')

NHDflowline <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHDPLUS_H_1019_HU4_GDB.gdb', layer = 'NHDFlowline')

NHDwaterbody <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHDPLUS_H_1019_HU4_GDB.gdb', layer = 'NHDWaterbody')

NHDmetadata <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHDPLUS_H_1019_HU4_GDB.gdb', layer = 'NHDMetadata')

ALB <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/Albion/area-of-interest.shp')


NHDflowline_reproj <- st_transform(NHDflowline, 4326)  # reproject

class(NHDflowline_reproj)
st_crs(NHDflowline_reproj)

ggplot() +
  geom_sf(st_zm(NHDflowline_reproj), mapping=aes())

class(ALB)
st_crs(ALB)

ALB_reproj <- st_transform(ALB, 4326) # reproject

st_crs(NHDflowline_reproj) == st_crs(ALB_reproj)
ALB_ws_flowline <- st_crop(NHDflowline_reproj, ALB_reproj)
mapview(ALB_ws_flowline) + mapview(sites) + mapview(ALB_reproj)



ggplot() +
  geom_sf(ALB_ws_flowline, mapping=aes()) +
  geom_sf(sites, mapping=aes())


NHDwaterbody_reproj <- st_transform(NHDwaterbody, 4326)
st_crs(NHDwaterbody_reproj) == st_crs(ALB_reproj)
ALB_ws_waterbody <- st_intersection(NHDwaterbody_reproj, ALB_reproj)











str(NHDflowline)
st_crs(ALB_ws) == st_crs(NHDflowline)

st_write(NHDflowline,'flowline.shp')
st_crs(NHDflowline)

nhdflowline2 <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHD_flowline_prj.shp')
st_crs(nhdflowline2)

ALB_ws2 <- st_read('albws_wgs84.shp')
st_crs(ALB_ws2)
projection(ALB_ws, proj4string="+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84")

crs(ALB_ws) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
class(ALB_ws)
raster::crs(ALB_ws) <- 'EPSG:4326'

ALB_ws_flowline <- st_crop(nhdflowline2, ALB_ws2)
mapview(ALB_ws_flowline) + mapview(sites)


st_crs(nhdflowline2)
st_crs(ALB_ws2)


library(raster)
test <- spTransform(nhdflowline2, crs=4326)


ggplot() +
  geom_sf(data=NHDflowline_reproj)
