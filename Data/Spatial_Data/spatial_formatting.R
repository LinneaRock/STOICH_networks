#------------------------#
# Spatial data wrangling 
#------------------------#

# 1. Read in libraries ####
library(tidyverse)
library(sf)
library(rgdal)
library(mapview)
library(raster)
library(stars)


# 2. Read in data ####
# List all feature classes in a file geodatabase for the NHD data
subset(ogrDrivers(), grepl("GDB", name))
fc_list_NHD <- ogrListLayers('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHDPLUS_H_1019_HU4_GDB.gdb')
print(fc_list_NHD)

### Get the sampling locations ####
sites <- read.csv('Data/sites.csv') |>
  drop_na(lat) |>
  st_as_sf(coords=c('long','lat'),crs=4269)
class(sites) # sf, df
crs(sites) # NAD83

### Get the NLCD 2019 data ####
NLCD_raster <- raster('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NLCD_2019/NLCD_2019_Land_Cover_L48_20210604_bbxDBdpHfb7O82puKmaF.tiff')
#NLCD_raster <- raster('C:/Users/lrock1/Downloads/NLCD2019_Linnea.tif')
class(NLCD_raster)
crs(NLCD_raster) # NAD83

NLCD <- as.data.frame(NLCD_raster, xy=TRUE) #|> rename(Layer_1=NLCD2019_Linnea)


# Layer number codes can be seen in NLCD Land Cover.gif
NLCD <- NLCD |>
  mutate(Layer_1 = case_when(Layer_1==11~'Open Water',
                             Layer_1==12~'Perennial Ice/Snow',
                             Layer_1==24~'Devleoped, High Intensity',
                             Layer_1==31~'Barren Land (Rock/Sand/Clay)',
                             Layer_1==42~'Evergreen Forest',
                             Layer_1==52~'Shrub/Scrub',
                             Layer_1==71~'Grasslands/Herbaceous',
                             Layer_1==90~'Woody Wetlands',
                             Layer_1==95~'Emergent Herbaceous Wetlands')) 

LCcolors <- NLCD |>
  dplyr::select(Layer_1) |>
  unique() |>
  drop_na() |>
  mutate(color = case_when(Layer_1=='Open Water'~'#476ba1',
                               Layer_1=='Perennial Ice/Snow'~'#d1defa',
                               Layer_1=='Devleoped, High Intensity'~'#ab0000',
                               Layer_1=='Barren Land (Rock/Sand/Clay)'~'#b3aea3',
                               Layer_1=='Evergreen Forest'~'#1c6330',
                               Layer_1=='Shrub/Scrub'~'#ccba7d',
                               Layer_1=='Grasslands/Herbaceous'~'#e3e3c2',
                               Layer_1=='Woody Wetlands'~'#bad9eb',
                               Layer_1=='Emergent Herbaceous Wetlands'~'#b5d4e6'))

write_rds(LCcolors, 'Data/Spatial_Data/LCcolors.RDS')

# Check
ggplot() +
  geom_raster(data = NLCD, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()


### Get NHD data! ####
## Flowlines
NHDflowline <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHDPLUS_H_1019_HU4_GDB.gdb', layer = 'NHDFlowline')
class(NHDflowline) # sf, df
crs(NHDflowline) # NAD83

## waterbody outlines
NHDwaterbody <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHDPLUS_H_1019_HU4_GDB.gdb', layer = 'NHDWaterbody')
class(NHDwaterbody)# sf, df
crs(NHDwaterbody) # NAD83

### Watershed shapefiles ####
## Albion (full network watershed)
ALB <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/Albion/area-of-interest.shp')

class(ALB)
st_crs(ALB)
ALB <- st_transform(ALB, crs=4269) # reproject to NAD83
crs(ALB)

# make this projection for the raster 
alb_ogr <- readOGR('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/Albion/area-of-interest.shp')
crs(NLCD_raster)@projargs
crs(alb_ogr)@projargs
ALB_raster <- spTransform(alb_ogr, crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

## Green Lake 2 
GL2 <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL2/area-of-interest.shp')

class(GL2)
st_crs(GL2)
GL2 <- st_transform(GL2, crs=4269) # reproject to NAD83
crs(GL2)

# make this projection for the raster 
GL2_ogr <- readOGR('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL2/area-of-interest.shp')
crs(NLCD_raster)@projargs
crs(GL2_ogr)@projargs
GL2_raster <- spTransform(GL2_ogr, crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

## Green Lake 3 
GL3 <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL3/area-of-interest.shp')

class(GL3)
st_crs(GL3)
GL3 <- st_transform(GL3, crs=4269) # reproject to NAD83
crs(GL3)

# make this projection for the raster 
GL3_ogr <- readOGR('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL3/area-of-interest.shp')
crs(NLCD_raster)@projargs
crs(GL3_ogr)@projargs
GL3_raster <- spTransform(GL3_ogr, crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


## Green Lake 4 
GL4 <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL4/area-of-interest.shp')

class(GL4)
st_crs(GL4)
GL4 <- st_transform(GL4, crs=4269) # reproject to NAD83
crs(GL4)

# make this projection for the raster 
GL4_ogr <- readOGR('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL4/area-of-interest.shp')
crs(NLCD_raster)@projargs
crs(GL4_ogr)@projargs
GL4_raster <- spTransform(GL4_ogr, crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


## Green Lake 5 
GL5 <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL5/area-of-interest.shp')

class(GL5)
st_crs(GL5)
GL5 <- st_transform(GL5, crs=4269) # reproject to NAD83
crs(GL5)

# make this projection for the raster 
GL5_ogr <- readOGR('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/GL5/area-of-interest.shp')
crs(NLCD_raster)@projargs
crs(GL5_ogr)@projargs
GL5_raster <- spTransform(GL5_ogr, crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


# 3. Check that rasters/sf object projections match as appropriate ####
st_crs(NHDflowline) == st_crs(ALB) 
st_crs(ALB) == st_crs(NHDwaterbody)
st_crs(NLCD_raster) == st_crs(ALB_raster)


st_crs(NHDflowline) == st_crs(GL2) 
st_crs(GL2) == st_crs(NHDwaterbody)
st_crs(NLCD_raster) == st_crs(GL2_raster)

st_crs(NHDflowline) == st_crs(GL3) 
st_crs(GL3) == st_crs(NHDwaterbody)
st_crs(NLCD_raster) == st_crs(GL3_raster)

st_crs(NHDflowline) == st_crs(GL4) 
st_crs(GL4) == st_crs(NHDwaterbody)

st_crs(NHDflowline) == st_crs(GL5) 
st_crs(GL5) == st_crs(NHDwaterbody)
st_crs(NLCD_raster) == st_crs(GL5_raster)


# 4. Crop! ####
### Crop to Albion (full watershed) ####
ALB_ws_flowline <- st_intersection(NHDflowline, ALB)
ALB_ws_waterbody <- st_intersection(NHDwaterbody |> st_make_valid(), ALB)
ALB_ws_LC_raster <- mask(crop(NLCD_raster, ALB_raster), ALB_raster)


ALB_ws_LC <- ALB_ws_LC_raster |>
  as.data.frame(xy=TRUE) |>
  mutate(Layer_1 = case_when(Layer_1==11~'Open Water',
                             Layer_1==12~'Perennial Ice/Snow',
                             Layer_1==24~'Devleoped, High Intensity',
                             Layer_1==31~'Barren Land (Rock/Sand/Clay)',
                             Layer_1==42~'Evergreen Forest',
                             Layer_1==52~'Shrub/Scrub',
                             Layer_1==71~'Grasslands/Herbaceous',
                             Layer_1==90~'Woody Wetlands',
                             Layer_1==95~'Emergent Herbaceous Wetlands')) |>
  drop_na() |>
  group_by(Layer_1) |>
  mutate(pixels=n()) |>
  mutate(LandCoverArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area
  ungroup() |>
  mutate(pixels=n()) |>
  mutate(DrainageArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area
  ungroup()


# Check land use plot
ggplot() +
  geom_raster(data = ALB_ws_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()

# Check flowlines and waterbodies
ggplot() +
  geom_sf(ALB, mapping=aes()) +
  geom_sf(ALB_ws_flowline, mapping=aes()) +
  geom_sf(ALB_ws_waterbody, mapping=aes())



### Crop to GL2 ####
GL2_ws_flowline <- st_intersection(NHDflowline, GL2)
GL2_ws_waterbody <- st_intersection(NHDwaterbody |> st_make_valid(), GL2)
GL2_ws_LC_raster <- mask(crop(NLCD_raster, GL2_raster), GL2_raster)

GL2_ws_LC <- GL2_ws_LC_raster |>
  as.data.frame(xy=TRUE) |>
  mutate(Layer_1 = case_when(Layer_1==11~'Open Water',
                             Layer_1==12~'Perennial Ice/Snow',
                             Layer_1==24~'Devleoped, High Intensity',
                             Layer_1==31~'Barren Land (Rock/Sand/Clay)',
                             Layer_1==42~'Evergreen Forest',
                             Layer_1==52~'Shrub/Scrub',
                             Layer_1==71~'Grasslands/Herbaceous',
                             Layer_1==90~'Woody Wetlands',
                             Layer_1==95~'Emergent Herbaceous Wetlands')) |>
  drop_na() |>
  group_by(Layer_1) |>
  mutate(pixels=n()) |>
  mutate(LandCoverArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area
  ungroup() |>
  mutate(pixels=n()) |>
  mutate(DrainageArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area
  ungroup()


# Check land use plot
ggplot() +
  geom_raster(data = GL2_ws_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()


# Check flowlines and waterbodies
ggplot() +
  geom_sf(GL2, mapping=aes()) +
  geom_sf(GL2_ws_flowline, mapping=aes()) +
  geom_sf(GL2_ws_waterbody, mapping=aes())


### Crop to GL3 ####
GL3_ws_flowline <- st_intersection(NHDflowline, GL3)
GL3_ws_waterbody <- st_intersection(NHDwaterbody |> st_make_valid(), GL3)
GL3_ws_LC_raster <- mask(crop(NLCD_raster, GL3_raster), GL3_raster)

GL3_ws_LC <- GL3_ws_LC_raster |>
  as.data.frame(xy=TRUE) |>
  mutate(Layer_1 = case_when(Layer_1==11~'Open Water',
                             Layer_1==12~'Perennial Ice/Snow',
                             Layer_1==24~'Devleoped, High Intensity',
                             Layer_1==31~'Barren Land (Rock/Sand/Clay)',
                             Layer_1==42~'Evergreen Forest',
                             Layer_1==52~'Shrub/Scrub',
                             Layer_1==71~'Grasslands/Herbaceous',
                             Layer_1==90~'Woody Wetlands',
                             Layer_1==95~'Emergent Herbaceous Wetlands')) |>
  drop_na() |>
  group_by(Layer_1) |>
  mutate(pixels=n()) |>
  mutate(LandCoverArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area
  ungroup() |>
  mutate(pixels=n()) |>
  mutate(DrainageArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area
  ungroup() 


# Check land use plot
ggplot() +
  geom_raster(data = GL3_ws_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()


# Check flowlines and waterbodies
ggplot() +
  geom_sf(GL3, mapping=aes()) +
  geom_sf(GL3_ws_flowline, mapping=aes()) +
  geom_sf(GL3_ws_waterbody, mapping=aes())



### Crop to GL4 ####
GL4_ws_flowline <- st_intersection(NHDflowline, GL4)
GL4_ws_waterbody <- st_intersection(NHDwaterbody |> st_make_valid(), GL4)
GL4_ws_LC_raster <- mask(crop(NLCD_raster, GL4_raster), GL4_raster)

GL4_ws_LC <- GL4_ws_LC_raster |>
  as.data.frame(xy=TRUE) |>
  mutate(Layer_1 = case_when(Layer_1==11~'Open Water',
                             Layer_1==12~'Perennial Ice/Snow',
                             Layer_1==24~'Devleoped, High Intensity',
                             Layer_1==31~'Barren Land (Rock/Sand/Clay)',
                             Layer_1==42~'Evergreen Forest',
                             Layer_1==52~'Shrub/Scrub',
                             Layer_1==71~'Grasslands/Herbaceous',
                             Layer_1==90~'Woody Wetlands',
                             Layer_1==95~'Emergent Herbaceous Wetlands')) |>
  drop_na() |>
  group_by(Layer_1) |>
  mutate(pixels=n()) |>
  mutate(LandCoverArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area
  ungroup() |>
  mutate(pixels=n()) |>
  mutate(DrainageArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area
  ungroup() 


# Check land use plot
ggplot() +
  geom_raster(data = GL4_ws_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()


# Check flowlines and waterbodies
ggplot() +
  geom_sf(GL4, mapping=aes()) +
  geom_sf(GL4_ws_flowline, mapping=aes()) +
  geom_sf(GL4_ws_waterbody, mapping=aes())



### Crop to GL5 ####
GL5_ws_flowline <- st_intersection(NHDflowline, GL5)
GL5_ws_waterbody <- st_intersection(NHDwaterbody |> st_make_valid(), GL5)
GL5_ws_LC_raster <- mask(crop(NLCD_raster, GL5_raster), GL5_raster)

GL5_ws_LC <- GL5_ws_LC_raster |>
  as.data.frame(xy=TRUE) |>
  mutate(Layer_1 = case_when(Layer_1==11~'Open Water',
                             Layer_1==12~'Perennial Ice/Snow',
                             Layer_1==24~'Devleoped, High Intensity',
                             Layer_1==31~'Barren Land (Rock/Sand/Clay)',
                             Layer_1==42~'Evergreen Forest',
                             Layer_1==52~'Shrub/Scrub',
                             Layer_1==71~'Grasslands/Herbaceous',
                             Layer_1==90~'Woody Wetlands',
                             Layer_1==95~'Emergent Herbaceous Wetlands')) |>
  drop_na() |>
  group_by(Layer_1) |>
  mutate(pixels=n()) |>
  mutate(LandCoverArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area -- this is the total land cover in the subwatershed -- but will still plot correctly as the raster features are intact.
  ungroup() |>
  mutate(pixels=n()) |>
  mutate(DrainageArea_km2 = round((pixels*30*30)/1e6,3)) |> # multiply by pixel area -- this is the total drainage area
  ungroup() 


# Check land use plot
ggplot() +
  geom_raster(data = GL5_ws_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()


# Check flowlines and waterbodies
ggplot() +
  geom_sf(GL5, mapping=aes()) +
  geom_sf(GL5_ws_flowline, mapping=aes()) +
  geom_sf(GL5_ws_waterbody, mapping=aes())



# 5. Save files ####
write_rds(ALB_ws_flowline, 'Data/Spatial_Data/ALB_flowline.RDS')
write_rds(ALB_ws_waterbody, 'Data/Spatial_Data/ALB_waterbody.RDS')
write_rds(ALB_ws_LC, 'Data/Spatial_Data/ALB_LC.RDS')
write_rds(ALB_ws_LC_raster, 'Data/Spatial_Data/ALB_LC_raster.RDS')

write_rds(GL2_ws_flowline, 'Data/Spatial_Data/GL2_flowline.RDS')
write_rds(GL2_ws_waterbody, 'Data/Spatial_Data/GL2_waterbody.RDS')
write_rds(GL2_ws_LC, 'Data/Spatial_Data/GL2_LC.RDS')
write_rds(GL2_ws_LC_raster, 'Data/Spatial_Data/GL2_LC_raster.RDS')

write_rds(GL3_ws_flowline, 'Data/Spatial_Data/GL3_flowline.RDS')
write_rds(GL3_ws_waterbody, 'Data/Spatial_Data/GL3_waterbody.RDS')
write_rds(GL3_ws_LC, 'Data/Spatial_Data/GL3_LC.RDS')
write_rds(GL3_ws_LC_raster, 'Data/Spatial_Data/GL3_LC_raster.RDS')

write_rds(GL4_ws_flowline, 'Data/Spatial_Data/GL4_flowline.RDS')
write_rds(GL4_ws_waterbody, 'Data/Spatial_Data/GL4_waterbody.RDS')
write_rds(GL4_ws_LC, 'Data/Spatial_Data/GL4_LC.RDS')
write_rds(GL4_ws_LC_raster, 'Data/Spatial_Data/GL4_LC_raster.RDS')

write_rds(GL5_ws_flowline, 'Data/Spatial_Data/GL5_flowline.RDS')
write_rds(GL5_ws_waterbody, 'Data/Spatial_Data/GL5_waterbody.RDS')
write_rds(GL5_ws_LC, 'Data/Spatial_Data/GL5_LC.RDS')
write_rds(GL5_ws_LC_raster, 'Data/Spatial_Data/GL5_LC_raster.RDS')



# FORMAT DATA for DATA ANALYSIS ####
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



GL5_flow <- readRDS('Data/Spatial_Data/GL5_flowline.RDS') |>
  mutate(WS_Group = 'GL5') |>
  dplyr::select(WS_Group, FType, ReachCode, LengthKM, GNIS_Name, Permanent_Identifier, WBArea_Permanent_Identifier, Shape_Length, NHDPlusID, Shape)

GL5_waterbody <- readRDS('Data/Spatial_Data/GL5_waterbody.RDS') |>
  mutate(WS_Group = 'GL5') |>
  mutate(Site = ifelse(GNIS_ID == '00202579', 'GL5_LAKE', GNIS_ID)) |>
  dplyr::select(WS_Group, Site, FType, ReachCode, AreaSqKm, GNIS_Name, Permanent_Identifier, Shape_Length, NHDPlusID, Shape) 

GL5_waterFeatures <- bind_rows(GL5_flow, GL5_waterbody)
write_rds(GL5_waterFeatures, 'Data/Spatial_Data/GL5_waterFeatures.RDS')

GL5_LC <- readRDS('Data/Spatial_Data/GL5_LC.RDS') |>
  mutate(WS_Group = 'GL5') 

ggplot() +
  geom_raster(data = GL3_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()

# this is a studpid manual workaround :/
gl5.tmp <-data.frame(WS_Group=c('GL5', 'GL5'), Layer_1=c('Drainage Area', 'Lake Area'), LandCoverArea_km2=c(as.numeric(unique((GL5_LC |> dplyr::select(DrainageArea_km2)))), as.numeric(unique((GL5_waterbody |> filter(Site=='GL5_LAKE') |> dplyr::select(AreaSqKm))))[1]))

GL5_landuse <- GL5_LC |>
  dplyr::select(WS_Group, Layer_1, LandCoverArea_km2) |>
  distinct() |>
  rbind(gl5.tmp)



GL4_flow <- readRDS('Data/Spatial_Data/GL4_flowline.RDS') |>
  mutate(WS_Group = 'GL4') |>
  dplyr::select(WS_Group, FType, ReachCode, LengthKM, GNIS_Name, Permanent_Identifier, WBArea_Permanent_Identifier, Shape_Length, NHDPlusID, Shape)

GL4_waterbody <- readRDS('Data/Spatial_Data/GL4_waterbody.RDS') |>
  mutate(WS_Group = 'GL4') |>
  mutate(Site = ifelse(ReachCode == '10190005001170', 'GL5_LAKE', 
                       ifelse(ReachCode== '10190005001171', 'GL4_LAKE', NA))) |>
  dplyr::select(WS_Group, Site, FType, ReachCode, AreaSqKm, GNIS_Name, Permanent_Identifier, Shape_Length, NHDPlusID, Shape) 

GL4_waterFeatures <- bind_rows(GL4_flow, GL4_waterbody)
write_rds(GL4_waterFeatures, 'Data/Spatial_Data/GL4_waterFeatures.RDS')

GL4_LC <- readRDS('Data/Spatial_Data/GL4_LC.RDS') |>
  mutate(WS_Group = 'GL4') 

ggplot() +
  geom_raster(data = GL4_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()

# this is a studpid manual workaround :/
gl4.tmp <-data.frame(WS_Group=c('GL4', 'GL4'), Layer_1=c('Drainage Area', 'Lake Area'), LandCoverArea_km2=c(as.numeric(unique((GL4_LC |> dplyr::select(DrainageArea_km2)))), as.numeric(unique((GL4_waterbody |> filter(Site=='GL4_LAKE') |> dplyr::select(AreaSqKm))))[1]))

GL4_landuse <- GL4_LC |>
  dplyr::select(WS_Group, Layer_1, LandCoverArea_km2) |>
  distinct() |>
  rbind(gl4.tmp)





GL3_flow <- readRDS('Data/Spatial_Data/GL3_flowline.RDS') |>
  mutate(WS_Group = 'GL3') |>
  dplyr::select(WS_Group, FType, ReachCode, LengthKM, GNIS_Name, Permanent_Identifier, WBArea_Permanent_Identifier, Shape_Length, NHDPlusID, Shape)

GL3_waterbody <- readRDS('Data/Spatial_Data/GL3_waterbody.RDS') |>
  mutate(WS_Group = 'GL3') |>
  mutate(Site = ifelse(ReachCode == '10190005001170', 'GL5_LAKE', 
                       ifelse(ReachCode== '10190005001171', 'GL4_LAKE',
                              ifelse(ReachCode =='10190005001172', 'GL3_LAKE', NA)))) |>
  dplyr::select(WS_Group, Site, FType, ReachCode, AreaSqKm, GNIS_Name, Permanent_Identifier, Shape_Length, NHDPlusID, Shape) 

GL3_waterFeatures <- bind_rows(GL3_flow, GL3_waterbody)
write_rds(GL3_waterFeatures, 'Data/Spatial_Data/GL3_waterFeatures.RDS')

GL3_LC <- readRDS('Data/Spatial_Data/GL3_LC.RDS') |>
  mutate(WS_Group = 'GL3') 

ggplot() +
  geom_raster(data = GL3_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()

# this is a studpid manual workaround :/
GL3.tmp <-data.frame(WS_Group=c('GL3', 'GL3'), Layer_1=c('Drainage Area', 'Lake Area'), LandCoverArea_km2=c(as.numeric(unique((GL3_LC |> dplyr::select(DrainageArea_km2)))), as.numeric(unique((GL3_waterbody |> filter(Site=='GL3_LAKE') |> dplyr::select(AreaSqKm))))[1]))

GL3_landuse <- GL3_LC |>
  dplyr::select(WS_Group, Layer_1, LandCoverArea_km2) |>
  distinct() |>
  rbind(GL3.tmp)




GL2_flow <- readRDS('Data/Spatial_Data/GL2_flowline.RDS') |>
  mutate(WS_Group = 'GL2') |>
  dplyr::select(WS_Group, FType, ReachCode, LengthKM, GNIS_Name, Permanent_Identifier, WBArea_Permanent_Identifier, Shape_Length, NHDPlusID, Shape)

GL2_waterbody <- readRDS('Data/Spatial_Data/GL2_waterbody.RDS') |>
  mutate(WS_Group = 'GL2') |>
  mutate(Site = ifelse(ReachCode == '10190005001170', 'GL5_LAKE', 
                       ifelse(ReachCode== '10190005001171', 'GL4_LAKE',
                              ifelse(ReachCode =='10190005001172', 'GL3_LAKE', 
                                     ifelse(ReachCode == '10190005001175', 'GL2_LAKE', NA))))) |>
  dplyr::select(WS_Group, Site, FType, ReachCode, AreaSqKm, GNIS_Name, Permanent_Identifier, Shape_Length, NHDPlusID, Shape) 

GL2_waterFeatures <- bind_rows(GL2_flow, GL2_waterbody)
write_rds(GL2_waterFeatures, 'Data/Spatial_Data/GL2_waterFeatures.RDS')

GL2_LC <- readRDS('Data/Spatial_Data/GL2_LC.RDS') |>
  mutate(WS_Group = 'GL2') 

ggplot() +
  geom_raster(data = GL2_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()

# this is a studpid manual workaround :/
GL2.tmp <-data.frame(WS_Group=c('GL2', 'GL2'), Layer_1=c('Drainage Area', 'Lake Area'), LandCoverArea_km2=c(as.numeric(unique((GL2_LC |> dplyr::select(DrainageArea_km2)))), as.numeric(unique((GL2_waterbody |> filter(Site=='GL2_LAKE') |> dplyr::select(AreaSqKm))))[1]))

GL2_landuse <- GL2_LC |>
  dplyr::select(WS_Group, Layer_1, LandCoverArea_km2) |>
  distinct() |>
  rbind(GL2.tmp)



ALB_flow <- readRDS('Data/Spatial_Data/ALB_flowline.RDS') |>
  mutate(WS_Group = 'ALB') |>
  dplyr::select(WS_Group, FType, ReachCode, LengthKM, GNIS_Name, Permanent_Identifier, WBArea_Permanent_Identifier, Shape_Length, NHDPlusID, Shape)

ALB_waterbody <- readRDS('Data/Spatial_Data/ALB_waterbody.RDS') |>
  mutate(WS_Group = 'ALB') |>
  mutate(Site = ifelse(ReachCode == '10190005001170', 'GL5_LAKE', 
                       ifelse(ReachCode== '10190005001171', 'GL4_LAKE',
                              ifelse(ReachCode =='10190005001172', 'GL3_LAKE', 
                                     ifelse(ReachCode == '10190005001175', 'GL2_LAKE',
                                            ifelse(ReachCode=='10190005001176', 'GL1_LAKE',
                                                   ifelse(ReachCode =='10190005001181', 'ALB_LAKE', NA))))))) |>
  dplyr::select(WS_Group, Site, FType, ReachCode, AreaSqKm, GNIS_Name, Permanent_Identifier, Shape_Length, NHDPlusID, Shape) 

ALB_waterFeatures <- bind_rows(ALB_flow, ALB_waterbody)
write_rds(ALB_waterFeatures, 'Data/Spatial_Data/ALB_waterFeatures.RDS')

ALB_LC <- readRDS('Data/Spatial_Data/ALB_LC.RDS') |>
  mutate(WS_Group = 'ALB') 

ggplot() +
  geom_raster(data = ALB_LC, aes(x = x, y=y, fill= Layer_1)) +
  scale_fill_viridis_d()+
  coord_quickmap()

# this is a studpid manual workaround :/
ALB.tmp <-data.frame(WS_Group=c('ALB', 'ALB'), Layer_1=c('Drainage Area', 'Lake Area'), LandCoverArea_km2=c(as.numeric(unique((ALB_LC |> dplyr::select(DrainageArea_km2)))), as.numeric(unique((ALB_waterbody |> filter(Site=='ALB_LAKE') |> dplyr::select(AreaSqKm))))[1]))

ALB_landuse <- ALB_LC |>
  dplyr::select(WS_Group, Layer_1, LandCoverArea_km2) |>
  distinct() |>
  rbind(ALB.tmp)





greenlakes_LC <- bind_rows(GL5_landuse, GL4_landuse, GL3_landuse, GL2_landuse, ALB_landuse)

write_rds(greenlakes_LC, 'Data/Spatial_Data/greenlakes_landcover.RDS')









