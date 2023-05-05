#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# River distances between points
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Data/CALL_DATA_PACKAGES.R') 
library(riverdist)
library(sf)

nhd <- st_read('C:/Users/lrock1/OneDrive - University of Wyoming/Spatial_Data/Niwot/NHDPlus/NHDPLUS_H_1019_HU4_GDB.gdb', layer = 'NHDFlowline')

flowlines <- read_rds('Data/Spatial_Data/ALB_flowline.RDS')
plot(flowlines)

sites <- read.csv('Data/sites.csv') |>
  drop_na(lat) |>
  st_as_sf(coords=c('long','lat'),crs=4269)
class(sites) # sf, df
crs(sites) # NAD83

line2network(nhd)
