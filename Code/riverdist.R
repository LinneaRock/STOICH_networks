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





library(sfnetworks)
net <- as_sfnetwork(flowlines |> filter(!is.na(GNIS_ID)),directed=TRUE)
plot(net)

# Find the nearest node on the network for each point
from_node <- st_nearest_feature((sites |> filter(site=='ALB_INLET')), net)
to_node <- st_nearest_feature((sites |> filter(site=='ALB_OUTLET')), net)


path <- st_network_paths(net, from=from_node, to=to_node)
library(units)
path <- path |>
  select(node_paths) |>
  unlist()
distance <- sum(path)/1000 

ggplot() +
  geom_sf(flowlines |> filter(!is.na(GNIS_ID)), mapping=aes(color=ReachCode))

