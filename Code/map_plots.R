#::::::::::::::::::::::::
# Map style plots 
#::::::::::::::::::::::::


library(tidyverse)
library(sf)
library(ggspatial)

sites <- read.csv("Data/sites.csv") |>
  drop_na(long)
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  mutate(network_position = as.factor(network_position))


#plots <- st_read('Data/lter_plots_09_05_2022/lter_plots.shp')

# # Background maps 
esri_land <- paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
#basemap <- paste0('https://tiles.wmflabs.org/osm-no-labels/${z}/${x}/${y}.png')
topo_map  <- paste0('https://services.arcgisonline.com/arcgis/rest/services/World_Terrain_Base/MapServer/tile/${z}/${y}/${x}.jpeg')


sites.sf = st_as_sf(sites, coords = c("long", "lat"), 
                      crs = 4326)


ggplot(sites.sf |> filter(site != 'FLUME')) +
  annotation_map_tile(type = topo, zoom = 5) +
  geom_sf() 



library(nhdplusTools)


bbox <- sf::st_bbox(c(xmin = -105.644739, ymin = 40.040162, xmax = -105.588581, ymax = 40.058095),
                    crs = "+proj=longlat +datum=WGS84 +no_defs")
plot_nhdplus(bbox = bbox)
