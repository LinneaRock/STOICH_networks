#::::::::::::::::::::::::
# Map style plots 
#::::::::::::::::::::::::


library(tidyverse)
library(sf)
library(ggspatial)

sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  mutate(network_position = as.factor(network_position))


plots <- st_read('Data/lter_plots_09_05_2022/lter_plots.shp')

# # Background maps 
# esri_land <- paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
# basemap <- paste0('https://tiles.wmflabs.org/osm-no-labels/${z}/${x}/${y}.png')
# 
# ggplot(plots) +
#   annotation_map_tile(type = esri_land, zoom = 12) +
#   geom_sf(aes(fill = PROJECT))



remotes::install_github("DOI-USGS/nhdplusTools")
