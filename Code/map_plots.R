#::::::::::::::::::::::::
# Map style plots 
#::::::::::::::::::::::::


sites <- read.csv("Data/sites.csv") |>
  drop_na(long)
# gl_network <- read.csv("Data/greenlakes_network.csv") |>
#   left_join(sites) |>
#   mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
#   mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
#   mutate(network_position = as.factor(network_position))


#plots <- st_read('Data/lter_plots_09_05_2022/lter_plots.shp')

# # Background maps 
esri_land <- paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
#basemap <- paste0('https://tiles.wmflabs.org/osm-no-labels/${z}/${x}/${y}.png')
topo_map  <- paste0('https://services.arcgisonline.com/arcgis/rest/services/World_Terrain_Base/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')
stamen <- paste0('https://stamen-tiles.a.ssl.fastly.net/terrain/${z}/${x}/${y}.jpg')


sites.sf = st_as_sf(sites, coords = c("long", "lat"), 
                      crs = 4326)


ggplot(sites.sf |> filter(site != 'FLUME')) +
  annotation_map_tile(type = stamen, zoom = 12) +
  geom_sf()





bbox <- sf::st_bbox(c(xmin = -105.644739, ymin = 40.040162, xmax = -105.588581, ymax = 40.058095),
                    crs = "+proj=longlat +datum=WGS84 +no_defs")
plot_nhdplus(bbox = bbox)

# doesn't work
# start_point <- sf::st_as_sf(data.frame(x = -105.644739, y = 40.040162), 
#                             coords = c("x", "y"), crs = 4326)
# plot_nhdplus(start_point)


# library(streamstats)
# library(leaflet)
# wsALBCAMP <- delineateWatershed(xlocation = -105.5923, ylocation = 40.04287, crs = 4326, includeparameters = 'true', includeflowtypes = 'true')



stamen <- leaflet() |> addProviderTiles(providers$Stamen.TopOSMFeatures) 

mymap <- get_map(location=c(-105.644739, 40.040162,-105.588581,40.058095),
        source='stamen', maptype = 'terrain', crop = FALSE) 
ggmap(mymap)



