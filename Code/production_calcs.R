#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to calculate production and consumption along the network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


source('Data/CALL_DATA_PACKAGES.R') 



## subset, format datasets, calculate production/consumption ####
## Note these are done in a lot of steps to allow examination at each step 
## In each, check the group_by right before the final summarise to understand what we are calculating



## nutrients ####

### Lakes - 3 lakes have the data for this ####
nuts_prod_lakes <- nuts |>
  group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  mutate(GL5 = GL5_OUTLET - GL5_INLET,
         GL4 = GL4_OUTLET - GL4_INLET,
         GL3 = GL3_OUTLET - GL3_INLET,
         GL2 = ALB_INLET - GL3_OUTLET,
         ALBION = ALB_OUTLET - ALB_INLET) |>
  ungroup() |>
  select(2:4, 19:23) |>
  pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result) |>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream, param, significance) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in lakes
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 
  


### streams - 4 reaches have the data for this ####
nuts_prod_streams <- nuts |>
  group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  # mutate(reach1 = GL5_LAKE - ARIKAREE,
  #        reach2 = GL4_LAKE - GL5_LAKE,
  #        reach3 = GL3_LAKE - GL4_LAKE,
  #        reach4 = GL2_LAKE - GL3_LAKE,
  #        reach5 = ALB_LAKE - GL2_LAKE,
  #        reach5a = ALB_LAKE - GL1_LAKE,
  #        reach6 = ALB_CAMP - ALB_LAKE) |>
  mutate(#reach1 = GL5_INLET - ARIKAREE,
         reach1 = GL4_INLET - GL5_OUTLET,
         reach2 = GL3_INLET - GL4_OUTLET,
         reach3 = GL2_LAKE - GL3_OUTLET,
         reach4 = ALB_INLET - GL2_LAKE,
         reach4a = ALB_LAKE - GL1_LAKE) |>
         #reach6 = ALB_CAMP - ALB_OUTLET) |>
  ungroup() |>
  select(2:4, 19:23) |>
  pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result)|>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream, param, significance) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in streams
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 






## Stoichiometry ####

### Lakes ####
stoich_prod_lakes <- stoich |>
  group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  mutate(GL5 = GL5_OUTLET - GL5_INLET,
         GL4 = GL4_OUTLET - GL4_INLET,
         GL3 = GL3_OUTLET - GL3_INLET,
         GL2 = ALB_INLET - GL3_OUTLET,
         ALBION = ALB_OUTLET - ALB_INLET) |>
  ungroup() |>
  select(2:4, 19:23) |>
  pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result) |>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream, param, significance) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in lakes
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 





### Streams ####
stoich_prod_streams <- stoich |>
group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  # mutate(reach1 = GL5_LAKE - ARIKAREE,
  #        reach2 = GL4_LAKE - GL5_LAKE,
  #        reach3 = GL3_LAKE - GL4_LAKE,
  #        reach4 = GL2_LAKE - GL3_LAKE,
  #        reach5 = ALB_LAKE - GL2_LAKE,
  #        reach5a = ALB_LAKE - GL1_LAKE,
  #        reach6 = ALB_CAMP - ALB_LAKE) |>
  mutate(#reach1 = GL5_INLET - ARIKAREE,
    reach1 = GL4_INLET - GL5_OUTLET,
    reach2 = GL3_INLET - GL4_OUTLET,
    reach3 = GL2_LAKE - GL3_OUTLET,
    reach4 = ALB_INLET - GL2_LAKE,
    reach4a = ALB_LAKE - GL1_LAKE) |>
  #reach6 = ALB_CAMP - ALB_OUTLET) |>
  ungroup() |>
  select(2:4, 19:23) |>
  pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result)|>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream, param, significance) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in streams
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 


# visualising source/sink information ####
## for SFS presentation ####
# all_prod <- rbind(nuts_prod_lakes, nuts_prod_streams) |>
#   filter(param %in% c('IN_umolL', 'IP_umolL','in.ip', 'TN_umolL','TP_umolL', 'tn.tp')) |>
#   mutate(lake_stream = gsub('ALBION', 'ALB', lake_stream))

library(sf)
library(raster)
world_grey <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

 # topo <- paste0('https://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/2/tile/${z}/${y}/${x}.jpeg')

waterfeatures <- st_transform(readRDS('Data/Spatial_Data/ALB_waterFeatures.RDS', crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")))

library(mapview)
mapview(waterfeatures)

# prod_map <- waterfeatures |>
#   mutate(Site=ifelse(ReachCode=='10190005000121', 'reach1',
#                      ifelse(ReachCode%in%c('10190005000119', '10190005008027'), 'reach2',
#                             ifelse(ReachCode=='10190005000117', 'reach3', 
#                                    ifelse(ReachCode=='10190005000115', 'reach4',
#                                           ifelse(ReachCode=='10190005006165', 'reach4a', Site)))))) |>
#   left_join(all_prod, by=c('Site' = 'lake_stream'))

ave_values <- rbind(stats_nuts,stats_stoich) |>
  left_join(sites) 

ave_values <- st_as_sf(ave_values)


in_m <- ggplot() +
 # annotation_map_tile(type = world_grey, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(waterfeatures, mapping=aes()) +
  geom_sf(ave_values |> filter(param=='IN_umolL',
                               season=='All data'), mapping=aes(size=mean), fill='goldenrod',color='white',pch=21) +
  scale_size_continuous('IN concentration') + dark_theme_bw()

ip_m <- ggplot() +
 # annotation_map_tile(type = world_grey, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(waterfeatures, mapping=aes()) +
  geom_sf(ave_values |> filter(param=='IP_umolL',
                               season=='All data'), mapping=aes(size=mean), fill='goldenrod',color='white',pch=21) +
  scale_size_continuous('IP concentration')  + dark_theme_bw()

in.ip_m <- ggplot() +
  #annotation_map_tile(type = world_grey, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(waterfeatures, mapping=aes()) +
  geom_sf(ave_values |> filter(param=='in.ip',
                               season=='All data'), mapping=aes(size=mean), fill='goldenrod',color='white',pch=21) +
  scale_size_continuous('IN:IP') + dark_theme_bw()

(in_m/ip_m/in.ip_m) 
ggsave('Figures/DarkTheme/inorganic_map.png', width=6, height=8, units='in',dpi=1200)



tn_m <- ggplot() +
 # annotation_map_tile(type = world_grey, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(waterfeatures, mapping=aes()) +
  geom_sf(ave_values |> filter(param=='TN_umolL',
                               season=='All data'), mapping=aes(size=mean), fill='goldenrod',color='white',pch=21) +
  scale_size_continuous('TN concentration')+
  theme(legend.position='bottom') + dark_theme_bw()

tp_m <- ggplot() +
 # annotation_map_tile(type = world_grey, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(waterfeatures, mapping=aes()) +
  geom_sf(ave_values |> filter(param=='TP_umolL',
                               season=='All data'), mapping=aes(size=mean), fill='goldenrod',color='white',pch=21) +
  scale_size_continuous('TP concentration')+
  theme(legend.position='bottom') + dark_theme_bw()

tn.tp_m <- ggplot() +
  #annotation_map_tile(type = world_grey, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(waterfeatures, mapping=aes()) +
  geom_sf(ave_values |> filter(param=='tn.tp',
                               season=='All data'), mapping=aes(size=mean), fill='goldenrod',color='white',pch=21) +
  scale_size_continuous('TN:TP') +
  theme(legend.position='bottom') + dark_theme_bw()

(tn_m/tp_m/tn.tp_m) 
ggsave('Figures/DarkTheme/total_map.png', width=6, height=8, units='in',dpi=1200)


invert_geom_defaults()














# ## Ions
# 
# ### Lakes 
# ions_prod_lakes <- ions |>
#   group_by(site, eco_type, date, season, param) |>
#   summarise(result = mean(result)) |>
#   ungroup() |>
#   pivot_wider(names_from = site, values_from = result) |>
#   group_by(eco_type, date, season, param) |>
#   mutate(GL5 = GL5_OUTLET - GL5_INLET,
#          GL4 = GL4_OUTLET - GL4_INLET,
#          GL3 = GL3_OUTLET - GL3_INLET,
#          GL2 = ALB_INLET - GL3_OUTLET,
#          ALBION = ALB_OUTLET - ALB_INLET) |>
#   ungroup() |>
#   select(2:4, 21:25) |>
#   pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
#   drop_na(result) |>
#   group_by(param) |>
#   # using two-tailed wilcoxon test to test if productivity is different from zero 
#   mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
#          significance = ifelse(p.value > 0.05, '-',
#                                ifelse(between(p.value, 0.01, 0.05), '<0.05',
#                                       ifelse(between(p.value, 0.001, 0.01), '<0.01',
#                                              ifelse(p.value <= 0.001, '<0.001', significance))))) |>
#   ungroup() |>
#   group_by(param, significance) |>
#   summarise(mean = mean(result),
#             median = median(result),
#             min = min(result),
#             max = max(result),
#             SD = sd(result),
#             n = n()) |>
#   ungroup() 
# 
# 
# 
# 
# 
# ### Streams 
# ions_prod_streams <- ions |>
#   pivot_wider(names_from = site, values_from = result) |>
#   group_by(eco_type, date, season, param) |>
#   # mutate(reach1 = GL5_LAKE - ARIKAREE,
#   #        reach2 = GL4_LAKE - GL5_LAKE,
#   #        reach3 = GL3_LAKE - GL4_LAKE,
#   #        reach4 = GL2_LAKE - GL3_LAKE,
#   #        reach5 = ALB_LAKE - GL2_LAKE,
#   #        reach5a = ALB_LAKE - GL1_LAKE,
#   #        reach6 = ALB_CAMP - ALB_LAKE) |>
#   mutate(reach1 = GL5_INLET - ARIKAREE,
#          reach2 = GL4_INLET - GL5_OUTLET,
#          reach3 = GL3_INLET - GL4_OUTLET,
#          reach4 = GL2_LAKE - GL3_OUTLET,
#          reach5 = ALB_INLET - GL2_LAKE,
#          reach5a = ALB_LAKE - GL1_LAKE,
#          reach6 = ALB_CAMP - ALB_OUTLET) |>
#   ungroup() |>
#   select(2:4, 21:27) |>
#   pivot_longer(4:10, names_to = 'lake_stream', values_to = 'result') |>
#   drop_na(result)|>
#   group_by(param) |>
#   # using two-tailed wilcoxon test to test if productivity is different from zero 
#   mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
#          significance = ifelse(p.value > 0.05, '-',
#                                ifelse(between(p.value, 0.01, 0.05), '<0.05',
#                                       ifelse(between(p.value, 0.001, 0.01), '<0.01',
#                                              ifelse(p.value <= 0.001, '<0.001', significance))))) |>
#   ungroup() |>
#   group_by(param, significance) |>
#   summarise(mean = mean(result),
#             median = median(result),
#             min = min(result),
#             max = max(result),
#             SD = sd(result),
#             n = n()) |>
#   ungroup() 
