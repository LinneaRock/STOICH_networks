#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to calculate source/sink potential along the network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# 1. call data and libraries ####
source('Data/CALL_DATA_PACKAGES.R') 
library(sf)
library(sfnetworks) # for centroids and network analyses
library(png)
library(grid)


# normalize source/sinks by distances between points. Unfortunately we don't have good estimates for volume or discharge here so distance is a proxy. 
# distances were calculated manually in ArcGIS
distances <- readxl::read_xlsx('Data/Results/site_distances_km.xlsx') |>
  pivot_longer(cols=c(2:16), names_to = 'site2', values_to = 'distance_km') |>
  drop_na() |>
  rename('site1'=1) |>
  filter(distance_km >0)

# read in gis data for plotting
waterfeatures <- st_transform(readRDS('Data/Spatial_Data/ALB_waterFeatures.RDS', st_crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")))

# 2. Calculating source/sink ####

## lakes ####
nuts_prod_lakes <- nuts |>
  group_by(site, date, season, param) |>
  summarise(averesult = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = averesult) |>
 # group_by(eco_type, date, season, param) |>
  mutate(GL5 = GL5_OUTLET - GL5_INLET,
         GL4 = GL4_OUTLET - GL4_INLET,
         GL3 = GL3_OUTLET - GL3_INLET,
         GL2 = ALB_INLET - GL3_OUTLET,
         ALBION = ALB_OUTLET - ALB_INLET) |>
 # ungroup() |>
  dplyr::select(1:3, 19:23) |>
  pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result)  |>
  # add in distance data - we only have GL5, GL4, and Albion here - all others did not have enough data
  left_join(distances |>
              mutate(lake_stream = case_when(site1=='GL5_INLET' & site2=='GL5_OUTLET'~'GL5',
                                             site1=='GL4_INLET' & site2=='GL4_OUTLET'~'GL4',
                                             site1=='ALB_INLET' & site2=='ALB_OUTLET'~'ALBION')) |>
              drop_na()) |>
  mutate(normalized_result = result/distance_km) |>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(normalized_result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream,season, param, significance,site1,site2) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in lakes
  summarise(mean = mean(normalized_result),
            median = median(normalized_result),
            min = min(normalized_result),
            max = max(normalized_result),
            SD = sd(normalized_result),
            n = n()) |>
  ungroup()


## streams ####
nuts_prod_streams <- nuts |>
  group_by(site, date, season, param) |>
  summarise(averesult = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = averesult) |>
 # group_by(eco_type, date, season, param) |>
  mutate(reach1 = GL5_INLET - ARIKAREE,
    reach2 = GL4_INLET - GL5_OUTLET,
    reach3 = GL3_INLET - GL4_OUTLET,
    reach4 = GL2_LAKE - GL3_OUTLET,
    reach5 = ALB_INLET - GL2_LAKE,
    reach5a = ALB_LAKE - GL1_LAKE) |>
  #reach6 = ALB_CAMP - ALB_OUTLET) |>
  ungroup() |>
  dplyr::select(1:3, 19:24) |>
  pivot_longer(4:9, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result) |>
  # add in distance data - we only have reach1, reach2, reach3 and Reach5a here - all others did not have enough data
  left_join(distances |>
              mutate(lake_stream = case_when(site1=='Arikaree_GLACIER' & site2=='GL5_INLET'~'reach1',
                                             site1=='GL5_OUTLET' & site2=='GL4_INLET'~'reach2',
                                             site1=='GL4_OUTLET' & site2=='GL3_INLET'~'reach3',
                                             site1=='GL1_LAKE' & site2=='ALB_LAKE'~'reach5a')) |>
              drop_na()) |>
  mutate(normalized_result = result/distance_km) |>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(normalized_result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream, param, season, significance,site1,site2) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in streams
  summarise(mean = mean(normalized_result),
            median = median(normalized_result),
            min = min(normalized_result),
            max = max(normalized_result),
            SD = sd(normalized_result),
            n = n()) |>
  ungroup() 


# 3. Plotting ####
## get median values for plots ####
medians <- nuts |>
  group_by(site, param) |>
  summarise(med = median(result)) |>
  ungroup() |>
  left_join(sites |>
              dplyr::select(site,geometry)) |>
  st_as_sf() |>
  mutate(param = sub('_.*','',param))|>
  mutate(pointcolor = ifelse(str_detect(param,'N'),'red4','#336a98'))

## get center of reaches ####
flowlines <- read_rds('Data/Spatial_Data/ALB_flowline.RDS')
ggplot() +
  geom_sf(flowlines |> filter(!is.na(GNIS_ID)), mapping=aes(color=ReachCode))

net <- as_sfnetwork(flowlines,directed=TRUE)


centroids <- net |>
  activate('edges') |>
  st_as_sf() |>
  st_centroid()


ggplot() +
  geom_sf(data = waterfeatures, mapping=aes()) +
  geom_sf(data = flowlines |> filter(!is.na(GNIS_ID)), mapping = aes()) +
  geom_sf(data = centroids|> filter(from==25), mapping=aes(color=as.character(from)), size = 3, shape = 21)

# from=25 is center reach1 = GL5_INLET - Arikaree
# from=50 is center reach2 = GL4_INLET - GL5_OUTLET,
# from=40 reach3 = GL3_INLET - GL4_OUTLET,
# from=46 reach5a = ALB_LAKE - GL1_LAKE

reach_centroids <- centroids |>
  filter(from %in% c(25, 50, 40, 46)) |>
  mutate(lake_stream=case_when(from==25~'reach1',
                               from==50~'reach2',
                               from==40~'reach3',
                               from==46~'reach5a')) |>
  dplyr::select(lake_stream, Shape) |>
  rename(geometry=Shape)


## source/sink dataframe ####
sigSourceSink <- nuts_prod_lakes |> 
  mutate(site=case_when(lake_stream=='ALBION'~'ALB_LAKE',
                        lake_stream=='GL5'~'GL5_LAKE',
                        lake_stream=='GL4'~'GL4_LAKE')) |>
  left_join(sites |> 
              dplyr::select(site, geometry)) |>
  bind_rows(nuts_prod_streams |>
              left_join(reach_centroids)) |>
  filter(significance != '-') |>
  mutate(param = sub('_.*','',param))|>
  mutate(img = ifelse(mean<0, '\U2193', '\U2191')) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  mutate(seasoncol=case_when(season=='Winter'~'blue4',
                              season=='Snowmelt runoff'~'palegreen4',
                              season=='Summer'~'goldenrod3')) |>
  st_as_sf() 

data_with_coords <- st_coordinates(sigSourceSink) |>
  as.data.frame() |>
  bind_cols(sigSourceSink)

## Create individual plots for each parameter ####
plot_list <- lapply(unique(medians$param), function(param_val) {
  ggplot() +
    geom_sf(data = waterfeatures, aes(),color='grey') +
    geom_sf(data = subset(medians, param == param_val), aes(size = med, fill=pointcolor),alpha=0.25, shape=21) +
    scale_fill_identity()+
    ggtitle(as.expression(param_val)) +
   # geom_sf(subset(sigSourceSink, param==param_val), mapping=aes(shape=img,color=seasoncol),size=8) +
    geom_jitter(subset(data_with_coords, param==param_val), mapping=aes(x=X, y=Y,shape=img,color=seasoncol), size=8, width=0.002) +
    scale_shape_identity() +
    scale_color_identity() +
    labs(x='',y='') +
    theme_bw() +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.key.size = unit(0.4, "cm"),
          legend.spacing.y = unit(0.2, "cm")) +
    labs(size = expression(mu*mol*L^-1)) 
})

## Combine all plots ####
combined_plot <- patchwork::wrap_plots(plot_list, ncol = 2)
combined_plot

# save plot ####
ggsave('Figures/sourceSink_significants.png', height = 8.5, width = 6.5, units = 'in', dpi = 1200)

# save data for results writing ####
write.csv(sigSourceSink, 'Data/Results/SourceSink.csv')



