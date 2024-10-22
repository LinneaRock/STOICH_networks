#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Nutrient distribution comparisons rain clouds
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# can't facet wrap this in a way that is meaningful. Beautiful plots though

# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R')

data <- rbind(nuts,stoich) |>
  select(-network_position) |>
  left_join(distances_Km |>
              filter(site1=='Arikaree_GLACIER') |>
              select(-site1) |>
              rename(site=site2,
                     distancefromglacier_Km=distance_Km)
  ) |>
  mutate(distancefromglacier_Km = ifelse(is.na(distancefromglacier_Km), 0, distancefromglacier_Km)) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  select(-depth_m,-elevation_m,-drainage_area_ha) |>
  left_join(greenlakes_LC) |>
  mutate(eco_type = ifelse(grepl('INLET', site), 'inlet', eco_type),
         eco_type = ifelse(grepl('OUTLET', site), 'outlet', eco_type)) |>
  filter(eco_type != 'glacier')


ggplot(data |> filter(param == 'TN_umolL'),
       aes(eco_type, result, fill=eco_type, group=eco_type)) +
  # add half-violin
  ggdist::stat_halfeye(
    # custom bandwidth
    adjust = 0.5,
    #move geom to right
    justification = -0.2,
    # remove slab interval
    .width = 0
  ) +
  geom_boxplot(
    width = 0.12,
    alpha=0.5
  ) +
  # add dot plots
  ggdist::stat_dots(
    # orientation to left
    side = 'left',
    # move geom to left
    justification = 1.1,
    # adjust binning of observations
    binwidth = NA,
    overflow = 'compress',
    alpha=0.25
  ) +
  # ggpubr::stat_compare_means(comparisons = list(c("inlet", "lake"), c("inlet", "outlet"), c("lake", "outlet")), label = "p.signif", fontface = "bold") +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x='', y='')  #+
 # facet_wrap(~param, scales='free', ncol=3)




