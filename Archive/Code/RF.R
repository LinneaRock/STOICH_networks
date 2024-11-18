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
  left_join(greenlakes_LC |>
              pivot_wider(names_from = Layer_1, values_from = LandCoverArea_km2) |>
              mutate(`Evergreen Forest` = ifelse(is.na(`Evergreen Forest`), 0, `Evergreen Forest`),
                     `Woody Wetlands` = ifelse(is.na(`Woody Wetlands`), 0, `Woody Wetlands`))) 



