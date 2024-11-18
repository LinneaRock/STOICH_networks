
source('Data/CALL_DATA_PACKAGES.R')

library(lme4)


nuts_dat <- nuts |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  left_join(distances_Km |>
              filter(site1=='Arikaree_GLACIER') |>
              select(-site1) |>
              rename(site=site2,
                     distancefromglacier_Km=distance_Km)
  ) |>
  mutate(distancefromglacier_Km = ifelse(is.na(distancefromglacier_Km), 0, distancefromglacier_Km)) |>
  distinct() |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  mutate(nutrient = case_when(grepl('N', param)~'Nitrogen',
                              grepl('P', param)~'Phosphorus')) |>
  mutate(distancefromglacier_Km=as.numeric(distancefromglacier_Km))


stoich_dat <- stoich |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  left_join(distances_Km |>
              filter(site1=='Arikaree_GLACIER') |>
              select(-site1) |>
              rename(site=site2,
                     distancefromglacier_Km=distance_Km)
  ) |>
  mutate(distancefromglacier_Km = ifelse(is.na(distancefromglacier_Km), 0, distancefromglacier_Km)) |>
  distinct() |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  mutate(distancefromglacier_Km=as.numeric(distancefromglacier_Km))

ggplot(nuts_dat, aes(distancefromglacier_Km, result, color=param, group=param)) +
  geom_point(aes(shape=eco_type)) +
  geom_smooth(method='gam') +
  facet_wrap(~nutrient*season, scales='free')

ggplot(stoich_dat, aes(distancefromglacier_Km, result, color=param, group=param)) +
  geom_point(aes(shape=eco_type)) +
  geom_smooth(method='gam')


ggplot(nuts_dat, aes(network_position, result, color=param, group=param)) +
  geom_point() +
  geom_smooth(method='gam') +
  facet_wrap(~nutrient, scales='free')

ggplot(nuts_dat, aes(distancefromglacier_Km, result, color=param)) +
  geom_point(aes(shape=eco_type)) +
  geom_smooth(method='lm') +
  facet_wrap(~nutrient, scales='free')





don.m <- lmer(result~distancefromglacier_Km*upstream_network_lakes  + (1|season),  nuts_dat |> filter(param=='DON_umolL'))
summary(don.m)


