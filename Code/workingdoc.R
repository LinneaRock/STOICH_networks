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
  left_join(greenlakes_LC)



# Boxplot comparing nutrient concentrations and stoichiometry across positions ####
#making pretty labels
data$param <- factor(data$param, labels = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')')))

ggplot(data) +
  geom_boxplot(aes(x = factor(network_position), y = result)) +
  geom_jitter(aes(factor(network_position), result, color=eco_type), shape=21,alpha=0.5) +
  theme_minimal() +
  labs(x = "Network position", y = "") +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  theme_classic()
