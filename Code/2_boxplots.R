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

# count of each variable in the dataset
count <- data |>
  group_by(param, site) |>
  summarise(n=n())
  



# 2. Boxplot comparing nutrient concentrations and stoichiometry across positions ####
#making pretty labels
data$param <- factor(data$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')')))

library(colorblindr)
ggplot(data) +
  #geom_jitter(aes(factor(network_position), result, color=eco_type), shape=21,alpha=0.01) +
  geom_boxplot(aes(x = factor(network_position), y = result, group=site, color=eco_type)) +
  scale_color_viridis_d('') +
  theme_bw() +
  labs(x = "Network position", y = "") +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  theme_classic()

ggsave('Figures/boxplots.png',height = 6.5, width = 8.5, units = 'in', dpi = 1200)
