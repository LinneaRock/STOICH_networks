

source('Data/CALL_DATA_PACKAGES.R') 


subset <- nuts |>
  mutate(year = year(date)) |>
  group_by(season, year, site, network_position, eco_type, param) |>
  summarise(meanconc_umolL = mean(result)) |>
  ungroup() 


ggplot(subset, aes(year, meanconc_umolL, color = network_position, group = site)) +
  geom_path(aes(group = season)) +
  geom_point() +
  facet_wrap(~param, scales = "free_y",ncol = 3) +
  theme_bw()


inorganic <- nuts |>
  #select(site, network_position, date, season, eco_type, param) |>
  filter(param %in% c('TP_umolL', 'IP_umolL', 'TN_umolL', 'IN_umolL')) |>
  pivot_wider(id_cols = c(1:6), names_from = param, values_from = result) |>
  drop_na() |>
  mutate(percentIP = (IP_umolL/TP_umolL) * 100) |>
  mutate(percentIN = (IN_umolL/TN_umolL) * 100) |>
  filter(percentIN < 100,
         percentIP < 100) |>
 #mutate(network_position=as.factor(network_position))  |>
  select(-TP_umolL, - TN_umolL, - IP_umolL, -IN_umolL) |>
  pivot_longer(6:7, names_to = 'param', values_to = 'percentoftot')



ggplot(inorganic, aes(percentoftot, network_position, fill = as.factor(param))) +
  geom_density_ridges(alpha = 0.5, scale=1.5, rel_min_height=0.02) +
  scale_fill_manual('',labels = c('%IN of TN', '%IP of TP'), values = c("red4", "#336a98")) +
  theme_bw() +
  labs(x='% inorganic of total nutrient', y='Network position') +
  facet_grid(.~season)
ggsave('Figures/densityplot_season.png', height=4.5, width = 6.5, units='in', dpi=1200)



