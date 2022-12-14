library(tidyverse)
library(lubridate)

sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))


subset <- gl_network |>
  select(3:17, network_position, eco_type, year, season, - NO3_ueqL, -NH4_ueqL, -PO4_ueqL) |>
  pivot_longer(1:10, names_to = 'parameter', values_to = 'concentration_umol_L') |>
  drop_na() |>
  mutate(parameter = sub('\\_.*', "", parameter)) |>
  group_by(season, year, site, network_position, eco_type, parameter) |>
  summarise(meanconc_umolL = mean(concentration_umol_L)) |>
  ungroup() |>
  mutate(parameter = factor(parameter, levels = c("TN", "TDN", "IN", "DON", "PN", "TP", "TDP", "IP", "DOP", "PP")))


ggplot(subset, aes(year, meanconc_umolL, color = network_position, group = site)) +
  geom_path(aes(group = season)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_y",ncol = 3) +
  theme_bw()


inorganic <- gl_network |>
  select(site, network_position, date, season, eco_type, TP_umolL, IP_umolL, TN_umolL, IN_umolL) |>
  drop_na() |>
  mutate(percentIP = (IP_umolL/TP_umolL) * 100) |>
  mutate(percentIN = (IN_umolL/TN_umolL) * 100) |>
  filter(percentIN < 100,
         percentIP < 100) |>
  mutate(network_position=as.factor(network_position))  |>
  select(-TP_umolL, - TN_umolL, - IP_umolL, -IN_umolL) |>
  pivot_longer(6:7, names_to = 'param', values_to = 'percentoftot')

library(ggridges)

ggplot(inorganic, aes(percentoftot, network_position, fill = as.factor(param))) +
  geom_density_ridges(alpha = 0.5, scale=1.5, rel_min_height=0.02) +
  scale_fill_manual('',labels = c('%IN of TN', '%IP of TP'), values = c("red4", "#336a98")) +
  theme_bw() +
  labs(x='% inorganic of total nutrient', y='Network position') +
  facet_grid(.~season)
ggsave('Figures/densityplot2.png', height=4.5, width = 6.5, units='in', dpi=1200)


forms_subset <- gl_network |>
  select(3:17, network_position, eco_type) |>
  drop_na()
