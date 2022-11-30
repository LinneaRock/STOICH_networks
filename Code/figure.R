library(tidyverse)
library(lubridate)

sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  select(-X) |>
  left_join(sites)


subset <- gl_network |>
  select(3:17, network_position, eco_type, - NO3_ueqL, -NH4_ueqL, -PO4_ueqL) |>
  mutate(mon = month(date), yr = year(date)) |> #and add seasons to the dataframe
  mutate(season = case_when(mon %in% c(10,11,12) ~ "Oct-Dec",
                            mon %in% c(1,2,3) ~ "Jan-Mar",
                            mon %in% c(4,5,6)  ~ "Apr-Jun",
                            mon %in% c(7,8,9) ~ "Jul-Sep")) |>
  pivot_longer(1:10, names_to = 'parameter', values_to = 'concentration_umol_L') |>
  drop_na() |>
  mutate(parameter = sub('\\_.*', "", parameter)) |>
  group_by(season, yr, site, network_position, eco_type, parameter) |>
  summarise(meanconc_umolL = mean(concentration_umol_L)) |>
  ungroup() |>
  mutate(parameter = factor(parameter, levels = c("TN", "TDN", "IN", "DON", "PN", "TP", "TDP", "IP", "DOP", "PP")))
         

ggplot(subset, aes(yr, meanconc_umolL, color = network_position, group = site)) +
  geom_path(aes(group = season)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_y",ncol = 3) +
  theme_bw()
  




forms_subset <- gl_network |>
  select(3:17, network_position, eco_type) |>
  drop_na()
