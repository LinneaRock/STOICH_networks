#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to calculate production and consumption along the network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Call in data and libraries ####
library(tidyverse)

sites <- read.csv("Data/sites.csv") |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', '7','8','9','10','11','12','12a','13','14','15','16')))
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))


# subset, format datasets, calculate production/consumption ####

## nutrients ####
nuts <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, lat, long, 2:7, 10:14) |>
  pivot_longer(9:19, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)  |> # get rid of weird sites with no locational information
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m),  # just look at photic zone
         site != 'FLUME') |>
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
  select(1:4, 21:25) |>
  pivot_longer(5:9, names_to = 'Lake', values_to = 'result') |>
  drop_na(result)
  



ions <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, lat, long, 21:31) |>
  pivot_longer(9:19, names_to = 'param', values_to = 'result') |>
  drop_na(network_position) |> # get rid of weird sites with no locational information
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m),   # just look at photic zone
         site != 'FLUME')


stoich <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, lat, long, 3:7, 10:14) |>
  mutate(tn.tp = TN_umolL/TP_umolL,
         don.dop = DON_umolL/DOP_umolL,
         tdn.tdp = TDN_umolL/TDP_umolL,
         pn.pp = PN_umolL/PP_umolL,
         in.ip = IN_umolL/IP_umolL) |>
  select(1:8, 19:23) |>
  pivot_longer(9:13, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)  |> # get rid of weird sites with no locational information
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m),  # just look at photic zone
         site != 'FLUME',
         is.finite(result))
