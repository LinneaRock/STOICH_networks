#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to create tables and look at differences between lakes, streams in network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Call in data and libraries ####
library(tidyverse)

sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))

# subset and format datasets for plotting ####
ions <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, lat, long, 21:31) |>
  pivot_longer(9:19, names_to = 'param', values_to = 'result') |>
  drop_na(network_position) |> # get rid of weird sites with no locational information
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m),   # just look at photic zone
         site != 'FLUME')

nuts <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, lat, long, 2:7, 10:14) |>
  pivot_longer(9:19, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)  |> # get rid of weird sites with no locational information
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m),  # just look at photic zone
         site != 'FLUME')

# get basic stats for each site, parameter ####
tmp <- ions |>
  group_by(site, eco_type, network_position, param, season) |>
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result)) |>
  ungroup() |>
  mutate(CV = (SD/mean) * 100) 

tmp2 <- ions |>
  group_by(site, eco_type, network_position, param) |>
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result)) |>
  ungroup() |>
  mutate(CV = (SD/mean) * 100,
         season = 'All data') 

stats_ions <- rbind(tmp, tmp2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

tmp <- nuts |>
  group_by(site, eco_type, network_position, param, season) |>
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result)) |>
  ungroup() |>
  mutate(CV = (SD/mean) * 100) 

tmp2 <- nuts |>
  group_by(site, eco_type, network_position, param) |>
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result)) |>
  ungroup() |>
  mutate(CV = (SD/mean) * 100,
         season = 'All data') 

stats_nuts <- rbind(tmp, tmp2)

rm(tmp)
rm(tmp2)
