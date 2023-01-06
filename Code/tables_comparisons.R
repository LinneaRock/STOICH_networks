#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to create tables and look at differences between lakes, streams in network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Call in data and libraries ####
library(tidyverse)
library(colorblindr)

sites <- read.csv("Data/sites.csv") |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', '7','8','9','10','11','12','12a','13','14','15','16')))
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





# get basic stats for each site, parameter ####
get_stats <- function(data) {
  
  tmp <- data |>
    group_by(site, eco_type, network_position, param, season) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SD = sd(result),
              n = n()) |>
    ungroup() |>
    mutate(CV = (SD/mean) * 100) 
  
  tmp2 <- data |>
    group_by(site, eco_type, network_position, param) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SD = sd(result),
              n = n()) |>
    ungroup() |>
    mutate(CV = (SD/mean) * 100,
           season = 'All data') 
  
  df <- rbind(tmp, tmp2)
  
  rm(tmp)
  rm(tmp2)
  return(df)
  
}

stats_ions <- get_stats(ions)
stats_nuts <- get_stats(nuts)
stats_stoich <- get_stats(stoich)


# plotting by network position and season ####
plot_season <- function(data) {
  ggplot(data |> filter(season != 'All data')) +
    geom_line(aes(network_position, mean, group = season, color = season), alpha = 0.5, linewidth = 1) +
    geom_point(aes(network_position, mean, group = season, fill = season), 
               color = "black", pch = 21, size = 2, position=position_dodge(width=0.1)) +
    geom_errorbar(aes(network_position, mean, ymin = mean-SD, ymax = mean+SD, color = season),
                  position=position_dodge(width=0.1), linetype = 'dashed')  +
    scale_color_OkabeIto() +
    scale_fill_OkabeIto() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    facet_wrap(~param, ncol=3, scales = 'free_y')
}


plot_season(stats_nuts)
ggsave("Figures/network_nuts_ts_seasonal.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_season(stats_ions)
ggsave("Figures/network_ions_ts_seasonal.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_season(stats_stoich) +
  ylim(0,NA)
ggsave("Figures/network_stoich_ts_seasonal.png", width = 10.5, height = 8.5, units = "in", dpi = 500)



# plotting by network position all data ####
plot_alldat <- function(data) {
  ggplot(data |> filter(season == 'All data')) +
    geom_point(aes(network_position, mean), 
               color = "black", pch = 21, size = 2) +
    geom_errorbar(aes(network_position, mean, ymin = mean-SD, ymax = mean+SD), linetype = 'dashed')  +
    geom_line(aes(network_position, mean, group = 1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    facet_wrap(~param, ncol=3, scales = 'free_y')
}

plot_alldat(stats_nuts)
ggsave("Figures/network_nuts_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_alldat(stats_ions)
ggsave("Figures/network_ions_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_alldat(stats_stoich) +
  ylim(0,NA)
ggsave("Figures/network_stoich_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)



# compare lakes vs rivers vs glacier for all parameters 
