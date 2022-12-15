#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to look at nutrients along the network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


library(tidyverse)
sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))

plot_log <- function(param, name) {
  ggplot(gl_network, aes(network_position, param)) +
    geom_point(aes(color = season)) +
    scale_y_log10() +
    geom_smooth() +
    labs(y = name)
  }

plot_log(gl_network$TN_umol, 'TN_umolL')
summary(lm(log10(TN_umolL)~network_position, gl_network |> filter(TN_umolL > 0 & !is.na(TN_umolL))))  

plot_log(gl_network$IN_umol, 'IN_umolL')
summary(lm(log10(IN_umolL)~network_position, gl_network |> filter(IN_umolL > 0 & !is.na(IN_umolL))))  


  
