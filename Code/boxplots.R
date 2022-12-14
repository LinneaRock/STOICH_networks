#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to make parameter boxplots 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Call in data and libraries ####
library(tidyverse)


sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))

# subset and format datasets for plotting ####
ions <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, 21:31) |>
  pivot_longer(7:17, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)

nuts <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, 2:7, 10:14) |>
  pivot_longer(7:17, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)








# Plotting by eco type ####
## plot non-nutrients by eco type ####
ggplot(ions, aes(eco_type, result)) +
  geom_boxplot() +
  geom_jitter(aes(color = network_position, shape = depth_m), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_c('Network\nposition',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '')

ggsave('Figures/boxplots/eco_params.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)
  

## plot nutrients by eco type ####
ggplot(nuts, aes(eco_type, result)) +
  geom_boxplot() +
  geom_jitter(aes(color = network_position, shape = depth_m), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_c('Network\nposition',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '')

ggsave('Figures/boxplots/eco_nuts.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)








# plotting by network position ####
## plot non-nutrients by network position ####
ggplot(ions, aes(network_position, result, group = network_position)) +
  geom_boxplot() +
  geom_jitter(aes(color = eco_type, shape = depth_m), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = 'Network position', y = '')

ggsave('Figures/boxplots/position_params.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)

## plot nutrients by network position ####
ggplot(nuts, aes(network_position, result, group = network_position)) +
  geom_boxplot() +
  geom_jitter(aes(color = eco_type, shape = depth_m), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = 'Network position', y = '')

ggsave('Figures/boxplots/position_nuts.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)







# plotting by season ####
## plot non-nutrients by season ####
ggplot(ions, aes(season, result, group = season)) +
  geom_boxplot() +
  geom_jitter(aes(color = eco_type, shape = depth_m), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black'),
        axis.text.x = element_text(angle=45, vjust=1,hjust=1)) +
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '')

ggsave('Figures/boxplots/season_params.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)

## plot nutrients by season ####
ggplot(nuts, aes(season, result, group = season)) +
  geom_boxplot() +
  geom_jitter(aes(color = eco_type, shape = depth_m), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black'),
        axis.text.x = element_text(angle=45, vjust=1,hjust=1)) +
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '')

ggsave('Figures/boxplots/season_nuts.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)
