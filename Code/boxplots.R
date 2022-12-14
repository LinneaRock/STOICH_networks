#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to make parameter boxplots 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Call in data and libraries ####
library(tidyverse)


sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  select(-X) |>
  left_join(sites) |>
  mutate(date = as.Date(date))


# write plot function ####
boxplot_fun <- function(x,y,xlab,ylab,name) {
  # first subset the data
  data <- data.frame(x,y, gl_network$network_position) |>
    drop_na() 
  
  colnames(data) <- c('xx','yy','network_position')
  
  ggplot(data, aes(xx, yy)) +
    geom_boxplot() +
    geom_jitter(aes(color = network_position), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(face='bold', family='serif',
                                    size=rel(1.2), hjust=0.5),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_blank(),
          text=element_text(family='serif'),
          axis.text=element_text(color='black')) +
    scale_color_viridis_c('Network\nposition',direction = -1) +
    labs(x = xlab, y = ylab)
  
  ggsave(paste0('Figures/boxplots/',name,'.png'), height = 4.5, width = 6.5, units = 'in', dpi = 1200)
}

# plot non-nutrients by eco type ####
ions <- gl_network |>
  select(site, network_position, eco_type, date, depth_m, 21:31) |>
  pivot_longer(6:16, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)

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

ggsave('Figures/boxplots/eco_params.png', height = 4.5, width = 6.5, units = 'in', dpi = 1200)
  

# Nitrogen by eco type ####
## TN ####
boxplot_fun(gl_network$eco_type, gl_network$TN_umolL, '', 'Total N '~mu*mol~L^-1, 'eco_TN')
## TDN ####
boxplot_fun(gl_network$eco_type, gl_network$TDN_umolL, '', 'Total dissolved N '~mu*mol~L^-1, 'eco_TDN')
## DON ####
boxplot_fun(gl_network$eco_type, gl_network$DON_umolL, '', 'Dissolved organic N '~mu*mol~L^-1, 'eco_DON')
## PN ####
boxplot_fun(gl_network$eco_type, gl_network$PN_umolL, '', 'Particulate N '~mu*mol~L^-1, 'eco_PN')
## IN ####
boxplot_fun(gl_network$eco_type, gl_network$IN_umolL, '', 'Inorganic N '~mu*mol~L^-1, 'eco_IN')



# Phosphorus by eco type ####
## TP ####
boxplot_fun(gl_network$eco_type, gl_network$TP_umolL, '', 'Total P '~mu*mol~L^-1, 'eco_TP')
## TDP ####
boxplot_fun(gl_network$eco_type, gl_network$TDP_umolL, '', 'Total dissolved P '~mu*mol~L^-1, 'eco_TDP')
## DOP ####
boxplot_fun(gl_network$eco_type, gl_network$DOP_umolL, '', 'Dissolved organic P '~mu*mol~L^-1, 'eco_DOP')
## PP ####
boxplot_fun(gl_network$eco_type, gl_network$PP_umolL, '', 'Particulate P '~mu*mol~L^-1, 'eco_PP')
## IP ####
boxplot_fun(gl_network$eco_type, gl_network$IP_umolL, '', 'Inorganic P '~mu*mol~L^-1, 'eco_IP')


