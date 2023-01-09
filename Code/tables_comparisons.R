#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to create tables and look at differences between lakes, streams in network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Call in data and libraries ####
library(tidyverse)
library(colorblindr)
library(lme4)

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
        geom_point(aes(network_position, mean, group = season, fill = season), 
               color = "black", pch = 21, size = 2, position=position_dodge(width=0.1)) +
    geom_errorbar(aes(network_position, mean, ymin = mean-SD, ymax = mean+SD, color = season),
                  position=position_dodge(width=0.1), linetype = 'dashed')  +
    geom_smooth(aes(network_position, mean, group = season, color = season), alpha = 0.5, linewidth = 1, method = 'glm', se=FALSE) +
    #geom_line(aes(network_position, mean, group = season, color = season), alpha = 0.5, linewidth = 1) +
    scale_color_OkabeIto() +
    scale_fill_OkabeIto() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    facet_wrap(~param, ncol=3, scales = 'free_y')
}


# Note : these are plotted by changing the function above to remove geom_smooth and add in geom_line
plot_season(stats_nuts) 
ggsave("Figures/network_nuts_ts_seasonal.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_season(stats_ions)
ggsave("Figures/network_ions_ts_seasonal.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_season(stats_stoich) #+
 # ylim(0,NA)
ggsave("Figures/network_stoich_ts_seasonal.png", width = 10.5, height = 8.5, units = "in", dpi = 500)



# plotting by network position all data ####
plot_alldat <- function(data) {
  
  ggplot(data |> filter(season == 'All data')) +
    geom_point(aes(network_position, mean), 
               color = "black", pch = 21, size = 2) +
    geom_errorbar(aes(network_position, mean, ymin = mean-SD, ymax = mean+SD), linetype = 'dashed')  +
    geom_smooth(aes(network_position, mean, group = 1), method = 'glm') +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    #scale_y_log10() +
    facet_wrap(~param, ncol=3, scales = 'free_y')

}


plot_alldat(stats_nuts)
ggsave("Figures/network_nuts_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_alldat(stats_ions)
ggsave("Figures/network_ions_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_alldat(stats_stoich) #+
 # ylim(0,NA)
ggsave("Figures/network_stoich_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)



# COMPARE lakes vs rivers vs glacier for all parameters  ####

## Nutrients and DOC comparisons ####
ggplot(nuts, aes(eco_type, result)) +
  geom_jitter(aes(color = network_position), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  geom_violin(alpha = 0.2) +
  stat_summary(geom = 'point', fun.y = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('Network position',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '') +
  stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('glacier','lake'), c('lake','stream'), c('glacier','stream'))) +
  # ^^ Kruskal-Wallis test --- y.label.npc does not work ???!!!!!! 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  guides(color = guide_legend(ncol = 3)) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

ggsave('Figures/boxplots/eco_nuts.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)


### Results from kruskal-wallis tests ####

# DOC varies significantly among all groups
# DON varies significantly between glacier and streams and glacier and lakes, but not between the lakes and streams
# DOP varies significantly among all groups
# IN varies significantly among all groups 
# IP varies significantly between glacier and streams and glacier and lakes, but not between the lakes and streams
# PN varies significantly between glacier and streams and glacier and lakes, but not between the lakes and streams
# PP only varies significantly between glacier and lakes
# TDN varies significantly among all groups 
# TDP varies significantly among all groups 
# TN varies significantly between glacier and streams and glacier and lakes, but not between the lakes and streams
# TP varies significantly between glacier and streams and lakes and streams, but not between the glacier and lakes

## Nutrients stoichiometry comparisons ####
ggplot(stoich, aes(eco_type, result)) +
  geom_jitter(aes(color = network_position), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  geom_violin(alpha = 0.2) +
  stat_summary(geom = 'point', fun.y = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('Network position',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '') +
  stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('glacier','lake'), c('lake','stream'), c('glacier','stream'))) +
  # ^^ Kruskal-Wallis test --- y.label.npc does not work ???!!!!!! 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  guides(color = guide_legend(ncol = 3)) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

ggsave('Figures/boxplots/eco_stoich.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)


### Results from kruskal-wallis tests ####

# DON:DOP varies significantly among all groups
# IN:IP varies significantly among all except between glacier and streams
# PN:PP has no significant variation
# TDN:TDP varies significantly among all except between glacier and streams
# TN:TP varies significantly among all groups 


# COMPARE results along network  ####

# plotting by network position all data log scale ####
log_transform_stats <- function(data) {
  tmp <- data |>
    mutate(result = log10(result)) |>
    filter(is.finite(result),
           !is.na(result)) |>
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
    mutate(result = log10(result)) |>
    filter(is.finite(result),
           !is.na(result)) |>
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

logstats_nuts <- log_transform_stats(nuts) |>
  mutate(network_position = as.numeric(network_position))

logstats_stoich <- log_transform_stats(stoich)  |>
  mutate(network_position = as.numeric(network_position))


plot_alldat(logstats_nuts)
ggsave("Figures/network_nuts_ts_trend.png", width = 10.5, height = 8.5, units = "in", dpi = 500)
plot_season(logstats_nuts)
ggsave("Figures/network_nuts_ts_seasonal_trend.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

m <- lmer(mean ~ network_position + (1|season), logstats_nuts |> filter(param == 'TDN_umolL'))
summary(m)

lm_df_nuts <- data.frame()
list <- as.vector(logstats_nuts |> select(param) |> distinct())[['param']]

for(name in list) {
  mod <- lm(mean ~ network_position, logstats_nuts |> filter(param == name))
  
  slope <- round(mod$coefficients[2], 2)
  sig <- round(summary(mod)$adj.r.squared, 2)
  pvalue <- round(summary(mod)$coefficients[2,4], 2)
  
  tmp <- data.frame(slope, sig, pvalue) |>
    mutate(param = name)
  
  lm_df_nuts <- bind_rows(lm_df_nuts, tmp) |> distinct()
  
}

## Results from linear models - nutrients ####
# non-significant (p-value >0.05): DOP, PP
# DOC: slope = 0.03, adj R = 0.76
# DON: slope = -0.03, adj R = 0.34
# IN: slope = -0.05, adj R = 0.59
# IP: slope = -0.02, adj R = 0.33
# PN: slope = 0.02, adj R = 0.16 ****** low significance
# TDN: slope = -0.03, adj R = 0.52
# TDP: slope = -0.01, adj R = 0.07 ****** low significance
# TN: slope = -0.03, adj = 0.64 
# TP: slope = -0.01, adj R = 0.07 ****** low significance 



plot_alldat(logstats_stoich)
ggsave("Figures/network_stoich_ts_trend.png", width = 10.5, height = 8.5, units = "in", dpi = 500)
plot_season(logstats_stoich)
ggsave("Figures/network_stoich_ts_seasonal_trend.png", width = 10.5, height = 8.5, units = "in", dpi = 500)



lm_df_stoich <- data.frame()
list <- as.vector(logstats_stoich |> select(param) |> distinct())[['param']]

for(name in list) {
  mod <- lm(mean ~ network_position, logstats_stoich|> filter(param == name))
  
  slope <- round(mod$coefficients[2], 2)
  sig <- round(summary(mod)$adj.r.squared, 2)
  pvalue <- round(summary(mod)$coefficients[2,4], 2)
  
  tmp <- data.frame(slope, sig, pvalue) |>
    mutate(param = name)
  
  lm_df_stoich <- bind_rows(lm_df_stoich, tmp) |> distinct()
  
}


## Results from linear models - stoich ####
# non-significant (p-value >0.05): PN:PP
# DON:DOP - slope = -0.03, adj R = 0.15 ****** low significance
# IN:IP - slope = -0.03, adj R = 0.15 ****** low significance
# TDN:TDP - slope = -0.02, adj R = 0.14 ****** low significance
# TN:TP - slope = -0.02, adj R = 0.26 ** low, but highest significance 






