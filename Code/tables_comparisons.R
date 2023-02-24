#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to create tables and look at differences between lakes, streams in network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Data/CALL_DATA_PACKAGES.R') 


# get basic stats for each site, parameter ####
get_stats <- function(data) {
  
  tmp <- data |>
    group_by(site, eco_type, network_position, param, season) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() #|>
   # mutate(CV = (SD/mean) * 100) 
  
  tmp2 <- data |>
    group_by(site, eco_type, network_position, param) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() |>
    mutate(#CV = (SD/mean) * 100,
           season = 'All data') 
  
  df <- rbind(tmp, tmp2)
  
  rm(tmp)
  rm(tmp2)
  return(df)
  
}

#stats_ions <- get_stats(ions)
stats_nuts <- get_stats(nuts)
stats_stoich <- get_stats(stoich)



# get basic stats for each ecotype, parameter ####
get_stats_eco <- function(data) {
  
  tmp <- data |>
    group_by(eco_type,param, season) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() #|>
    #mutate(CV = (SE/mean) * 100) 
  
  tmp2 <- data |>
    group_by(eco_type, param) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() |>
    mutate(#CV = (SD/mean) * 100,
           season = 'All data') 
  
  df <- rbind(tmp, tmp2)
  
  rm(tmp)
  rm(tmp2)
  return(df)
  
}

#eco_stats_ions <- get_stats_eco(ions)
eco_stats_nuts <- get_stats_eco(nuts)
eco_stats_stoich <- get_stats_eco(stoich)



# plotting by network position and season ####
plot_season <- function(data) {
  ggplot(data |> filter(season != 'All data')) +
        geom_point(aes(network_position, mean, group = season, fill = season), 
               color = "black", pch = 21, size = 2, position=position_dodge(width=0.1)) +
    geom_errorbar(aes(network_position, mean, ymin = mean-SE, ymax = mean+SE, color = season),
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

# plot_season(stats_ions)
# ggsave("Figures/network_ions_ts_seasonal.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_season(stats_stoich) #+
 # ylim(0,NA)
ggsave("Figures/network_stoich_ts_seasonal.png", width = 10.5, height = 8.5, units = "in", dpi = 500)



# plotting by network position all data ####
plot_alldat <- function(data) {
  
  ggplot(data |> filter(season == 'All data')) +
    geom_point(aes(network_position, mean), 
               color = "black", pch = 21, size = 2) +
    geom_errorbar(aes(network_position, mean, ymin = mean-SE, ymax = mean+SE), linetype = 'dashed')  +
    geom_smooth(aes(network_position, mean, group = 1), method = 'glm') +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    #scale_y_log10() +
    facet_wrap(~param, ncol=3, scales = 'free_y')

}


plot_alldat(stats_nuts)
ggsave("Figures/network_nuts_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

# plot_alldat(stats_ions)
# ggsave("Figures/network_ions_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)

plot_alldat(stats_stoich) #+
 # ylim(0,NA)
ggsave("Figures/network_stoich_ts_all.png", width = 10.5, height = 8.5, units = "in", dpi = 500)



# COMPARE lakes vs rivers vs glacier for all parameters in boxplots.R  ####


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
              SE = std.error(result),
              n = n()) |>
     ungroup() #|>
    # mutate(CV = (SD/mean) * 100) 
  
  tmp2 <- data |>
    mutate(result = log10(result)) |>
    filter(is.finite(result),
           !is.na(result)) |>
    group_by(site, eco_type, network_position, param) |>
    summarise(mean = mean(result),
              median = median(result),
              min = min(result),
              max = max(result),
              SE = std.error(result),
              n = n()) |>
    ungroup() |>
    mutate(#CV = (SD/mean) * 100,
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

# m <- lmer(mean ~ network_position + (1|season), logstats_nuts |> filter(param == 'TDN_umolL'))
# summary(m)

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
# Significant p-values ::
# DOC: slope = 0.03, adj R = 0.79
# DON: slope = -0.03, adj R = 0.4
# IN: slope = -0.05, adj R = 0.59
# IP: slope = -0.02, adj R = 0.41
# PN: slope = 0.02, adj R = 0.18 ****** low significance
# TDN: slope = -0.03, adj R = 0.54
# TDP: slope = -0.01, adj R = 0.21 ****** low significance
# TN: slope = -0.03, adj = 0.66 
# TP: slope = -0.01, adj R = 0.06 ****** low significance 



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
# Significant p values :: 
# DON:DOP - slope = -0.04, adj R = 0.21 ****** low significance
# IN:IP - slope = -0.02, adj R = 0.12 ****** low significance
# TDN:TDP - slope = -0.01, adj R = 0.10 ****** low significance
# TN:TP - slope = -0.02, adj R = 0.26 ** low, but highest significance 






