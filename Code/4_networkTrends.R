#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Theil-Sens slope for network trends
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

all_data_trend <- rbind(nuts, stoich) |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  group_by(network_position, param) |>
  mutate(mean = mean(result),
         median = median(result),
         min = min(result),
         max = max(result),
         SE = std.error(result),
         n = n()) |>
  ungroup() |>
  distinct() |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  # remove GL1 from these analyses
  filter(site != 'GL1_LAKE') |>
  mutate(network_position = network_position+1)


# 2. get sen's slopes and intercepts ####
params <- unique(all_data_trend$param)
sens.estimates <- data.frame()

for(p in 1:length(params)) {
  tmp <- all_data_trend[order(as.numeric(as.character(all_data_trend$network_position))),] |>
    filter(param == params[p])
  
  intercept = as.numeric(zyp.sen(result~network_position, tmp)$coefficients[[1]])
  senslope = as.numeric(zyp.sen(result~network_position, tmp)$coefficients[[2]])
  
  tmp2 <- data.frame(param = params[p], intercept = intercept, sensslope = senslope)
  
  sens.estimates <- rbind(sens.estimates, tmp2)
  
}

# 3. Mann-Kendall (non-parametric) test to see if significant trends exist in our data ####
mk_df <-  all_data_trend[order(as.numeric(as.character(all_data_trend$network_position))),] |>
  # we need to order by network position ^^^ so that the Mann Kendall uses the vector over the network, rather than treat it as timeseries
  group_by(param) |>
  summarise(z.stat = glance(mk.test(result))$statistic,
            p.value = glance(mk.test(result))$p.value,
            n = glance(mk.test(result))$parameter) |>
  ## Calculate Sen's slope and add to dataframe #### -- OLD -- need to use zyp function
  # slope = as.numeric(sens.slope(result)[[1]])) |>
  ungroup() |>
  mutate(significance = ifelse(between(p.value, 0.001, 0.05), '*',
                               ifelse(between(p.value, 0.0001, 0.001), '**',
                                      ifelse(p.value < 0.0001, '***', NA)))) |>
  left_join(sens.estimates)


## 3a. Mann-Kendall (non-parametric) test varied by season ####
## get sen's slopes and intercepts by season 
params <- unique(all_data_trend$param)
seasons <-unique(all_data_trend$season)
sens.estimates <- data.frame()

for(p in 1:length(params)) {
  for(s in 1:length(seasons)) {
    tmp <- all_data_trend[order(as.numeric(as.character(all_data_trend$network_position))),] |>
      filter(param == params[p],
             season == seasons[s])
    
    intercept = as.numeric(zyp.sen(result~network_position, tmp)$coefficients[[1]])
    senslope = as.numeric(zyp.sen(result~network_position, tmp)$coefficients[[2]])
    
    tmp2 <- data.frame(param = params[p],season = seasons[s], intercept = intercept, sensslope = senslope)
    
    sens.estimates <- rbind(sens.estimates, tmp2)
  }
}


## test slopes varied by season
mk_df_season <-  all_data_trend[order(as.numeric(as.character(all_data_trend$network_position))),] |>
  # we need to order by network position ^^^ so that the Mann Kendall and Sens slopes use the vector over the network, rather than treat it as timeseries
  group_by(param, season) |>
  summarise(z.stat = glance(mk.test(result))$statistic,
            p.value = glance(mk.test(result))$p.value,
            n = glance(mk.test(result))$parameter) |>
  ungroup() |>
  mutate(significance = ifelse(between(p.value, 0.001, 0.05), '*',
                               ifelse(between(p.value, 0.0001, 0.001), '**',
                                      ifelse(p.value < 0.0001, '***', NA)))) |>
  left_join(sens.estimates)


# 4. Plot trends ####
## 4a. format dataset for plotting ####

mk_plot_global <- mk_df |>
  mutate(season = 'Global slope') |>
  # this madness is making pretty labels
  mutate(param = factor(param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')')))) |>
  filter(!is.na(significance))
  

mk_plot_season <- mk_df_season |>
  filter(!is.na(significance))
 

plot_trend <- full_join(all_data_trend, mk_plot_season|> select(-n))

# this madness is just making pretty labels
plot_trend$param <- factor(plot_trend$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 




# factor subwatersheds
plot_trend$WS_Group <- factor(all_data_trend$WS_Group, levels = c('GL5','GL4','GL3','ALB'))


# for better plot viewing
# Calculate the range for network_position
network_range <- range(as.numeric(plot_trend$network_position), na.rm = TRUE)
# Calculate points for lines
mk_plot_global <- mk_plot_global |>
  mutate(y_start = intercept + sensslope * network_range[1],
         y_end = intercept + sensslope * network_range[2])
plot_trend <- plot_trend |>
  mutate(y_start = intercept + sensslope * network_range[1],
         y_end = intercept + sensslope * network_range[2])


ggplot(plot_trend) +
  geom_jitter(aes(network_position, result, color=WS_Group), alpha=0.1) +
  geom_point(aes(network_position, mean, fill=WS_Group), 
             color = "black", pch = 21, size = 2) +
#  geom_errorbar(aes(network_position, mean, ymin = mean-SE, ymax = mean+SE), linetype = 'dashed')  +
  # Manually plot the line with geom_segment
  geom_segment(data = mk_plot_global, 
               aes(x = network_range[1], xend = network_range[2],
                   y = y_start, yend = y_end,
                   color = season)) +
               #lwd = 1.5) +
  # Manually plot the line with geom_segment
  geom_segment( # using plot_trend for seasonal data
               aes(x = network_range[1], xend = network_range[2],
                   y = y_start, yend = y_end,
                   color = season)) +
               #lwd = 1.5) +
  scale_color_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8','grey50','blue4','palegreen4','goldenrod3')) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_log10() + # log for better viewing
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  labs(x = 'Network Position', y = '') +
  theme(legend.position = 'none')
ggsave('Figures/networkTrends.png', width=10.5, height=8.5, units='in', dpi=1200)

