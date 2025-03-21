#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Theil-Sens slope and Mann-Kendall Tests for network trends
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) 


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


## 3a. Mann-Kendall (non-parametric) test varied by szn ####
## get sen's slopes and intercepts by szn 
params <- unique(all_data_trend$param)
szns <-unique(all_data_trend$szn)
sens.estimates <- data.frame()

for(p in 1:length(params)) {
  for(s in 1:length(szns)) {
    tmp <- all_data_trend[order(as.numeric(as.character(all_data_trend$network_position))),] |>
      filter(param == params[p],
             szn == szns[s])
    
    intercept = as.numeric(zyp.sen(result~network_position, tmp)$coefficients[[1]])
    senslope = as.numeric(zyp.sen(result~network_position, tmp)$coefficients[[2]])
    
    tmp2 <- data.frame(param = params[p],szn = szns[s], intercept = intercept, sensslope = senslope)
    
    sens.estimates <- rbind(sens.estimates, tmp2)
  }
}


## test slopes varied by szn
mk_df_szn <-  all_data_trend[order(as.numeric(as.character(all_data_trend$network_position))),] |>
  # we need to order by network position ^^^ so that the Mann Kendall and Sens slopes use the vector over the network, rather than treat it as timeseries
  group_by(param, szn) |>
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
  mutate(szn = 'Global slope') |>
  # this madness is making pretty labels
  mutate(param = factor(param,levels=c('DON_umolL','DOP_umolL','don.dop', 'IN_umolL','IP_umolL','in.ip', 'PN_umolL','PP_umolL', 'pn.pp','TDN_umolL','TDP_umolL','tdn.tdp','TN_umolL','TP_umolL','tn.tp'), labels = c(expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'),expression('(DON:DOP)'),  expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'),expression('(IN:IP)'),  expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'),expression('(PN:PP)'),  expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TDN:TDP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'),expression('(TN:TP)')))) |>
  filter(!is.na(significance))
  

mk_plot_szn <- mk_df_szn |>
  filter(!is.na(significance))
 

plot_trend <- full_join(all_data_trend, mk_plot_szn|> select(-n))

# this madness is just making pretty labels
plot_trend$param <- factor(plot_trend$param, levels=c('DON_umolL','DOP_umolL','don.dop', 'IN_umolL','IP_umolL','in.ip', 'PN_umolL','PP_umolL', 'pn.pp','TDN_umolL','TDP_umolL','tdn.tdp','TN_umolL','TP_umolL','tn.tp'), labels = c(expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'),expression('(DON:DOP)'),  expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'),expression('(IN:IP)'),  expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'),expression('(PN:PP)'),  expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TDN:TDP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'),expression('(TN:TP)')))




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
  geom_jitter(aes(as.factor(network_position), result), alpha=0.1) +
#  geom_errorbar(aes(network_position, mean, ymin = mean-SE, ymax = mean+SE), linetype = 'dashed')  +
  # Manually plot the line with geom_segment
  geom_segment(data = mk_plot_global, 
               aes(x = network_range[1], xend = network_range[2],
                   y = y_start, yend = y_end,
                   color = szn)) +
               #lwd = 1.5) +
  # Manually plot the line with geom_segment
  geom_segment( # using plot_trend for sznal data
               aes(x = network_range[1], xend = network_range[2],
                   y = y_start, yend = y_end,
                   color = szn)) +
               #lwd = 1.5) +
  geom_point(aes(as.factor(network_position), mean, fill=WS_Group, shape=eco_type), 
             color = "black", size = 2.5) +
  scale_shape_manual('', values=c(21,22,23)) +
  scale_color_manual('', values=c('grey50','blue4','palegreen4','goldenrod3')) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_log10() + # log for better viewing
  facet_wrap(.~param, scales='free_y', labeller=label_parsed, nrow=5, axes='all',axis.labels='all_y') +
  labs(x = '', y = '') +
  theme(legend.position = 'none') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) #+ # removes facet titles
 
ggsave('Figures/networkTrends_noLabels.png', width=10.5, height=8.5, units='in', dpi=1200)



# 5. Slopes plot ####
slopes <- mk_plot_szn |>
  select(param, szn, sensslope, intercept, significance) |>
  distinct() |>
  drop_na() |>
  rbind(mk_df |>
          select(param, sensslope, intercept, significance) |>
          mutate(szn='Global slope')) |>
  mutate(nutrient = NA) |>
  mutate(nutrient = case_when(grepl('N', param)~'N',
                              grepl('P',param)~'P',
                              is.na(nutrient)~'Ratio')) |>
  mutate(szn=factor(szn, levels=c('Global slope', 'baseflow', 'spring snowmelt','falling limb'))) |>
  drop_na() |>
  mutate(param = sub('_.*','',param),
         param = ifelse(param=='don.dop', 'DON:DOP',
                          ifelse(param=='in.ip', 'IN:IP',
                            ifelse(param=='pn.pp', 'PN:PP',
                              ifelse(param=='tdn.tdp','TDN:TDP', param)))))



r <- ggplot(slopes|>filter(nutrient=='Ratio')) +
  geom_boxplot(aes(sensslope, szn, color=szn)) +
  geom_jitter(aes(sensslope, szn, shape=param),size=2) +
  scale_color_manual('',values=c('grey50','blue4','palegreen4','goldenrod3')) +
  geom_vline(xintercept = 0, color='grey20') +
  labs(y='',x='', title='Ratio') +
  theme_bw() +
  guides(color='none') +
  scale_shape_manual('', values=c(0,1,2,3,4)) +
  theme(#legend.position=c(0.15,0.5),
        legend.box.background = element_rect(),
        legend.title=element_blank())

n <- ggplot(slopes|>filter(nutrient=='N')) +
  geom_boxplot(aes(sensslope, szn, color=szn)) +
  geom_jitter(aes(sensslope, szn,shape=param),size=2) +
  scale_color_manual('',values=c('grey50','blue4','palegreen4','goldenrod3')) +
  geom_vline(xintercept = 0, color='grey20') +
  labs(y='',x='Slope',title='Nitrogen') +
  theme_bw() +
  guides(color='none') +
  scale_shape_manual('', values=c(0,1,2,3,4)) +
  theme(#legend.position=c(0.1,0.75),
        legend.box.background = element_rect(),
        legend.title=element_blank())



p <- ggplot(slopes|>filter(nutrient=='P')) +
  geom_boxplot(aes(sensslope, szn, color=szn)) +
  geom_jitter(aes(sensslope, szn,shape=param),size=2) +
  scale_color_manual('',values=c('grey50','blue4','palegreen4','goldenrod3')) +
  geom_vline(xintercept = 0, color='grey20') +
  labs(y='',x='',title='Phosphorus') +
  theme_bw() +
  guides(color='none') +
  scale_shape_manual('', values=c(0,1,2,3,4)) +
  theme(#legend.position=c(0.1,0.75),
        legend.box.background = element_rect(),
        legend.title=element_blank())


r/n/p

ggsave('Figures/slopes.png', height=8.5,width=6.5,units='in',dpi=1200)
