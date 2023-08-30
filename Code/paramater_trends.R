#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Theil-Sens slope for network trends ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

all_data_trend <- rbind(nuts, stoich) |>
  mutate(network_position = ifelse(site=='GL1_LAKE', 11.5, 
                                   ifelse(site=='ALB_OUTLET', 13,
                                          ifelse(site=='ALB_LAKE', 12, network_position)))) |>
  mutate(network_position = network_position + 1) |>
  group_by(network_position, param) |>
  mutate(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SE = std.error(result),
            n = n()) |>
  ungroup() |>
  left_join(sites |> select(-network_position)) |>
  left_join(greenlakes_LC) |>
  select(-Layer_1, -LandCoverArea_km2, -depth_m, - arik_flow_site, - notes, - elevation_m, - drainage_area_ha, - geometry) |>
  distinct() |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |> mutate(season = ifelse(season %in% c('Oct-Dec', 'Jan-Mar'), 'winter', 
                                                                                          ifelse(season=='Apr-Jun', 'snowmelt runoff', 'summer')))



## get sen's slopes and intercepts ####
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

## Mann-Kendall (non-parametric) test to see if significant trends exist in our data ####
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






### Mann-Kendall (non-parametric) test varied by season ####
## get sen's slopes and intercepts by season ####
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

# this madness is just making pretty labels
all_data_trend$param <- factor(all_data_trend$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 

# factor subwatersheds
all_data_trend$WS_Group <- factor(all_data_trend$WS_Group, levels = c('GL5','GL4','GL3','ALB'))

ggplot(all_data_trend ) +
  geom_jitter(aes(network_position, result, color=WS_Group), alpha=0.1) +
  geom_point(aes(network_position, mean, fill=WS_Group), 
             color = "black", pch = 21, size = 2) +
  geom_errorbar(aes(network_position, mean, ymin = mean-SE, ymax = mean+SE), linetype = 'dashed')  +
  scale_color_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_log10() +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  labs(x = 'Network Position', y = '') +
  theme(legend.position = 'bottom')
ggsave('Figures/Trends/network_scale_y_log10.png', width=10.5, height=8.5, units='in', dpi=1200)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# plots for SFS focusing on total and inorganic nutrients #### 
# changed to look at all nutrients!!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
sfs_trend <- all_data_trend |> 
  filter(param %in% c(expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')')))

mk_sfs <- mk_df |>
  # filter(param %in% c('in.ip','IN_umolL', 'IP_umolL', 'tn.tp', 'TN_umolL', 'TP_umolL')) |>
  mutate(season = 'global slope') |>
  rbind(mk_df_season) |>
          # filter(param %in% c('in.ip','IN_umolL', 'IP_umolL', 'tn.tp', 'TN_umolL', 'TP_umolL'))) |>
  mutate(significance = ifelse(is.na(significance), 'p > 0.05', 'p < 0.05')) |>
  # mutate(param = factor(param, labels = c(expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')')))) |>
  # filter(season != 'global slope')
  mutate(param = factor(param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')')))) 

# annoying workaround to keep NA out of subwatershed for plotting
mk_sfs_global <- mk_df |>
  # filter(param %in% c('in.ip','IN_umolL', 'IP_umolL', 'tn.tp', 'TN_umolL', 'TP_umolL')) |>
  mutate(season = 'global slope') |>
  mutate(significance = ifelse(is.na(significance), 'p > 0.05', 'p < 0.05')) |>
  # mutate(param = factor(param, labels = c(expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))))
  mutate(param = factor(param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')')))) 


sfs_trend <- full_join(sfs_trend, mk_sfs|> select(-n))

# factor subwatersheds
sfs_trend$WS_Group <- factor(sfs_trend$WS_Group, levels = c('GL5','GL4','GL3','ALB'))

ggplot(sfs_trend) +
  geom_jitter(sfs_trend |> filter(result < 500), mapping=aes(network_position, result, color=WS_Group), alpha=0.25) +
  geom_point(aes(network_position, mean, fill=WS_Group), 
             color = "black", pch = 21, size = 2) +
  geom_errorbar(aes(network_position, mean, ymin = mean-SE, ymax = mean+SE), linetype='dotted')  +
  #scale_color_manual('', values=c('grey80','green4','goldenrod','blue1')) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  #scale_color_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) + 
  # more stupid workarounds for a nice presentation plot :( 
  scale_color_manual('', values=c('#B5DDD8','#81C4E7','#9398D2','#906388','grey50','green4','goldenrod','blue1')) +
 theme_bw() +
  #dark_theme_bw(base_size=15) +
  geom_abline(aes(intercept=intercept, slope=sensslope, linetype=significance, color=season)) +
  geom_abline(mk_sfs_global, mapping=aes(intercept=intercept, slope=sensslope, linetype=significance, color=season)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  #scale_y_log10() +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  labs(x = 'Network Position', y = '') +
  theme(legend.position = 'bottom') +
  scale_linetype_manual('', values=c(1,3))
# ggsave('Figures/DarkTheme/network_trend.png', width=12.5, height=10.5, units='in', dpi=1200)

ggsave('Figures/Trends/log10Scale_MKtests.png', width=10.5, height=8.5, units='in', dpi=1200)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Use gams to assess trends along network ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
source('Functions/derivative_analyses.R')

nuts.gam <- nuts |>
  mutate(network_position = ifelse(network_position=='12a', '12.5', network_position)) |>
  mutate(network_position = as.numeric(network_position))

stoich.gam <- stoich |>
  mutate(network_position = ifelse(network_position=='12a', '12.5', network_position)) |>
  mutate(network_position = as.numeric(network_position))

## Quick plots of all the variables of interest ####  
ggplot(nuts.gam |>
         filter(!param %in% c('NH4_ueqL', 'NO3_ueqL', 'DOC_mgL')), 
       aes(network_position, result, group=param)) +
  facet_wrap(~param, scales='free_y') +
  geom_point(shape=21)+
  geom_smooth(method = "gam", se = TRUE, color="black") + #fit a gam
  labs(y="Concentration", x="Network position") +
  theme_pubr()

ggplot(stoich.gam, aes(network_position, result, group=param)) +
  facet_wrap(~param, scales='free_y') +
  geom_point(shape=21)+
  geom_smooth(method = "gam", se = TRUE, color="black") + #fit a gam
  labs(y="Molar ratio", x="Network position") +
  theme_pubr() +
  scale_y_log10()

## Identifying and plotting periods of change ####
### Where along the network is rate of change in the parameters increasing or decreasing? ####
gam0 <- gam(
  result ~  s(network_position,k=4), #formula, where k is the basis dimension
  data = stoich.gam |> filter(param=='don.dop'), #dataset 
  method = "REML" #The smoothing parameter estimation method, REML is default
)
summary(gam0)

# From the broom package
glance(gam0)
tidy(gam0)



#### Extract network_positions ####
networkPos <- with(stoich.gam, data.frame(network_position=seq(min(network_position), max(network_position), length.out=20)))

#### Create a dataframe with predicted ("fitted") values from the GAM and year, on the response scale ####
Pred <- cbind(networkPos,
              data.frame(predict(
                gam0, networkPos,
                type = "response",
                se.fit = TRUE
              )))
head(Pred)

#### Calculate upper and lower bounds ####
Pred <- transform(Pred,
                  upper = fit + (2 * se.fit),
                  lower = fit - (2 * se.fit))

head(Pred)


#### Extract first derivative of the trend ####
Term = "network_position"
m1.d <- Deriv(gam0) 

#### Calculate confidence intervals around the first derivative ####
m1.dci <- confint(m1.d, term = "network_position")

# Extract periods of increasing or decreasing trends
m1.dsig <- signifD(Pred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

# Plot the first derivative 
plot.Deriv(m1.d)
# To interpret these first derivative plots– if the confidence intervals DO NOT overlap 0, it means that the trend is either increasing or decreasing.





# Add a column for periods of time when the trend is accelerating
Pred <- cbind(Pred, data.frame(incr=unlist(m1.dsig$incr)))


Pred %>%
  ggplot(aes(x=network_position,y=fit))+
  geom_point(data=stoich.gam |> filter(param=='don.dop'), aes(x=network_position, y=result),
             shape=21,fill="grey50", alpha=0.5)+ #Plot raw data
  geom_line(size=1, alpha=0.8)+ #Plot fitted trend
  geom_line(aes(x=network_position, y=incr), color="red", size=1, alpha=0.8)+ #Highlight period of increasing trend
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = network_position), alpha = 0.5, inherit.aes = FALSE) + #Plot CI around fitted trend
  labs(x="Network position",y="Molar ratio")+
  # coord_cartesian(xlim=c(1930,2020),
  #                 ylim=c(22,30))+
  # scale_x_continuous(breaks=seq(1930, 2020, 15))+
  # scale_y_continuous(breaks=seq(22,30,2))+
  theme_pubr(base_size=8, border=TRUE)




# below was timeseries trends
# ## Prepare data 
# nuts.lm <- nuts |>
#   mutate(year=year(date)) |>
#   group_by(network_position, param) |>
#   add_count() |>
#   ungroup() |>
#   filter(n>3)
# 
# stoich.lm <- stoich |>
#   mutate(year=year(date)) |>
#   group_by(network_position, param) |>
#   add_count() |>
#   ungroup() |>
#   filter(n>3)
# 
# ## Quick plots of all the variables of interest 
# ggplot(nuts.lm, aes(year, result, group=network_position, color=param)) +
#   geom_point(shape=21, alpha=0.5) +
#   #scale_y_log10() +
#   facet_grid(param~network_position, scales="free") +
#   geom_smooth(method="lm", color="black")
# 
# ggplot(stoich.lm, aes(year, result, group=network_position, color=param)) +
#   geom_point(shape=21, alpha=0.5) +
#   #scale_y_log10() +
#   facet_grid(param~network_position, scales="free") +
#   geom_smooth(method="lm", color="black")
# 
# ## Mann-Kendall (non-parametric) test to see if significant trends exist in our data 
# mk_df <- nuts.lm |>
#   group_by(network_position, param) |>
#   summarise(z.stat = glance(mk.test(result))$statistic,
#          p.value = glance(mk.test(result))$p.value,
#          n = glance(mk.test(result))$parameter,
# ## Calculate Sen's slope and add to dataframe 
#          slope = as.numeric(sens.slope(result)[[1]])) |>
#   ungroup() |>
#   rbind((stoich.lm |>
#            group_by(network_position, param) |>
#            summarise(z.stat = glance(mk.test(result))$statistic,
#                      p.value = glance(mk.test(result))$p.value,
#                      n = glance(mk.test(result))$parameter,
#                      slope = as.numeric(sens.slope(result)[[1]])) |>
#            ungroup()))
# 
# 
# ggplot(mk_df |> filter(p.value<=0.05,
#                        n>=10), aes(slope, network_position,color=param)) +
#   geom_jitter() +
#   theme_bw() +
#   geom_vline(xintercept = 0)






