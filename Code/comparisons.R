#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Nutrient and stoichiometry scatterplots in vs out, in vs lake, lake vs out ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

## Prepare data ####

setup <- rbind(nuts, stoich) |>
  mutate(position = ifelse(grepl("INLET", site), 'inlet', NA)) |>
  mutate(position = ifelse(grepl("OUTLET", site), 'outlet', position)) |>
  mutate(position = ifelse(eco_type=='lake', 'in-lake', position)) |>
  mutate(position = ifelse(site=='GL1_LAKE', 'inlet2', position)) |>
  left_join(sites) |>
  left_join(greenlakes_LC) |>
  select(-Layer_1, -LandCoverArea_km2, -depth_m, - arik_flow_site, - notes, - elevation_m, - drainage_area_ha, - geometry) |>
  distinct()

## Write loop to create plots and dataframe ####
params <- unique(setup$param) # list of params
comp_data <- data.frame() # create empty dataframe

for(p in 1:length(params)) {
  tmp <- setup |>
    filter(param==params[p]) |>
    group_by(site,date,param,position,WS_Group,season) |>
    summarise(result=mean(result)) |>
    ungroup() |>
    pivot_wider(id_cols=c('WS_Group', 'date'), names_from='position',values_from='result') |>
    mutate(param=params[p])
  
  comp_data <- bind_rows(comp_data, tmp) 
}

# this madness is just making pretty labels
comp_data$param <- factor(comp_data$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')')))   

## Plot inlet vs outlet ####
ggplot(comp_data) +
  geom_point(aes(inlet, outlet, color=)) +
  theme_bw(base_size = 15) +
  geom_abline(slope=1,intercept=0, color= 'red4') +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5)
ggsave('Figures/Comparisons/inlets_outlets.png', width=10.5, height=8.5, units='in', dpi=1200)

## Plot inlet vs lake ####
ggplot(comp_data) +
  geom_point(aes(inlet, `in-lake`)) +
  geom_point(aes(inlet2, `in-lake`)) + # add the Green Lake 1 here (it serves as inlet to Albion Lake in this case)
  theme_bw(base_size = 15) +
  geom_abline(slope=1,intercept=0, color= 'red4') +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5)
ggsave('Figures/Comparisons/inlets_in-lake.png', width=10.5, height=8.5, units='in', dpi=1200)


## Plot in-lake vs outlet ####
ggplot(comp_data) +
  geom_point(aes(`in-lake`, outlet)) +
  theme_bw(base_size = 15) +
  geom_abline(slope=1,intercept=0, color= 'red4') +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5)
ggsave('Figures/Comparisons/in-lake_outlets.png', width=10.5, height=8.5, units='in', dpi=1200)



# Now, let's create a ladder plot of inlet, in-lake, outlet concentrations ####

ladder <- setup |>
  filter(site != 'GL2_LAKE') |>
  mutate(position = ifelse(position=='inlet2','inlet',position))  |>
  group_by(WS_Group, position, param) |>
  mutate(median_result = median(result))|>
  ungroup()

factor(unique(ladder$position)) # checking factor levels
ladder$position <- factor(ladder$position, levels = c('inlet','in-lake','outlet')) # changing factor levels
factor(unique(ladder$position)) # checking new factor levels 
ladder$WS_Group <- factor(ladder$WS_Group, levels = c('GL5','GL4','GL3','ALB'))

# this madness is just making pretty labels
ladder$param <- factor(ladder$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 


# note, I am cutting the y-axis to be less than 500. This changes the appearance of IN:IP, which has inlet data extending to 1000 and outlet to >3000; TDN:TDP, which has outlet results extending beyond 9000 and inlet results extending beyond 500; and TN:TP, which has outlet results extending beyond 600
ggplot(ladder |>filter(result<500)) +
  geom_violin(aes(position, result)) +
  geom_jitter(aes(position, result, color=WS_Group), alpha=0.4) +
  geom_point(aes(position, median_result, fill=WS_Group),color='black', size=4, shape=21) +
  geom_line(aes(position, median_result, group=WS_Group)) +
  scale_color_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  labs(x='',y='') +
  facet_wrap(~param, scales='free',ncol=3,labeller=label_parsed) +
  theme_classic()

ggsave('Figures/Comparisons/ladder.png', height=10.5, width=8.5, units='in',dpi=1200)

