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
param <- unique(setup$param) # list of params
comp_data <- data.frame() # create empty dataframe

for(p in 1:length(param)) {
  tmp <- setup |>
    filter(param==param[p]) |>
    group_by(site,network_position,eco_type,date,param,position,WS_Group,season) |>
    summarise(result=mean(result)) |>
    pivot_wider(id_cols=c('WS_Group', 'date', 'season'), names_from='position',values_from='result') |>
    mutate(param=param[p])
  
  comp_data <- rbind(comp_data, tmp) 
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
