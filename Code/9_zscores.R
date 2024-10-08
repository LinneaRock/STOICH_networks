# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 


# 2. prepare data ####
data <- nuts |>
  mutate(nutrient = case_when(grepl('N', param)~'Nitrogen',
                              grepl('P', param)~'Phosphorus')) |>
  rbind(stoich |>
          mutate(nutrient='Ratio')) |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  left_join(distances_Km |>
              filter(site1=='Arikaree_GLACIER') |>
              select(-site1) |>
              rename(site=site2,
                     distancefromglacier_Km=distance_Km)
  ) |>
  mutate(distancefromglacier_Km = ifelse(is.na(distancefromglacier_Km), 
                                         0, distancefromglacier_Km)) |>
  distinct() |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  mutate(distancefromglacier_Km=as.numeric(distancefromglacier_Km)) |>
  mutate(param = sub('_.*','',param),
         param=ifelse(param=='tn.tp', 'TN:TP',
                      ifelse(param=='tdn.tdp','TDN:TDP',
                             ifelse(param=='pn.pp','PN:PP',
                                    ifelse(param=='in.ip','IN:IP',
                                           ifelse(param=='don.dop','DON:DOP', param)))))) |>
  left_join(greenlakes_LC |>
              pivot_wider(names_from = Layer_1, values_from = LandCoverArea_km2) |>
              mutate(`Evergreen Forest` = ifelse(is.na(`Evergreen Forest`), 0, `Evergreen Forest`),
                     `Woody Wetlands` = ifelse(is.na(`Woody Wetlands`), 0, `Woody Wetlands`))) |>
  select(-depth_m) |>
  distinct() 




# 2. Zscore ####
# get zscores for each site and param within it's subwatershed
scored_data <- data |>
  group_by(WS_Group, param) |>
  mutate(mean=as.numeric(mean(testdat$result)),
         std=as.numeric(sd(testdat$result))) |>
  ungroup() |>
  group_by(param,site) |>
  add_count() |>
  mutate(zscore=(result-mean)/std) |>
  mutate(ave_z = mean(zscore)) |>
  mutate(CIup = ifelse(n>1, (t.test(zscore))$conf.int[2], NA)) |>
  mutate(CIdown = ifelse(n>1, (t.test(zscore))$conf.int[1], NA)) |>
  ungroup()

scores <- scored_data |>
  select(site,eco_type,nutrient,param,network_position,upstream_network_lakes,WS_Group,distancefromglacier_Km,ave_z,CIup,CIdown) |>
  distinct()  |>
  mutate(WS_Group=factor(WS_Group, levels=c('GL5','GL4','GL3','ALB')))


# 3. Plot ####
ggplot(scores, aes(upstream_network_lakes, ave_z, shape=eco_type, fill=WS_Group)) +
  geom_jitter(size=2) +
 # geom_errorbar(aes(ymin=CIdown,ymax=CIup))
  facet_wrap(~nutrient, scales='free_y') +
  scale_shape_manual('',values=c(21,22,25)) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8'))+
  labs(x='Number upstream lakes', y='Average Zscore of each parameter and \nsite within subwatersheds') +
  theme_bw()
ggsave('Figures/zscore_upstreamlakes.png',width=8.5,height=4.5,units='in',dpi=1200)
  

ggplot(scores, aes(param, ave_z, shape=eco_type, fill=network_position)) +
  geom_jitter(size=2) +
  facet_wrap(~nutrient, scales='free') +
  labs(x='', y='Average Zscore of each parameter and \nsite within subwatersheds') +
  scale_fill_viridis_c()+
  scale_shape_manual('',values=c(21,22,25)) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 45))
ggsave('Figures/zscore_params.png',width=8.5,height=4.5,units='in',dpi=1200)
