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
  distinct() |>
  mutate(position = ifelse(grepl('INLET', site), 'inlets', NA),
         position = ifelse(grepl('OUTLET', site), 'outlets', position),
         position = ifelse(eco_type == 'lake', 'lakes', position),
         position = ifelse(eco_type == 'glacier', 'glacier', position)) |>
  mutate(nut_type = case_when(param %in% c('TN','TP','TN:TP')~'total',
                              param %in% c('TDN','TDP','TDN:TDP')~'total dissolved',
                              param %in% c('PN','PP','PN:PP')~'particulate',
                              param %in% c('IN','IP','IN:IP')~'inorganic',
                              param %in% c('DON','DOP','DON:DOP')~'dissolved organic'),
         nut_type = factor(nut_type, levels = c('dissolved organic', 'inorganic', 'particulate', 'total dissolved', 'total')))






# 2. Zscore subwatershed  ####
# get zscores for each site and param within it's subwatershed
scored_data <- data |>
  group_by(WS_Group, param) |>
  mutate(mean=as.numeric(mean(result)),
         std=as.numeric(sd(result))) |>
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


## 2a. Plot ####
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








# 3. Zscore full watershed  ####
# get zscores for each site and param within it's subwatershed
scored_data_fullwat <- data |>
  group_by(param) |>
  mutate(mean=as.numeric(mean(result)),
         std=as.numeric(sd(result))) |>
  ungroup() |>
  group_by(param, site) |>
  add_count() |>
  mutate(zscore=(result-mean)/std) |>
  mutate(ave_z = mean(zscore)) |>
  mutate(CIup = ifelse(n>1, (t.test(zscore))$conf.int[2], NA)) |>
  mutate(CIdown = ifelse(n>1, (t.test(zscore))$conf.int[1], NA)) |>
  ungroup()

scores_fullwat <- scored_data_fullwat |>
  select(site,eco_type,nutrient,param,network_position,upstream_network_lakes,position,WS_Group,distancefromglacier_Km,ave_z,CIup,CIdown, nut_type) |>
  distinct()  |>
  mutate(WS_Group=factor(WS_Group, levels=c('GL5','GL4','GL3','ALB')))


## 3a. Plot ####
library(colorblindr)
# Get the full Okabe-Ito palette
library(scales)
show_col(colorblindr::palette_OkabeIto_black)
okabe_ito_colors <- palette_OkabeIto_black

ggplot(scores_fullwat, aes(distancefromglacier_Km, ave_z,color=position)) +
  geom_jitter(size=2) +
  facet_wrap(~nutrient, scales='free_y') +
  scale_color_manual('',values=c('grey50', okabe_ito_colors[6], okabe_ito_colors[8], okabe_ito_colors[7]), labels=c('glacier','inlets','lakes','outlets')) +
  labs(x='Distance from glacier (km)', y='Average Zscore of each parameter and site') +
  theme_bw()

ggplot(scores_fullwat, aes(position, ave_z)) +
  geom_violin(aes(color=position)) +
  geom_jitter(aes(fill=nut_type),shape=21) +
  scale_color_manual('',values=c('grey50', okabe_ito_colors[6], okabe_ito_colors[8], okabe_ito_colors[7]), labels=c('glacier','inlets','lakes','outlets')) +
  scale_fill_manual('',values=c(okabe_ito_colors[1:5])) +
  facet_wrap(~nutrient, scales='free_y') +
  labs(x='', y='Average z-score of each parameter and site') +
  theme_bw()+
  theme() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(color='none')

ggsave('Figures/zscore_allWatershed.png',width=8.5,height=4.5,units='in',dpi=1200)








# 4. Zscore of inlets and outlets using subwatershed lakes as the pop ave  ####
#get zscores for each site and param within it's subwatershed
# exclude GL1 and GL2 which don't have inlets/outlets monitored
lake_means <- data |>
  filter(!site%in%c('GL2_LAKE','GL1_LAKE')) |>
  filter(eco_type=='lake') |>
  group_by(param, site, WS_Group) |>
  summarise(mean=as.numeric(mean(result, na.rm=TRUE)),
            std=as.numeric(mean(result,na.rm=TRUE))) |>
  ungroup() |>
  select(-site)
  


scored_data_lakes <- data |>
              filter(position%in%c('inlets','outlets')) |>
  left_join(lake_means) |>
  group_by(param, site) |>
  add_count() |>
  mutate(zscore=(result-mean)/std) |>
  mutate(ave_z = mean(zscore)) |>
 # mutate(CIup = ifelse(n>1, (t.test(zscore))$conf.int[2], NA)) |>
 # mutate(CIdown = ifelse(n>1, (t.test(zscore))$conf.int[1], NA)) |>
  ungroup()

scores_lake <- scored_data_lakes|>
  select(site,eco_type,nutrient,param,network_position,upstream_network_lakes,position,WS_Group, nut_type, distancefromglacier_Km,ave_z) |>
  distinct()  |>
  mutate(WS_Group=factor(WS_Group, levels=c('GL5','GL4','GL3','ALB')))


## 4a. Plot ####
library(colorblindr)
# Get the full Okabe-Ito palette
library(scales)
show_col(colorblindr::palette_OkabeIto_black)
okabe_ito_colors <- palette_OkabeIto_black

ggplot(scores_lake, aes(distancefromglacier_Km, ave_z, color=position)) +
  geom_jitter(size=2) +
  facet_wrap(~nutrient, scales='free_y') +
  labs(x='Distance from glacier (km)', y='Average z-score of each parameter and site') +
  theme_bw() +
  scale_color_manual('',values=c(okabe_ito_colors[6], okabe_ito_colors[7])) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
  
  ggsave('Figures/zscore_insouts.png',width=8.5,height=4.5,units='in',dpi=1200)




# 5. Z-scores of lakes around their own population means ####

lake_means <- data |>
  filter(eco_type=='lake') |>
  group_by(param, site) |>
  summarise(mean=as.numeric(mean(result, na.rm=TRUE)),
            std=as.numeric(mean(result,na.rm=TRUE))) |>
  ungroup() 


scored_data_lakes <- data |>
  filter(eco_type=='lake')  |>
  left_join(lake_means) |>
  group_by(param, site,szn) |>
  add_count() |>
  mutate(zscore=(result-mean)/std) |>
  mutate(ave_z = mean(zscore)) |>
  # mutate(CIup = ifelse(n>1, (t.test(zscore))$conf.int[2], NA)) |>
  # mutate(CIdown = ifelse(n>1, (t.test(zscore))$conf.int[1], NA)) |>
  ungroup()

scores_lake <- scored_data_lakes|>
  select(site,eco_type,nutrient,param,network_position,upstream_network_lakes,position,WS_Group, nut_type, distancefromglacier_Km,ave_z, szn) |>
  distinct()  |>
  mutate(WS_Group=factor(WS_Group, levels=c('GL5','GL4','GL3','ALB')))


## 5a. Plot ####
library(colorblindr)
# Get the full Okabe-Ito palette
library(scales)
show_col(colorblindr::palette_OkabeIto_black)
okabe_ito_colors <- palette_OkabeIto_black

ggplot(scores_lake, aes(distancefromglacier_Km, ave_z, color=szn)) +
  geom_jitter(size=2) +
  facet_wrap(~nutrient, scales='free_y') +
  labs(x='Distance from glacier (km)', y='Average Z-score of each parameter and site') +
  theme_bw() +
  scale_color_manual('',values=c('blue4','goldenrod3','palegreen4')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
ggsave('Figures/zscore_lakes.png',width=8.5,height=4.5,units='in',dpi=1200)

# add shape for nutrient form discussion
ggplot(scores_lake, aes(distancefromglacier_Km, ave_z, color=szn, shape=nut_type)) +
  geom_jitter(size=2) +
  facet_wrap(~nutrient, scales='free_y') +
  labs(x='Distance from glacier (km)', y='Average Z-score of each parameter and site') +
  theme_bw() +
  scale_color_manual('',values=c('blue4','goldenrod3','palegreen4')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
ggsave('Figures/zscore_lakes_form.png',width=8.5,height=4.5,units='in',dpi=1200)
