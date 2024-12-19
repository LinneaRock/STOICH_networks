# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 
library(colorblindr)
library(scales)



# 2. Format data ####
gams <- nuts |>
  mutate(nutrient = case_when(grepl('N', param)~'Nitrogen',
                              grepl('P', param)~'Phosphorus')) |>
  # rbind(stoich |>
  #         mutate(nutrient='Ratio')) |>
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
  # mutate(param = sub('_.*','',param),
  #        param=ifelse(param=='tn.tp', 'TN:TP',
  #                     ifelse(param=='tdn.tdp','TDN:TDP',
  #                            ifelse(param=='pn.pp','PN:PP',
  #                                   ifelse(param=='in.ip','IN:IP',
  #                                          ifelse(param=='don.dop','DON:DOP', param)))))) |>
  mutate(param = sub('_.*','',param),
         nut_type = case_when(param %in% c('TN','TP','TN:TP')~'total',
                              param %in% c('TDN','TDP','TDN:TDP')~'total dissolved',
                              param %in% c('PN','PP','PN:PP')~'particulate',
                              param %in% c('IN','IP','IN:IP')~'inorganic',
                              param %in% c('DON','DOP','DON:DOP')~'dissolved organic'),
         nut_type = factor(nut_type, levels = c('dissolved organic', 'inorganic', 'particulate', 'total dissolved', 'total')))

# Get the full Okabe-Ito palette
show_col(colorblindr::palette_OkabeIto_black)
okabe_ito_colors <- palette_OkabeIto_black


fraction_ratios <- gams |>
  select(site, eco_type, nutrient, network_position, upstream_network_lakes, distancefromglacier_Km, param, result, date, waterYear, szn) |>
  pivot_wider(names_from = 'param', values_from = 'result', values_fn = first) |>
  mutate(inorganicN = IN/TN,
         organicN = DON/TN,
         particulateN = PN/TN,
         dissolvedN = TDN/TN,
         inorganicP = IP/TP,
         organicP = DOP/TP,
         particulateP = PP/TP,
         dissolvedP = TDP/TP) |>
  select(-c(IN,DON,PN,TDN,TN,IP,DOP,PP,TDP,TP)) |>
  pivot_longer(cols=c(10:17), names_to = 'fraction', values_to = 'ratio_total') |>
  drop_na(ratio_total) |>
  mutate(nut_type = case_when(
                              fraction %in% c('dissolvedN','dissolvedP')~'total dissolved',
                              fraction %in% c('particulateN','particulateP')~'particulate',
                              fraction %in% c('inorganicN','inorganicP')~'inorganic',
                              fraction %in% c('organicN','organicP')~'dissolved organic'),
         nut_type = factor(nut_type, levels = c('dissolved organic', 'inorganic', 'particulate', 'total dissolved', 'total')))

ggplot(fraction_ratios) +
  geom_violin(aes(nut_type, ratio_total)) +
  geom_jitter(aes(nut_type, ratio_total, color=nut_type)) +
  facet_wrap(~nutrient*eco_type)

ggplot(fraction_ratios) +
  geom_point(aes(distancefromglacier_Km, ratio_total, color=nut_type, group=nut_type, shape=eco_type)) +
  facet_wrap(~nutrient) +
  labs(y='', x="Distance from glacier (km)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_color_manual('',values=c(okabe_ito_colors[1:5])) +
  scale_shape_manual('', values=c(21,22,23)) 

