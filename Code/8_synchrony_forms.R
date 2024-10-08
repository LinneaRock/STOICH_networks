#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Synchrony among forms of N and forms of P  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 


# 2. prepare data ####
sync <- nuts |>
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
                                           ifelse(param=='don.dop','DON:DOP', param))))))


# 3. Spearman correlation of the nutrient timeseries ####
## Nitrogen ####
corr_dat <- sync |>
  filter(nutrient=='Nitrogen') |>
  arrange(date) |>
  pivot_wider(names_from = param, values_from = result) |>
  select(-c(1:13)) |>
  scale()

# Spearman's correlation with pairwise complete observations
cor_matrix_spearman <- cor(corr_dat,
                           method = "spearman",
                           use = "pairwise.complete.obs")

cor_matrix_spearman
ggcorrplot(cor_matrix_spearman, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggsave('Figures/synchrony/N_sync.png',width=6.5,height=4.5,units='in',dpi=1200)

## Phosphorus ####
corr_dat <- sync |>
  filter(nutrient=='Phosphorus') |>
  arrange(date) |>
  pivot_wider(names_from = param, values_from = result) |>
  select(-c(1:13)) |>
  scale()

# Spearman's correlation with pairwise complete observations
cor_matrix_spearman <- cor(corr_dat,
                           method = "spearman",
                           use = "pairwise.complete.obs")

cor_matrix_spearman
ggcorrplot(cor_matrix_spearman, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggsave('Figures/synchrony/P_sync.png',width=6.5,height=4.5,units='in',dpi=1200)

## Ratios ####
corr_dat <- sync |>
  filter(nutrient=='Ratio') |>
  arrange(date) |>
  pivot_wider(names_from = param, values_from = result) |>
  select(-c(1:13)) |>
  scale()

# Spearman's correlation with pairwise complete observations
cor_matrix_spearman <- cor(corr_dat,
                           method = "spearman",
                           use = "pairwise.complete.obs")

cor_matrix_spearman
ggcorrplot(cor_matrix_spearman, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggsave('Figures/synchrony/ratio_sync.png',width=6.5,height=4.5,units='in',dpi=1200)


