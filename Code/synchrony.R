#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Synchrony along the network - script looks at Pearson's correlations 
# among all network positions 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

# prepare data ####
nuts.setup <- nuts |>
  mutate(year=year(date)) |>
  select(-site, -depth_m, -date) |>
  group_by(network_position, eco_type, season, year,param) |>
  summarise(med=median(result)) |> # get median value per year and season
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  #select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL) |>
  filter(eco_type != 'glacier') |>
  group_by(network_position) |>
  add_count()|>
  ungroup()



stoich.setup <- stoich |>
  mutate(year=year(date)) |>
  select(-site, -depth_m, -date) |>
  group_by(network_position, eco_type, season, year, param) |>
  summarise(med=median(result)) |> # get median value per year and season
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  #select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL) |>
  filter(eco_type != 'glacier') |>
  group_by(network_position) |>
  add_count()|>
  ungroup()


# Write function to pull corrleation values and p-values of corr into lists ####
synchrony_fun <- function(setup.dat, variable) {

corr_dat <- setup.dat |>
  select(1:4, variable) |>
  drop_na() |>
  group_by(network_position) |>
  add_count() |>
  ungroup() |>
  filter(n>3) |>
  unite(col = position, network_position, eco_type, sep = "_") |>
  select(-n) |>
  pivot_wider(id_cols = c('season', 'year'), 
              names_from = 'position', values_from = variable)  |>
  select(-season, - year) |>
  scale()
# 
# ggcorr(corr_dat, method=c('pairwise.complete.obs','pearson'))
# check <- as.data.frame(cor(corr_dat, use='pairwise')) |>
#   rownames_to_column() |>
#   mutate(param = quoVariable)


corr <- cor(corr_dat, use='pairwise')
p.mat <- cor_pmat(corr)
p.mat[is.na(corr)] <- corr[is.na(corr)]


ggcorrplot(corr,
           hc.order = FALSE, type = "upper",
           outline.color = "white", p.mat = p.mat, lab = TRUE,
           ggtheme=ggplot2::theme_dark())
ggsave(paste0('Figures/synchrony/', variable, '.png'), 
       height=4.25, width=6.25, units = 'in', dpi=1200)

return(list(corr, p.mat))

}

# Run synchrony function for nutrients ####
TN_synchrony <- synchrony_fun(nuts.setup, 'TN_umolL')
TP_synchrony <- synchrony_fun(nuts.setup, 'TP_umolL')
DON_synchrony <- synchrony_fun(nuts.setup, 'DON_umolL') 
DOP_synchrony <- synchrony_fun(nuts.setup, 'DOP_umolL')
DIN_synchrony <- synchrony_fun(nuts.setup, 'IN_umolL')
DIP_synchrony <- synchrony_fun(nuts.setup, 'IP_umolL') 
TDN_synchrony <- synchrony_fun(nuts.setup, 'TDN_umolL')
TDP_synchrony <- synchrony_fun(nuts.setup, 'TDP_umolL')
PN_synchrony <- synchrony_fun(nuts.setup, 'PN_umolL')
PP_synchrony <- synchrony_fun(nuts.setup, 'PP_umolL') 

# Run synchrony function for stoichiometry ####
TN_TP_synchrony <- synchrony_fun(stoich.setup, 'tn.tp')
TDN_TDP_synchrony <- synchrony_fun(stoich.setup, 'tdn.tdp')
DIN_DIP_synchrony <- synchrony_fun(stoich.setup, 'in.ip')
PN_PP_synchrony <- synchrony_fun(stoich.setup, 'pn.pp')
DON_DOP_synchrony <- synchrony_fun(stoich.setup, 'don.dop')


# Look at distance vs synchrony ####
# write function to compile correlation and distance data
distance_synchrony <- function(synchrony_data, param_name) {

sync <- round(synchrony_data[[1]], 3)
sync [lower.tri(sync, diag=TRUE)] <- NA
sync_dist <- as.data.frame(sync) |>
  #select(-1) |>
  rownames_to_column('site1')
sync_dist <- sync_dist |>
  pivot_longer(2:ncol(sync_dist), names_to = 'site2', values_to = 'correlation') |>
  drop_na()

sync_p <- round(synchrony_data[[2]], 3)
sync_p [lower.tri(sync_p, diag=TRUE)] <- NA
sync_p <- as.data.frame(sync_p) |>
  #select(-1) |>
  rownames_to_column('site1')
sync_p <- sync_p |>
  pivot_longer(2:ncol(sync_p), names_to = 'site2', values_to = 'p.value') |>
  drop_na()


sync_dist <- left_join(sync_dist, sync_p) |>
  left_join(distances_Km) |>
  mutate(param=param_name)
}

sync_dist_all <- distance_synchrony(DON_DOP_synchrony, 'DON:DOP') |>
  bind_rows(distance_synchrony(DON_synchrony, 'DON')) |>
  bind_rows(distance_synchrony(DOP_synchrony, 'DOP')) |>
  bind_rows(distance_synchrony(DIN_DIP_synchrony, 'DIN:DIP')) |>
  bind_rows(distance_synchrony(DIN_synchrony, 'DIN')) |>
  bind_rows(distance_synchrony(DIP_synchrony, 'DIP')) |>
  bind_rows(distance_synchrony(PN_PP_synchrony, 'PN:PP')) |>
  bind_rows(distance_synchrony(PN_synchrony, 'PN')) |>
  bind_rows(distance_synchrony(PP_synchrony, 'PP')) |>
  bind_rows(distance_synchrony(TDN_TDP_synchrony, 'TDN:TDP')) |>
  bind_rows(distance_synchrony(TDN_synchrony, 'TDN')) |>
  bind_rows(distance_synchrony(TDP_synchrony, 'TDP')) |>
  bind_rows(distance_synchrony(TN_TP_synchrony, 'TN:TP')) |>
  bind_rows(distance_synchrony(TN_synchrony, 'TN')) |>
  bind_rows(distance_synchrony(TP_synchrony, 'TP')) 

sync_dist_all$param <- factor(sync_dist_all$param, levels=c('DON:DOP','DON','DOP','DIN:DIP','DIN','DIP','PN:PP','PN','PP', 'TDN:TDP','TDN','TDP','TN:TP','TN','TP'))



ggplot(sync_dist_all, aes(distance_Km, correlation)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  facet_wrap(~param, ncol=3, scales='free') +
  theme_classic() +
  labs(x='Distance between sites (Km)', y='Correlation coefficient')
ggsave('Figures/synchrony/synchrony_distance_plot.png',height=6.5, width=8.5, units = 'in', dpi=1200)





# plot for class project
ggplot(sync_dist_all |> filter(param %in% c('TN:TP', 'TN', 'TP')), aes(distance_Km, correlation)) +
  geom_abline(slope=0, intercept=0, color = 'red4') +
  geom_point() +
  geom_smooth(se=FALSE) +
  facet_wrap(~param, ncol=3) +
  theme_bw() +
  labs(x='Distance between sites (Km)', y='Correlation coefficient')
