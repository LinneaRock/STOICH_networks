#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Synchrony along the network - using Spearman's correlations 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

# prepare data ####
nuts.setup <- nuts |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  distinct() |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  mutate(year=year(date)) |>
  select(-depth_m, -date) |>
 group_by(site, network_position, eco_type, season, year, mon, param) |>
 summarise(med=median(result, na.rm=TRUE)) |> # get median value per year and month at each site
 ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  #select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL) |>
 # filter(eco_type != 'glacier') |>
  group_by(network_position) |>
  add_count()|>
  ungroup() |>
  mutate(fakedate=as.Date(paste(year,mon,'01',sep='-')))


stoich.setup <- stoich |>
  mutate(year=year(date)) |>
  select(-depth_m, -date) |>
  group_by(site,network_position, eco_type, season, year, mon, param) |>
  summarise(med=median(result, na.rm=TRUE)) |> # get median value per year and season at each site
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  #select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL) |>
  #filter(eco_type != 'glacier') |>
  group_by(network_position) |>
  add_count()|>
  ungroup()





# Write function to pull correlation values and p-values of corr into lists ####
synchrony_fun <- function(setup.dat, variable) {

corr_dat <- setup.dat |>
  select(1:6, variable) |>
  # filter(nutrient=="Nitrogen") |>
 # pivot_wider(names_from = param, values_from = result) |>
# 
 group_by(network_position) |>
 add_count() |>
 ungroup() |>
 filter(n>4) |>
 #unite(col = position, network_position, eco_type, sep = "_") |>
 select(-n) |>
  drop_na() |>
  mutate(site=ifelse(site=='ARIKAREE','Arikaree_GLACIER', site)) |>
  select(-c(2:3)) |>
  pivot_wider(names_from = 'site', values_from = variable)  |>
  select(-c(1:3)) |>
  scale()
# 
ggcorr(corr_dat, method=c('pairwise.complete.obs','spearman'))
check <- as.data.frame(cor(corr_dat, use='pairwise')) |>
  rownames_to_column() |>
  mutate(param = variable)



corr <- cor(corr_dat, use='pairwise.complete.obs', method='spearman')
# p.mat <- cor_pmat(corr, method='spearman')
# p.mat[is.na(corr)] <- corr[is.na(corr)]


ggcorrplot(corr,
           hc.order = FALSE, type = "upper",
           outline.color = "white", #p.mat = p.mat,
           lab = TRUE,
           colors = c("#336a98", "white", "red4"),
           ggtheme=ggplot2::theme_classic)
ggsave(paste0('Figures/Network_synchrony/', variable, '.png'), 
       height=4.25, width=6.25, units = 'in', dpi=1200)

return(list(corr))

}


# # Pearson's correlation with pairwise complete observations
# cor_matrix_pearson <- cor(corr_dat, 
#                           method = "pearson", 
#                           use = "pairwise.complete.obs")
# 
# # Spearman's correlation with pairwise complete observations
# cor_matrix_spearman <- cor(corr_dat, 
#                            method = "spearman", 
#                            use = "pairwise.complete.obs")
# 
# # Compare the results -- Yes, they are indeed different.
# cor_matrix_pearson
# cor_matrix_spearman

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
# write function to compile correlation and distance data and run Mann-Kendall trend tests for synchrony along distance

distance_synchrony <- function(synchrony_data, param_name) {

# get correlation coefficients
sync <- round(synchrony_data[[1]], 3)
sync [lower.tri(sync, diag=TRUE)] <- NA
sync_dist <- as.data.frame(sync) |>
  #select(-1) |>
  rownames_to_column('site1')
sync_dist <- sync_dist |>
  pivot_longer(2:ncol(sync_dist), names_to = 'site2', values_to = 'correlation') |>
  drop_na()

# p-values of correlation stats 
# sync_p <- round(synchrony_data[[2]], 3)
# sync_p [lower.tri(sync_p, diag=TRUE)] <- NA
# sync_p <- as.data.frame(sync_p) |>
#   #select(-1) |>
#   rownames_to_column('site1')
# sync_p <- sync_p |>
#   pivot_longer(2:ncol(sync_p), names_to = 'site2', values_to = 'p.value') |>
#   drop_na()

distance_rev <- distances_Km |>
  rename(site1=site2,
         site2=site1) 

distanceallcombo <- distances_Km |>
  rbind(distance_rev)

# combine data
#sync_dist <- left_join(sync_dist, sync_p) |>
sync_dist <- sync_dist |>
  left_join(distanceallcombo) |>
  mutate(param=param_name) |>
  # mutate(distance_Km = ifelse(site1=='12_stream' & site2=='0_lake', 
  #                             0.368, distance_Km)) |> # this distance is just listed in reverse order
# run Mann-Kendall trend test of whether there is significant trend over distance or not
  arrange(distance_Km) |>
  mutate(z.stat = glance(mk.test(correlation))$statistic,
         p.MK = glance(mk.test(correlation))$p.value,
         n = glance(mk.test(correlation))$parameter,
         slope = as.numeric(sens.slope(correlation)[[1]]))

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



ggplot(sync_dist_all, aes(as.numeric(distance_Km), correlation, color=correlation)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  facet_wrap(~param, ncol=3, scales='free') +
  theme_classic() +
  scale_color_gradient2(low="#336a98", high='red4') +
  labs(x='Distance between sites (km)', y='Correlation coefficient')
ggsave('Figures/Network_synchrony/synchrony_distance_plot.png',height=6.5, width=8.5, units = 'in', dpi=1200)

## average synchrony plots ####
# nitrogen
sync_dist_mean_N <- distance_synchrony(DON_synchrony, 'DON') |>
  bind_rows(distance_synchrony(DIN_synchrony, 'DIN')) |>
  bind_rows(distance_synchrony(PN_synchrony, 'PN')) |>
  bind_rows(distance_synchrony(TDN_synchrony, 'TDN')) |>
  bind_rows(distance_synchrony(TN_synchrony, 'TN')) |>
  group_by(site1,site2, distance_Km) |>
  summarise(meancorr = mean(correlation)) |>
  ungroup() |>
  mutate(distance_Km = as.numeric(distance_Km)) |>
  mutate(nutrient='Nitrogen')

library(zyp)

senslopeN = as.numeric(zyp.sen(meancorr~distance_Km, sync_dist_mean_N)$coefficients[[2]])
interceptN = as.numeric(zyp.sen(meancorr~distance_Km, sync_dist_mean_N)$coefficients[[1]])
p.valueN = glance(mk.test((sync_dist_mean_N |> arrange(distance_Km))$meancorr))$p.value



ggplot(sync_dist_mean_N, aes(as.numeric(distance_Km), meancorr, color=meancorr)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  theme_classic() +
  geom_abline(slope=senslope, intercept=intercept) +
  scale_color_gradient2(~rho,low="#336a98", high='red4') +
  annotate('text', x=0.5, y=-0.75, label=paste0('p=',round(p.value, 5))) +
  labs(x='Distance between sites (km)', y='Average correlation coefficient for N')
#ggsave('Figures/Network_synchrony/synchrony_distance_plot_meanN.png',height=4.5, width=6.5, units = 'in', dpi=1200)

# phosphorus
sync_dist_mean_P <- distance_synchrony(DOP_synchrony, 'DOP') |>
  bind_rows(distance_synchrony(DIP_synchrony, 'DIP')) |>
  bind_rows(distance_synchrony(PP_synchrony, 'PP')) |>
  bind_rows(distance_synchrony(TDP_synchrony, 'TDP')) |>
  bind_rows(distance_synchrony(TP_synchrony, 'TP')) |>
  group_by(site1,site2, distance_Km) |>
  summarise(meancorr = mean(correlation)) |>
  ungroup() |>
  mutate(distance_Km = as.numeric(distance_Km)) |>
  mutate(nutrient='Phosphorus')


senslopeP = as.numeric(zyp.sen(meancorr~distance_Km, sync_dist_mean_P)$coefficients[[2]])
interceptP = as.numeric(zyp.sen(meancorr~distance_Km, sync_dist_mean_P)$coefficients[[1]])
p.valueP = glance(mk.test((sync_dist_mean_P |> arrange(distance_Km))$meancorr))$p.value


ggplot(sync_dist_mean_P, aes(as.numeric(distance_Km), meancorr, color=meancorr)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  theme_classic() +
  geom_abline(slope=senslope, intercept=intercept) +
  scale_color_gradient2(~rho,low="#336a98", high='red4') +
  annotate('text', x=0.5, y=-0.75, label=paste0('p=',round(p.value, 5))) +
  labs(x='Distance between sites (km)', y='Average correlation coefficient for P')
#ggsave('Figures/Network_synchrony/synchrony_distance_plot_meanP.png',height=4.5, width=6.5, units = 'in', dpi=1200)

# stoichiometry
sync_dist_mean_stoich <- distance_synchrony(DON_DOP_synchrony, 'DON:DOP') |>
  bind_rows(distance_synchrony(DIN_DIP_synchrony, 'DIN:DIP')) |>
  bind_rows(distance_synchrony(PN_PP_synchrony, 'PN:PP')) |>
  bind_rows(distance_synchrony(TDN_TDP_synchrony, 'TDN:TDP')) |>
  bind_rows(distance_synchrony(TN_TP_synchrony, 'TN:TP')) |>
  group_by(site1,site2, distance_Km) |>
  summarise(meancorr = mean(correlation)) |>
  ungroup() |>
  mutate(distance_Km = as.numeric(distance_Km)) |>
  mutate(nutrient = 'Molar ratio')


senslopeNP = as.numeric(zyp.sen(meancorr~distance_Km, sync_dist_mean_stoich)$coefficients[[2]])
interceptNP = as.numeric(zyp.sen(meancorr~distance_Km, sync_dist_mean_stoich)$coefficients[[1]])
p.valueNP = glance(mk.test((sync_dist_mean_stoich |> arrange(distance_Km))$meancorr))$p.value



ggplot(sync_dist_mean_stoich, aes(as.numeric(distance_Km), meancorr,color=meancorr)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  theme_classic() +
  geom_abline(slope=senslope, intercept=intercept) +
  scale_color_gradient2(~rho,low="#336a98", high='red4') +
  annotate('text', x=0.5, y=-0.75, label=paste0('p=',round(p.value, 5))) +
  labs(x='Distance between sites (km)', y='Average correlation coefficient for N:P')
#ggsave('Figures/Network_synchrony/synchrony_distance_plot_meanNP.png',height=4.5, width=6.5, units = 'in', dpi=1200)


all_sync <- sync_dist_mean_stoich |>
  rbind(sync_dist_mean_N) |>
  rbind(sync_dist_mean_P)

ggplot(all_sync, aes(distance_Km, meancorr, fill=meancorr, shape=nutrient)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  theme_classic() +
  scale_shape_manual('', values=c(23,24,25)) +
 # geom_abline(slope=senslope, intercept=intercept) +
  scale_fill_gradient2(~rho,low="#336a98", high='red4') +
#  annotate('text', x=0.5, y=-0.75, label=paste0('p=',round(p.value, 5))) +
  labs(x='Distance between sites (km)', y='Average correlation coefficient for N:P')


# ###############################################################################
# # LAKES ONLY ####
# ###############################################################################
# # Write function to pull correlation values and p-values of corr into lists ####
# synchrony_fun_lakes <- function(setup.dat, variable) {
#   #tryCatch({ # open tryCatch to prevent errors from stopping loop
#   corr_dat <- setup.dat |>
#     filter(eco_type == 'lake') |>
#     select(1:4, variable) |>
#     drop_na() |>
#     group_by(network_position) |>
#     add_count() |>
#     ungroup() |>
#     filter(n>3) |>
#     unite(col = position, network_position, eco_type, sep = "_") |>
#     select(-n) |>
#     pivot_wider(id_cols = c('season', 'year'), 
#                 names_from = 'position', values_from =variable)  |>
#     select(-season, - year) |>
#     scale()
#   # 
#   # ggcorr(corr_dat, method=c('pairwise.complete.obs','pearson'))
#   # check <- as.data.frame(cor(corr_dat, use='pairwise')) |>
#   #   rownames_to_column() |>
#   #   mutate(param = quoVariable)
#   
#   
#   corr <- cor(corr_dat, use='pairwise')
#   p.mat <- cor_pmat(corr)
#   p.mat[is.na(corr)] <- corr[is.na(corr)]
#   
#   
#   ggcorrplot(corr,
#              hc.order = FALSE, type = "upper",
#              outline.color = "white", p.mat = p.mat, lab = TRUE,
#              ggtheme=ggplot2::theme_dark())
#   ggsave(paste0('Figures/synchrony/Lakes/', variable, '.png'), 
#          height=4.25, width=6.25, units = 'in', dpi=1200)
#   
#   return(list(corr, p.mat))
# #}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
# }
# 
# # Run synchrony function for nutrients ####
# #TN_synchrony <- synchrony_fun_lakes(nuts.setup, 'TN_umolL') # not enough data
# #TP_synchrony <- synchrony_fun_lakes(nuts.setup, 'TP_umolL') # not enough data
# DON_synchrony <- synchrony_fun_lakes(nuts.setup, 'DON_umolL') 
# DOP_synchrony <- synchrony_fun_lakes(nuts.setup, 'DOP_umolL')
# DIN_synchrony <- synchrony_fun_lakes(nuts.setup, 'IN_umolL')
# DIP_synchrony <- synchrony_fun_lakes(nuts.setup, 'IP_umolL') 
# TDN_synchrony <- synchrony_fun_lakes(nuts.setup, 'TDN_umolL')
# TDP_synchrony <- synchrony_fun_lakes(nuts.setup, 'TDP_umolL')
# #PN_synchrony <- synchrony_fun_lakes(nuts.setup, 'PN_umolL') # not enough data
# #PP_synchrony <- synchrony_fun_lakes(nuts.setup, 'PP_umolL') # not enough data
# 
# # Run synchrony function for stoichiometry ####
# #TN_TP_synchrony <- synchrony_fun_lakes(stoich.setup, 'tn.tp') # not enough data
# TDN_TDP_synchrony <- synchrony_fun_lakes(stoich.setup, 'tdn.tdp') 
# DIN_DIP_synchrony <- synchrony_fun_lakes(stoich.setup, 'in.ip')
# #PN_PP_synchrony <- synchrony_fun_lakes(stoich.setup, 'pn.pp') # not enough data
# DON_DOP_synchrony <- synchrony_fun_lakes(stoich.setup, 'don.dop')
# 
# 
# # Look at distance vs synchrony ####
# # write function to compile correlation and distance data and run Mann-Kendall trend tests for synchrony along distance
# distance_synchrony_lakes <- function(synchrony_data, param_name) {
#   
#   # get correlation coefficients
#   sync <- round(synchrony_data[[1]], 3)
#   sync [lower.tri(sync, diag=TRUE)] <- NA
#   sync_dist <- as.data.frame(sync) |>
#     #select(-1) |>
#     rownames_to_column('site1')
#   sync_dist <- sync_dist |>
#     pivot_longer(2:ncol(sync_dist), names_to = 'site2', values_to = 'correlation') |>
#     drop_na()
#   
#   # p-values of correlation stats 
#   sync_p <- round(synchrony_data[[2]], 3)
#   sync_p [lower.tri(sync_p, diag=TRUE)] <- NA
#   sync_p <- as.data.frame(sync_p) |>
#     #select(-1) |>
#     rownames_to_column('site1')
#   sync_p <- sync_p |>
#     pivot_longer(2:ncol(sync_p), names_to = 'site2', values_to = 'p.value') |>
#     drop_na()
#   
#   # combine data
#   sync_dist <- left_join(sync_dist, sync_p) |>
#     left_join(distances_Km) |>
#     mutate(param=param_name) |>
#     # run Mann-Kendall trend test of whether there is significant trend over distance or not
#     arrange(distance_Km) |>
#     mutate(z.stat = glance(mk.test(correlation))$statistic,
#            p.MK = glance(mk.test(correlation))$p.value,
#            n = glance(mk.test(correlation))$parameter,
#            slope = as.numeric(sens.slope(correlation)[[1]]))
#   
# }
# 
# sync_dist_all_lakes <- distance_synchrony_lakes(DON_DOP_synchrony, 'DON:DOP') |>
#   bind_rows(distance_synchrony_lakes(DON_synchrony, 'DON')) |>
#   bind_rows(distance_synchrony_lakes(DOP_synchrony, 'DOP')) |>
#   bind_rows(distance_synchrony_lakes(DIN_DIP_synchrony, 'DIN:DIP')) |>
#   bind_rows(distance_synchrony_lakes(DIN_synchrony, 'DIN')) |>
#   bind_rows(distance_synchrony_lakes(DIP_synchrony, 'DIP')) |>
#   bind_rows(distance_synchrony_lakes(TDN_TDP_synchrony, 'TDN:TDP')) |>
#   bind_rows(distance_synchrony_lakes(TDN_synchrony, 'TDN')) |>
#   bind_rows(distance_synchrony_lakes(TDP_synchrony, 'TDP')) 
# 
# sync_dist_all_lakes$param <- factor(sync_dist_all_lakes$param, levels=c('DON:DOP','DON','DOP','DIN:DIP','DIN','DIP','TDN:TDP','TDN','TDP'))
# 
# 
# 
# ggplot(sync_dist_all_lakes, aes(distance_Km, correlation)) +
#   geom_point() +
#   #geom_smooth(se=FALSE) +
#   facet_wrap(~param, ncol=3, scales='free') +
#   theme_classic() +
#   labs(x='Distance between sites (Km)', y='Correlation coefficient')
# ggsave('Figures/synchrony/Lakes/synchrony_distance_plot.png',height=6.5, width=8.5, units = 'in', dpi=1200)
# 
# 
# 
# 
# 
# ###############################################################################
# # STREAMS ONLY ####
# ###############################################################################
# # Write function to pull correlation values and p-values of corr into lists ####
# synchrony_fun_streams <- function(setup.dat, variable) {
#   #tryCatch({ # open tryCatch to prevent errors from stopping loop
#   corr_dat <- setup.dat |>
#     filter(eco_type == 'stream') |>
#     select(1:4, variable) |>
#     drop_na() |>
#     group_by(network_position) |>
#     add_count() |>
#     ungroup() |>
#     filter(n>3) |>
#     unite(col = position, network_position, eco_type, sep = "_") |>
#     select(-n) |>
#     pivot_wider(id_cols = c('season', 'year'), 
#                 names_from = 'position', values_from =variable)  |>
#     select(-season, - year) |>
#     scale()
#   # 
#   # ggcorr(corr_dat, method=c('pairwise.complete.obs','pearson'))
#   # check <- as.data.frame(cor(corr_dat, use='pairwise')) |>
#   #   rownames_to_column() |>
#   #   mutate(param = quoVariable)
#   
#   
#   corr <- cor(corr_dat, use='pairwise')
#   p.mat <- cor_pmat(corr)
#   p.mat[is.na(corr)] <- corr[is.na(corr)]
#   
#   
#   ggcorrplot(corr,
#              hc.order = FALSE, type = "upper",
#              outline.color = "white", p.mat = p.mat, lab = TRUE,
#              ggtheme=ggplot2::theme_dark())
#   ggsave(paste0('Figures/synchrony/Streams/', variable, '.png'), 
#          height=4.25, width=6.25, units = 'in', dpi=1200)
#   
#   return(list(corr, p.mat))
#   #}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
# }
# 
# # Run synchrony function for nutrients ####
# TN_synchrony <- synchrony_fun_streams(nuts.setup, 'TN_umolL') 
# TP_synchrony <- synchrony_fun_streams(nuts.setup, 'TP_umolL') 
# DON_synchrony <- synchrony_fun_streams(nuts.setup, 'DON_umolL') 
# DOP_synchrony <- synchrony_fun_streams(nuts.setup, 'DOP_umolL')
# DIN_synchrony <- synchrony_fun_streams(nuts.setup, 'IN_umolL')
# DIP_synchrony <- synchrony_fun_streams(nuts.setup, 'IP_umolL') 
# TDN_synchrony <- synchrony_fun_streams(nuts.setup, 'TDN_umolL')
# TDP_synchrony <- synchrony_fun_streams(nuts.setup, 'TDP_umolL')
# PN_synchrony <- synchrony_fun_streams(nuts.setup, 'PN_umolL') 
# PP_synchrony <- synchrony_fun_streams(nuts.setup, 'PP_umolL') 
# 
# # Run synchrony function for stoichiometry ####
# TN_TP_synchrony <- synchrony_fun_streams(stoich.setup, 'tn.tp') 
# TDN_TDP_synchrony <- synchrony_fun_streams(stoich.setup, 'tdn.tdp') 
# DIN_DIP_synchrony <- synchrony_fun_streams(stoich.setup, 'in.ip')
# PN_PP_synchrony <- synchrony_fun_streams(stoich.setup, 'pn.pp') 
# DON_DOP_synchrony <- synchrony_fun_streams(stoich.setup, 'don.dop')
# 
# 
# # Look at distance vs synchrony ####
# # write function to compile correlation and distance data and run Mann-Kendall trend tests for synchrony along distance
# distance_synchrony_streams <- function(synchrony_data, param_name) {
#   
#   # get correlation coefficients
#   sync <- round(synchrony_data[[1]], 3)
#   sync [lower.tri(sync, diag=TRUE)] <- NA
#   sync_dist <- as.data.frame(sync) |>
#     #select(-1) |>
#     rownames_to_column('site1')
#   sync_dist <- sync_dist |>
#     pivot_longer(2:ncol(sync_dist), names_to = 'site2', values_to = 'correlation') |>
#     drop_na()
#   
#   # p-values of correlation stats 
#   sync_p <- round(synchrony_data[[2]], 3)
#   sync_p [lower.tri(sync_p, diag=TRUE)] <- NA
#   sync_p <- as.data.frame(sync_p) |>
#     #select(-1) |>
#     rownames_to_column('site1')
#   sync_p <- sync_p |>
#     pivot_longer(2:ncol(sync_p), names_to = 'site2', values_to = 'p.value') |>
#     drop_na()
#   
#   # combine data
#   sync_dist <- left_join(sync_dist, sync_p) |>
#     left_join(distances_Km) |>
#     mutate(param=param_name) |>
#     # run Mann-Kendall trend test of whether there is significant trend over distance or not
#     arrange(distance_Km) |>
#     mutate(z.stat = glance(mk.test(correlation))$statistic,
#            p.MK = glance(mk.test(correlation))$p.value,
#            n = glance(mk.test(correlation))$parameter,
#            slope = as.numeric(sens.slope(correlation)[[1]]))
#   
# }
# 
# sync_dist_all_streams <- distance_synchrony_streams(DON_DOP_synchrony, 'DON:DOP') |>
#   bind_rows(distance_synchrony_streams(DON_synchrony, 'DON')) |>
#   bind_rows(distance_synchrony_streams(DOP_synchrony, 'DOP')) |>
#   bind_rows(distance_synchrony_streams(DIN_DIP_synchrony, 'DIN:DIP')) |>
#   bind_rows(distance_synchrony_streams(DIN_synchrony, 'DIN')) |>
#   bind_rows(distance_synchrony_streams(DIP_synchrony, 'DIP')) |>
#   bind_rows(distance_synchrony_streams(PN_PP_synchrony, 'PN:PP')) |>
#   bind_rows(distance_synchrony_streams(PN_synchrony, 'PN')) |>
#   bind_rows(distance_synchrony_streams(PP_synchrony, 'PP')) |>
#   bind_rows(distance_synchrony_streams(TDN_TDP_synchrony, 'TDN:TDP')) |>
#   bind_rows(distance_synchrony_streams(TDN_synchrony, 'TDN')) |>
#   bind_rows(distance_synchrony_streams(TDP_synchrony, 'TDP')) |>
#   bind_rows(distance_synchrony_streams(TN_TP_synchrony, 'TN:TP')) |>
#   bind_rows(distance_synchrony_streams(TN_synchrony, 'TN')) |>
#   bind_rows(distance_synchrony_streams(TP_synchrony, 'TP')) 
# 
# sync_dist_all_streams$param <- factor(sync_dist_all_streams$param, levels=c('DON:DOP','DON','DOP','DIN:DIP','DIN','DIP','PN:PP','PN','PP', 'TDN:TDP','TDN','TDP','TN:TP','TN','TP'))
# 
# 
# 
# ggplot(sync_dist_all_streams, aes(distance_Km, correlation)) +
#   geom_point() +
#   #geom_smooth(se=FALSE) +
#   facet_wrap(~param, ncol=3, scales='free') +
#   theme_classic() +
#   labs(x='Distance between sites (Km)', y='Correlation coefficient')
# ggsave('Figures/synchrony/Streams/synchrony_distance_plot.png',height=6.5, width=8.5, units = 'in', dpi=1200)
# 
# 
# ###############################################################################
# # SYNCHRONY OF VARIABLES ####
# ###############################################################################
# 
# ## streams ####
# corr_dat <- nuts.setup |>
#   left_join(stoich.setup) |>
#   filter(eco_type == 'stream') |>
#   select(5:14, 16:20) |>
#   scale()
# 
# corr <- cor(corr_dat, use='pairwise')
# p.mat <- cor_pmat(corr)
# p.mat[is.na(corr)] <- corr[is.na(corr)]
# 
# 
# ggcorrplot(corr,
#            hc.order = FALSE, type = "upper",
#            outline.color = "white", p.mat = p.mat, lab = TRUE,
#            ggtheme=ggplot2::theme_dark())
# ggsave('Figures/synchrony/Streams/all_variates.png', 
#        height=4.25, width=6.25, units = 'in', dpi=1200)
# 
# 
# ## lakes ####
# corr_dat <- nuts.setup |>
#   left_join(stoich.setup) |>
#   filter(eco_type == 'lake') |>
#   select(5:14, 16:20) |>
#   scale()
# 
# corr <- cor(corr_dat, use='pairwise')
# p.mat <- cor_pmat(corr)
# p.mat[is.na(corr)] <- corr[is.na(corr)]
# 
# 
# ggcorrplot(corr,
#            hc.order = FALSE, type = "upper",
#            outline.color = "white", p.mat = p.mat, lab = TRUE,
#            ggtheme=ggplot2::theme_dark())
# ggsave('Figures/synchrony/Lakes/all_variates.png', 
#        height=4.25, width=6.25, units = 'in', dpi=1200)
# 
# #############################################
# 
