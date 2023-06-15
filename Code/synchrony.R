#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Synchrony along the network - script looks at Pearson's correlations 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

# prepare data ####
nuts.setup <- nuts |>
  mutate(year=year(date)) |>
  select(-site, -depth_m, -date) |>
  group_by(network_position, eco_type, season, year,param) |>
  summarise(med=median(result)) |> # get median value per year and season at each site
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
  summarise(med=median(result)) |> # get median value per year and season at each site
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  #select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL) |>
  filter(eco_type != 'glacier') |>
  group_by(network_position) |>
  add_count()|>
  ungroup()


# Write function to pull correlation values and p-values of corr into lists ####
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
ggsave(paste0('Figures/synchrony/Network/', variable, '.png'), 
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
sync_p <- round(synchrony_data[[2]], 3)
sync_p [lower.tri(sync_p, diag=TRUE)] <- NA
sync_p <- as.data.frame(sync_p) |>
  #select(-1) |>
  rownames_to_column('site1')
sync_p <- sync_p |>
  pivot_longer(2:ncol(sync_p), names_to = 'site2', values_to = 'p.value') |>
  drop_na()

# combine data
sync_dist <- left_join(sync_dist, sync_p) |>
  left_join(distances_Km) |>
  mutate(param=param_name) |>
  mutate(distance_Km = ifelse(site1=='12_stream' & site2=='12a_lake', 
                              0.368, distance_Km)) |> # this distance is just listed in reverse order
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



ggplot(sync_dist_all, aes(distance_Km, correlation)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  facet_wrap(~param, ncol=3, scales='free') +
  theme_classic() +
  labs(x='Distance between sites (Km)', y='Correlation coefficient')
ggsave('Figures/synchrony/Network/synchrony_distance_plot.png',height=6.5, width=8.5, units = 'in', dpi=1200)

## average synchrony plots ####
# nitrogen
sync_dist_mean_N <- distance_synchrony(DON_synchrony, 'DON') |>
  bind_rows(distance_synchrony(DIN_synchrony, 'DIN')) |>
  bind_rows(distance_synchrony(PN_synchrony, 'PN')) |>
  bind_rows(distance_synchrony(TDN_synchrony, 'TDN')) |>
  bind_rows(distance_synchrony(TN_synchrony, 'TN')) |>
  group_by(site1,site2, distance_Km) |>
  summarise(meancorr = mean(correlation)) |>
  ungroup() 

sens.slope((sync_dist_mean_N |> arrange(distance_Km))$meancorr)
# pvalue = 0.0275
# slope = -0.02469231

ggplot(sync_dist_mean_N, aes(distance_Km, meancorr)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  theme_classic() +
  labs(x='Distance between sites (Km)', y='Average correlation coefficient for N')
ggsave('Figures/synchrony/Network/synchrony_distance_plot_meanN.png',height=6.5, width=8.5, units = 'in', dpi=1200)

# phosphorus
sync_dist_mean_P <- distance_synchrony(DOP_synchrony, 'DOP') |>
  bind_rows(distance_synchrony(DIP_synchrony, 'DIP')) |>
  bind_rows(distance_synchrony(PP_synchrony, 'PP')) |>
  bind_rows(distance_synchrony(TDP_synchrony, 'TDP')) |>
  bind_rows(distance_synchrony(TP_synchrony, 'TP')) |>
  group_by(site1,site2, distance_Km) |>
  summarise(meancorr = mean(correlation)) |>
  ungroup() 

sens.slope((sync_dist_mean_P |> arrange(distance_Km))$meancorr)
# pvalue = 0.9759


ggplot(sync_dist_mean_P, aes(distance_Km, meancorr)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  theme_classic() +
  labs(x='Distance between sites (Km)', y='Average correlation coefficient for P')
ggsave('Figures/synchrony/Network/synchrony_distance_plot_meanP.png',height=6.5, width=8.5, units = 'in', dpi=1200)

# stoichiometry
sync_dist_mean_stoich <- distance_synchrony(DON_DOP_synchrony, 'DON:DOP') |>
  bind_rows(distance_synchrony(DIN_DIP_synchrony, 'DIN:DIP')) |>
  bind_rows(distance_synchrony(PN_PP_synchrony, 'PN:PP')) |>
  bind_rows(distance_synchrony(TDN_TDP_synchrony, 'TDN:TDP')) |>
  bind_rows(distance_synchrony(TN_TP_synchrony, 'TN:TP')) |>
  group_by(site1,site2, distance_Km) |>
  summarise(meancorr = mean(correlation)) |>
  ungroup() 

sens.slope((sync_dist_mean_stoich |> arrange(distance_Km))$meancorr)
# pvalue = 0.5661


ggplot(sync_dist_mean_stoich, aes(distance_Km, meancorr)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  theme_classic() +
  labs(x='Distance between sites (Km)', y='Average correlation coefficient for N:P')
ggsave('Figures/synchrony/Network/synchrony_distance_plot_meanNP.png',height=6.5, width=8.5, units = 'in', dpi=1200)

###############################################################################
# LAKES ONLY ####
###############################################################################
# Write function to pull correlation values and p-values of corr into lists ####
synchrony_fun_lakes <- function(setup.dat, variable) {
  #tryCatch({ # open tryCatch to prevent errors from stopping loop
  corr_dat <- setup.dat |>
    filter(eco_type == 'lake') |>
    select(1:4, variable) |>
    drop_na() |>
    group_by(network_position) |>
    add_count() |>
    ungroup() |>
    filter(n>3) |>
    unite(col = position, network_position, eco_type, sep = "_") |>
    select(-n) |>
    pivot_wider(id_cols = c('season', 'year'), 
                names_from = 'position', values_from =variable)  |>
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
  ggsave(paste0('Figures/synchrony/Lakes/', variable, '.png'), 
         height=4.25, width=6.25, units = 'in', dpi=1200)
  
  return(list(corr, p.mat))
#}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
}

# Run synchrony function for nutrients ####
#TN_synchrony <- synchrony_fun_lakes(nuts.setup, 'TN_umolL') # not enough data
#TP_synchrony <- synchrony_fun_lakes(nuts.setup, 'TP_umolL') # not enough data
DON_synchrony <- synchrony_fun_lakes(nuts.setup, 'DON_umolL') 
DOP_synchrony <- synchrony_fun_lakes(nuts.setup, 'DOP_umolL')
DIN_synchrony <- synchrony_fun_lakes(nuts.setup, 'IN_umolL')
DIP_synchrony <- synchrony_fun_lakes(nuts.setup, 'IP_umolL') 
TDN_synchrony <- synchrony_fun_lakes(nuts.setup, 'TDN_umolL')
TDP_synchrony <- synchrony_fun_lakes(nuts.setup, 'TDP_umolL')
#PN_synchrony <- synchrony_fun_lakes(nuts.setup, 'PN_umolL') # not enough data
#PP_synchrony <- synchrony_fun_lakes(nuts.setup, 'PP_umolL') # not enough data

# Run synchrony function for stoichiometry ####
#TN_TP_synchrony <- synchrony_fun_lakes(stoich.setup, 'tn.tp') # not enough data
TDN_TDP_synchrony <- synchrony_fun_lakes(stoich.setup, 'tdn.tdp') 
DIN_DIP_synchrony <- synchrony_fun_lakes(stoich.setup, 'in.ip')
#PN_PP_synchrony <- synchrony_fun_lakes(stoich.setup, 'pn.pp') # not enough data
DON_DOP_synchrony <- synchrony_fun_lakes(stoich.setup, 'don.dop')


# Look at distance vs synchrony ####
# write function to compile correlation and distance data and run Mann-Kendall trend tests for synchrony along distance
distance_synchrony_lakes <- function(synchrony_data, param_name) {
  
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
  sync_p <- round(synchrony_data[[2]], 3)
  sync_p [lower.tri(sync_p, diag=TRUE)] <- NA
  sync_p <- as.data.frame(sync_p) |>
    #select(-1) |>
    rownames_to_column('site1')
  sync_p <- sync_p |>
    pivot_longer(2:ncol(sync_p), names_to = 'site2', values_to = 'p.value') |>
    drop_na()
  
  # combine data
  sync_dist <- left_join(sync_dist, sync_p) |>
    left_join(distances_Km) |>
    mutate(param=param_name) |>
    # run Mann-Kendall trend test of whether there is significant trend over distance or not
    arrange(distance_Km) |>
    mutate(z.stat = glance(mk.test(correlation))$statistic,
           p.MK = glance(mk.test(correlation))$p.value,
           n = glance(mk.test(correlation))$parameter,
           slope = as.numeric(sens.slope(correlation)[[1]]))
  
}

sync_dist_all_lakes <- distance_synchrony_lakes(DON_DOP_synchrony, 'DON:DOP') |>
  bind_rows(distance_synchrony_lakes(DON_synchrony, 'DON')) |>
  bind_rows(distance_synchrony_lakes(DOP_synchrony, 'DOP')) |>
  bind_rows(distance_synchrony_lakes(DIN_DIP_synchrony, 'DIN:DIP')) |>
  bind_rows(distance_synchrony_lakes(DIN_synchrony, 'DIN')) |>
  bind_rows(distance_synchrony_lakes(DIP_synchrony, 'DIP')) |>
  bind_rows(distance_synchrony_lakes(TDN_TDP_synchrony, 'TDN:TDP')) |>
  bind_rows(distance_synchrony_lakes(TDN_synchrony, 'TDN')) |>
  bind_rows(distance_synchrony_lakes(TDP_synchrony, 'TDP')) 

sync_dist_all_lakes$param <- factor(sync_dist_all_lakes$param, levels=c('DON:DOP','DON','DOP','DIN:DIP','DIN','DIP','TDN:TDP','TDN','TDP'))



ggplot(sync_dist_all_lakes, aes(distance_Km, correlation)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  facet_wrap(~param, ncol=3, scales='free') +
  theme_classic() +
  labs(x='Distance between sites (Km)', y='Correlation coefficient')
ggsave('Figures/synchrony/Lakes/synchrony_distance_plot.png',height=6.5, width=8.5, units = 'in', dpi=1200)





###############################################################################
# STREAMS ONLY ####
###############################################################################
# Write function to pull correlation values and p-values of corr into lists ####
synchrony_fun_streams <- function(setup.dat, variable) {
  #tryCatch({ # open tryCatch to prevent errors from stopping loop
  corr_dat <- setup.dat |>
    filter(eco_type == 'stream') |>
    select(1:4, variable) |>
    drop_na() |>
    group_by(network_position) |>
    add_count() |>
    ungroup() |>
    filter(n>3) |>
    unite(col = position, network_position, eco_type, sep = "_") |>
    select(-n) |>
    pivot_wider(id_cols = c('season', 'year'), 
                names_from = 'position', values_from =variable)  |>
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
  ggsave(paste0('Figures/synchrony/Streams/', variable, '.png'), 
         height=4.25, width=6.25, units = 'in', dpi=1200)
  
  return(list(corr, p.mat))
  #}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
}

# Run synchrony function for nutrients ####
TN_synchrony <- synchrony_fun_streams(nuts.setup, 'TN_umolL') 
TP_synchrony <- synchrony_fun_streams(nuts.setup, 'TP_umolL') 
DON_synchrony <- synchrony_fun_streams(nuts.setup, 'DON_umolL') 
DOP_synchrony <- synchrony_fun_streams(nuts.setup, 'DOP_umolL')
DIN_synchrony <- synchrony_fun_streams(nuts.setup, 'IN_umolL')
DIP_synchrony <- synchrony_fun_streams(nuts.setup, 'IP_umolL') 
TDN_synchrony <- synchrony_fun_streams(nuts.setup, 'TDN_umolL')
TDP_synchrony <- synchrony_fun_streams(nuts.setup, 'TDP_umolL')
PN_synchrony <- synchrony_fun_streams(nuts.setup, 'PN_umolL') 
PP_synchrony <- synchrony_fun_streams(nuts.setup, 'PP_umolL') 

# Run synchrony function for stoichiometry ####
TN_TP_synchrony <- synchrony_fun_streams(stoich.setup, 'tn.tp') 
TDN_TDP_synchrony <- synchrony_fun_streams(stoich.setup, 'tdn.tdp') 
DIN_DIP_synchrony <- synchrony_fun_streams(stoich.setup, 'in.ip')
PN_PP_synchrony <- synchrony_fun_streams(stoich.setup, 'pn.pp') 
DON_DOP_synchrony <- synchrony_fun_streams(stoich.setup, 'don.dop')


# Look at distance vs synchrony ####
# write function to compile correlation and distance data and run Mann-Kendall trend tests for synchrony along distance
distance_synchrony_streams <- function(synchrony_data, param_name) {
  
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
  sync_p <- round(synchrony_data[[2]], 3)
  sync_p [lower.tri(sync_p, diag=TRUE)] <- NA
  sync_p <- as.data.frame(sync_p) |>
    #select(-1) |>
    rownames_to_column('site1')
  sync_p <- sync_p |>
    pivot_longer(2:ncol(sync_p), names_to = 'site2', values_to = 'p.value') |>
    drop_na()
  
  # combine data
  sync_dist <- left_join(sync_dist, sync_p) |>
    left_join(distances_Km) |>
    mutate(param=param_name) |>
    # run Mann-Kendall trend test of whether there is significant trend over distance or not
    arrange(distance_Km) |>
    mutate(z.stat = glance(mk.test(correlation))$statistic,
           p.MK = glance(mk.test(correlation))$p.value,
           n = glance(mk.test(correlation))$parameter,
           slope = as.numeric(sens.slope(correlation)[[1]]))
  
}

sync_dist_all_streams <- distance_synchrony_streams(DON_DOP_synchrony, 'DON:DOP') |>
  bind_rows(distance_synchrony_streams(DON_synchrony, 'DON')) |>
  bind_rows(distance_synchrony_streams(DOP_synchrony, 'DOP')) |>
  bind_rows(distance_synchrony_streams(DIN_DIP_synchrony, 'DIN:DIP')) |>
  bind_rows(distance_synchrony_streams(DIN_synchrony, 'DIN')) |>
  bind_rows(distance_synchrony_streams(DIP_synchrony, 'DIP')) |>
  bind_rows(distance_synchrony_streams(PN_PP_synchrony, 'PN:PP')) |>
  bind_rows(distance_synchrony_streams(PN_synchrony, 'PN')) |>
  bind_rows(distance_synchrony_streams(PP_synchrony, 'PP')) |>
  bind_rows(distance_synchrony_streams(TDN_TDP_synchrony, 'TDN:TDP')) |>
  bind_rows(distance_synchrony_streams(TDN_synchrony, 'TDN')) |>
  bind_rows(distance_synchrony_streams(TDP_synchrony, 'TDP')) |>
  bind_rows(distance_synchrony_streams(TN_TP_synchrony, 'TN:TP')) |>
  bind_rows(distance_synchrony_streams(TN_synchrony, 'TN')) |>
  bind_rows(distance_synchrony_streams(TP_synchrony, 'TP')) 

sync_dist_all_streams$param <- factor(sync_dist_all_streams$param, levels=c('DON:DOP','DON','DOP','DIN:DIP','DIN','DIP','PN:PP','PN','PP', 'TDN:TDP','TDN','TDP','TN:TP','TN','TP'))



ggplot(sync_dist_all_streams, aes(distance_Km, correlation)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  facet_wrap(~param, ncol=3, scales='free') +
  theme_classic() +
  labs(x='Distance between sites (Km)', y='Correlation coefficient')
ggsave('Figures/synchrony/Streams/synchrony_distance_plot.png',height=6.5, width=8.5, units = 'in', dpi=1200)


###############################################################################
# SYNCHRONY OF VARIABLES ####
###############################################################################

## streams ####
corr_dat <- nuts.setup |>
  left_join(stoich.setup) |>
  filter(eco_type == 'stream') |>
  select(5:14, 16:20) |>
  scale()

corr <- cor(corr_dat, use='pairwise')
p.mat <- cor_pmat(corr)
p.mat[is.na(corr)] <- corr[is.na(corr)]


ggcorrplot(corr,
           hc.order = FALSE, type = "upper",
           outline.color = "white", p.mat = p.mat, lab = TRUE,
           ggtheme=ggplot2::theme_dark())
ggsave('Figures/synchrony/Streams/all_variates.png', 
       height=4.25, width=6.25, units = 'in', dpi=1200)


## lakes ####
corr_dat <- nuts.setup |>
  left_join(stoich.setup) |>
  filter(eco_type == 'lake') |>
  select(5:14, 16:20) |>
  scale()

corr <- cor(corr_dat, use='pairwise')
p.mat <- cor_pmat(corr)
p.mat[is.na(corr)] <- corr[is.na(corr)]


ggcorrplot(corr,
           hc.order = FALSE, type = "upper",
           outline.color = "white", p.mat = p.mat, lab = TRUE,
           ggtheme=ggplot2::theme_dark())
ggsave('Figures/synchrony/Lakes/all_variates.png', 
       height=4.25, width=6.25, units = 'in', dpi=1200)

