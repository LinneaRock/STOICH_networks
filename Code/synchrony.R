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
  select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL) |>
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


# Write script to pull corrleation values and p-values of corr into lists ####
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
           hc.order = FALSE, type = "lower",
           outline.color = "white", p.mat = p.mat, insig = "blank", lab = TRUE,
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
