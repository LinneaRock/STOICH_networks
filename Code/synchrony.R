#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Synchrony along the network - script looks at Pearson's correlations 
# among all network positions 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

# prepare data ####
nuts.setup <- nuts |>
  mutate(year=year(date)) |>
  select(-site, -depth_m, -date) |>
  group_by(network_position, eco_type, season, year,param) |>
  summarise(med=median(result)) |>
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL) |>
  filter(eco_type != 'glacier') |>
  group_by(network_position) |>
  add_count()|>
  ungroup()



synchrony_fun <- function(setup.dat, variable, quoVariable) {

corr_dat <- setup.dat |>
  select(1:4, variable) |>
  drop_na() |>
  group_by(network_position) |>
  add_count() |>
  ungroup() |>
  filter(n>2) |>
  unite(col = position, network_position, eco_type, sep = "_") |>
  select(-n) |>
  pivot_wider(id_cols = c('season', 'year'), 
              names_from = 'position', values_from = quoVariable)  |>
  select(-season, - year) |>
  scale()
# 
# ggcorr(corr_dat, method=c('pairwise.complete.obs','pearson'))
# check <- as.data.frame(cor(corr_dat, use='pairwise')) |>
#   rownames_to_column() |>
#   mutate(param = quoVariable)


corr <- cor(corr_dat, use='pairwise')
p.mat <-cor_pmat(corr)


ggcorrplot(corr,
           hc.order = FALSE, type = "lower",
           outline.color = "white", p.mat = p.mat, insig = "blank", lab = TRUE,
           ggtheme=ggplot2::theme_dark())
ggsave(paste0('Figures/synchrony/', quoVariable, '.png'), 
       height=4.25, width=6.25, units = 'in', dpi=1200)

return(c(corr, p.mat))

}


synchrony_fun(nuts.setup, nuts.setup$TN_umolL, 'TN_umolL')
