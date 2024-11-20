#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Nutrient distribution comparisons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Non-parametric kernal density estimation ####
# https://cran.r-project.org/web/packages/kdensity/index.html

source('Data/CALL_DATA_PACKAGES.R')

# 1. Nutrients distributions ####
nuts_param <- as.vector(unique(nuts$param))
nuts_eco <- as.vector(c('lake', 'stream'))
inlets <- as.vector(unique((nuts |> filter(grepl('INLET', site)))$site))
outlets <- as.vector(unique((nuts |> filter(grepl('OUTLET', site)))$site))
k_densities <- data.frame()

## KDEs for ecotype ####
for(e in 1:length(nuts_eco)) {
  for(p in 1:length(nuts_param)) {
    tmp_f <- kdensity((nuts |> filter(param==nuts_param[p],
                                      eco_type==nuts_eco[e]))$result, start='gumbel', kernel='gaussian')
    
    dens_df <- nuts |>
      filter(param==nuts_param[p],
             eco_type==nuts_eco[e]) |>
      mutate(total_type_density = tmp_f(result))
    
    k_densities <- rbind(k_densities,dens_df)
  }
}



## inlet KDEs ####
k_densities_tmp <- data.frame() # create temporary dataframe

for(p in 1:length(nuts_param)) {
  tmp_f <- kdensity((nuts |> filter(param==nuts_param[p],
                                    site %in% inlets))$result, 
                    start='gumbel', kernel='gaussian')
  
  dens_df <- nuts |>
    filter(param==nuts_param[p],
           site %in% inlets) |>
    mutate(inlets_density = tmp_f(result))
  
  k_densities_tmp <- rbind(k_densities_tmp,dens_df)
}

# add input KDEs to dataframe
k_densities <- left_join(k_densities, k_densities_tmp)


## outlet KDEs ####
k_densities_tmp <- data.frame() # clear temporary dataframe

for(p in 1:length(nuts_param)) {
  tmp_f <- kdensity((nuts |> filter(param==nuts_param[p],
                                    site %in% outlets))$result, 
                    start='gumbel', kernel='gaussian')
  
  dens_df <- nuts |>
    filter(param==nuts_param[p],
           site %in% outlets) |>
    mutate(outlets_density = tmp_f(result))
  
  k_densities_tmp <- rbind(k_densities_tmp,dens_df)
}


## final dataset - add outlet KDEs ####
k_densities_NUTS <- left_join(k_densities, k_densities_tmp)




# 2. Stoichiometry distributions ####
stoich_param <- as.vector(unique(stoich$param))
stoich_eco <- as.vector(c('lake', 'stream'))
inlets <- as.vector(unique((stoich |> filter(grepl('INLET', site)))$site))
outlets <- as.vector(unique((stoich |> filter(grepl('OUTLET', site)))$site))
k_densities <- data.frame()


## KDEs for ecotype ####
for(e in 1:length(stoich_eco)) {
  for(p in 1:length(stoich_param)) {
    tmp_f <- kdensity((stoich |> filter(param==stoich_param[p],
                                        eco_type==stoich_eco[e]))$result, start='gumbel', kernel='gaussian')
    
    dens_df <- stoich |>
      filter(param==stoich_param[p],
             eco_type==stoich_eco[e]) |>
      mutate(total_type_density = tmp_f(result))
    
    k_densities <- rbind(k_densities,dens_df)
  }
}



## inlet KDEs  ####
k_densities_tmp <- data.frame() # create temporary dataframe

for(p in 1:length(stoich_param)) {
  tmp_f <- kdensity((stoich |> filter(param==stoich_param[p],
                                      site %in% inlets))$result, 
                    start='gumbel', kernel='gaussian')
  
  dens_df <- stoich |>
    filter(param==stoich_param[p],
           site %in% inlets) |>
    mutate(inlets_density = tmp_f(result))
  
  k_densities_tmp <- rbind(k_densities_tmp,dens_df)
}

# add input KDEs to dataframe
k_densities <- left_join(k_densities, k_densities_tmp)


## outlet KDEs ####
k_densities_tmp <- data.frame() # clear temporary dataframe

for(p in 1:length(stoich_param)) {
  tmp_f <- kdensity((stoich |> filter(param==stoich_param[p],
                                      site %in% outlets))$result, 
                    start='gumbel', kernel='gaussian')
  
  dens_df <- stoich |>
    filter(param==stoich_param[p],
           site %in% outlets) |>
    mutate(outlets_density = tmp_f(result))
  
  k_densities_tmp <- rbind(k_densities_tmp,dens_df)
}

## final dataset - add outlet KDEs ####
k_densities_STOICH <- left_join(k_densities, k_densities_tmp)


rm(dens_df, k_densities, k_densities_tmp, e, inlets, outlets, nuts_eco, p, stoich_eco, tmp_f)

# 3. Network nutrient distributions ####
# approach: bootstrap result for lakes and streams 500x each for each parameter, combine lake and stream bootstrapped data, and run kde function

## bootstrap and run kdes ####
set.seed(69420)
nsim=500
sims <- data.frame()
eco <- as.vector(c('lake', 'stream'))
bootstrap_nut_kde <- data.frame()


for(p in 1:length(nuts_param)) { # start parameter loop
  for(e in 1:length(eco)) { # start eco type loop
    for(i in 1:nsim) { # start bootstrap loop
      
      tmp.a <- k_densities_NUTS |>
        filter(param == nuts_param[p],
               eco_type == eco[e]) # we only want the lakes and streams here
      
      N <- nrow(tmp.a)
      
      tmp <- sample(tmp.a$result, 1, replace=TRUE)
      
      tmp1 <- data.frame(tmp) |>
        rename(bootstrapped_result = tmp) |>
        mutate(param = nuts_param[p]) 
      
      sims <- rbind(sims, tmp1)
      
    } # close bootstrap loop
  } # close eco type loop
  
  kde_fn <- kdensity((sims |> filter(param==nuts_param[p]))$bootstrapped_result, 
                     start='gumbel', kernel='gaussian')
  
  dens_df <- sims |>
    filter(param==nuts_param[p]) |>
    mutate(bs_kde = kde_fn(bootstrapped_result)) 
  
  bootstrap_nut_kde <- rbind(bootstrap_nut_kde, dens_df)
  
} # close parameter loop 


## get confidence interval for each parameter ####
bootstrap_nut <- bootstrap_nut_kde |>
  group_by(param) |>
  mutate(SE = std.error(bs_kde),
         CI_lwr = confintr::ci_sd(bs_kde)[['interval']][1],
         CI_upr = confintr::ci_sd(bs_kde)[['interval']][2])



# 4. Network stoichiometry distributions ####
# approach: bootstrap result for lakes and streams 500x each for each parameter, combine, and and run kde function

## bootstrap and run kdes ####
set.seed(69420)
nsim=500
sims <- data.frame()
bootstrap_stoich_kde <- data.frame()

for(p in 1:length(stoich_param)) { # start param loop
  for(e in 1:length(eco)) { # start eco type loop
    for(i in 1:nsim) {
      
      tmp.a <- k_densities_STOICH |>
        filter(param == stoich_param[p],
               eco_type != 'glacier') # we only want the lakes and streams here
      
      N <- nrow(tmp.a)
      
      tmp <- sample(tmp.a$result, 1, replace=TRUE)
      
      tmp1 <- data.frame(tmp) |>
        rename(bootstrapped_result = tmp) |>
        mutate(param = stoich_param[p]) 
      
      sims <- rbind(sims, tmp1)
      
    } # end bootstrap loop
  } # end eco type loop
  
  kde_fn <- kdensity((sims |> filter(param==stoich_param[p]))$bootstrapped_result, 
                     start='gumbel', kernel='gaussian')
  
  dens_df <- sims |>
    filter(param==stoich_param[p]) |>
    mutate(bs_kde = kde_fn(bootstrapped_result)) 
  
  bootstrap_stoich_kde <- rbind(bootstrap_stoich_kde, dens_df)
  
} # end param loop


# get confidence interval for each parameter
bootstrap_stoich <- bootstrap_stoich_kde |>
  group_by(param) |>
  mutate(SE = std.error(bs_kde),
         CI_lwr = ci_sd(bs_kde)[['interval']][1],
         CI_upr = ci_sd(bs_kde)[['interval']][2])


rm(bootstrap_nut_kde, bootstrap_stoich_kde, dens_df, i, kde_fn, tmp, tmp1, tmp.a, N, nsim, nuts_param, p, e, sims, stoich_param, eco)



# 5. Kolmogorov-Smirnov tests ####
# non-parametric way to test null hypothesis: distributions are the same, alternative hypothesis: distributions are significantly different 

# set up 
invsout_nuts <- k_densities_NUTS |>
  pivot_longer(cols = c(inlets_density, outlets_density), names_to = 'position', 
               values_to = 'position_dens') |>
  drop_na(position_dens)

invsout_stoich <- k_densities_STOICH |>
  pivot_longer(cols = c(inlets_density, outlets_density), names_to = 'position', 
               values_to = 'position_dens') |>
  drop_na(position_dens)

nuts_param <- as.vector(unique(nuts$param))
stoich_param <- as.vector(unique(stoich$param))
nuts_eco <- as.vector(c('lake', 'stream'))
KS_tests <- data.frame()


# the loop - wowwwzzzzaaaa
for(n in 1:length(nuts_param)) { 
  
  # compare lakes vs stream nutrient concentrations 
  tmp.obj <- ks.test((k_densities_NUTS |> filter(param == nuts_param[n],
                                                 eco_type == 'lake'))$result, 
                     (k_densities_NUTS |> filter(param == nuts_param[n],
                                                 eco_type == 'stream'))$result)
  
  tmp <- data.frame(param = nuts_param[n], variables = 'lake-stream', d_stat = as.numeric(tmp.obj[['statistic']]), p_value = round(as.numeric(tmp.obj[['p.value']]),5))
  
  KS_tests <- rbind(KS_tests, tmp)
  
  
  
  # inlet vs outlet nutrient concentrations
  inout.ks <- ks.test((invsout_nuts |> filter(param == nuts_param[n],
                                              position == 'inlets_density'))$result, 
                      (invsout_nuts |> filter(param == nuts_param[n],
                                              position == 'outlets_density'))$result)
  
  inout.tmp <- data.frame(param = nuts_param[n], variables = 'inlet-outlet', d_stat = as.numeric(inout.ks[['statistic']]), p_value = round(as.numeric(inout.ks[['p.value']]),5))
  
  KS_tests <- rbind(KS_tests, inout.tmp)
  
  
  
  # outlet vs lake nutrients
  outlake.ks <- ks.test((invsout_nuts |> filter(param == nuts_param[n],
                                                position == 'outlets_density'))$result,
                        (k_densities_NUTS |> filter(param == nuts_param[n],
                                                    eco_type == 'lake'))$result)
  
  outlake.tmp <- data.frame(param = nuts_param[n], variables = 'outlet-lake', d_stat = as.numeric(outlake.ks[['statistic']]), p_value = round(as.numeric(outlake.ks[['p.value']]),5))
  
  KS_tests <- rbind(KS_tests, outlake.tmp)
  
  
  # inlet vs lake nutrients
  inlake.ks <- ks.test((invsout_nuts |> filter(param == nuts_param[n],
                                                position == 'inlets_density'))$result,
                        (k_densities_NUTS |> filter(param == nuts_param[n],
                                                    eco_type == 'lake'))$result)
  
  inlake.tmp <- data.frame(param = nuts_param[n], variables = 'inlet-lake', d_stat = as.numeric(inlake.ks[['statistic']]), p_value = round(as.numeric(inlake.ks[['p.value']]),5))
  
  KS_tests <- rbind(KS_tests, inlake.tmp)


  
  
  
  for(s in 1:length(stoich_param)) { 
    # compare lake vs stream stoichiometry
    
    tmp.obj2 <- ks.test((k_densities_STOICH |> filter(param == stoich_param[s],
                                                      eco_type == 'lake'))$result, 
                        (k_densities_STOICH |> filter(param == stoich_param[s],
                                                      eco_type == 'stream'))$result)
    
    tmp2 <- data.frame(param = stoich_param[s], variables = 'lake-stream', d_stat = as.numeric(tmp.obj2[['statistic']]), p_value = round(as.numeric(tmp.obj2[['p.value']]),5))
    
    KS_tests <- rbind(KS_tests, tmp2)
    
    
    
    
    # inlet vs outlet stoichiometry
    inout.ks2 <- ks.test((invsout_stoich |> filter(param == stoich_param[s],
                                                   position == 'inlets_density'))$result, 
                         (invsout_stoich |> filter(param == stoich_param[s],
                                                   position == 'outlets_density'))$result)
    
    inout.tmp2 <- data.frame(param = stoich_param[s], variables = 'inlet-outlet', d_stat = as.numeric(inout.ks2[['statistic']]), p_value = round(as.numeric(inout.ks2[['p.value']]),5))
    
    KS_tests <- rbind(KS_tests, inout.tmp2)
    
    
    
    # outlet vs lake stoichiometry
    outlake.ks2 <- ks.test((invsout_stoich |> filter(param == stoich_param[s],
                                                     position == 'outlets_density'))$result,
                           (k_densities_STOICH |> filter(param == stoich_param[s],
                                                         eco_type == 'lake'))$result)
    
    outlake.tmp2 <- data.frame(param = stoich_param[s], variables = 'outlet-lake', d_stat = as.numeric(outlake.ks2[['statistic']]), p_value = round(as.numeric(outlake.ks2[['p.value']]),5))
    
    KS_tests <- rbind(KS_tests, outlake.tmp2)
    
    
    
    # inlet vs lake stoichiometry
    inlake.ks2 <- ks.test((invsout_stoich |> filter(param == stoich_param[s],
                                                     position == 'inlets_density'))$result,
                           (k_densities_STOICH |> filter(param == stoich_param[s],
                                                         eco_type == 'lake'))$result)
    
    inlake.tmp2 <- data.frame(param = stoich_param[s], variables = 'inlet-lake', d_stat = as.numeric(inlake.ks2[['statistic']]), p_value = round(as.numeric(inlake.ks2[['p.value']]),5))
    
    KS_tests <- rbind(KS_tests, inlake.tmp2)
    
    
    
    for(e in 1:length(nuts_eco)) { 
      # compare lake/stream vs network nutrients 
      tmp.obj3 <- ks.test((k_densities_NUTS |> filter(param == nuts_param[n],
                                                      eco_type == nuts_eco[e]))$result, 
                          (bootstrap_nut |> filter(param == nuts_param[n]))$bootstrapped_result)
      
      tmp3 <- data.frame(param = nuts_param[n], variables = paste0(nuts_eco[e], '-network'), d_stat = as.numeric(tmp.obj3[['statistic']]), p_value = round(as.numeric(tmp.obj3[['p.value']]),5))
      
      KS_tests <- rbind(KS_tests, tmp3)
      
      
      
      
      # compare lake/stream vs network stoichiometry
      
      tmp.obj4 <- ks.test((k_densities_STOICH |> filter(param == stoich_param[s],
                                                        eco_type == nuts_eco[e]))$result, 
                          (bootstrap_stoich |> filter(param == stoich_param[s]))$bootstrapped_result)
      
      tmp4 <- data.frame(param = stoich_param[s], variables = paste0(nuts_eco[e], '-network'), d_stat = as.numeric(tmp.obj4[['statistic']]), p_value = round(as.numeric(tmp.obj4[['p.value']]),5))
      
      KS_tests <- rbind(KS_tests, tmp4) |> distinct()
      
    } # end e loop
  } #end s loop
} # end n loop

rm(list=c('inout.ks', 'inout.ks2', 'inout.tmp', 'inout.tmp2', 'tmp', 'tmp2', 'tmp3', 'tmp4', 'tmp.obj', 'tmp.obj2', 'tmp.obj3', 'tmp.obj4', 'e', 'n', 'nuts_eco', 'nuts_param', 's', 'stoich_param', 'outlake.ks','outlake.ks2', 'outlake.tmp','outlake.tmp2','inlake.ks','inlake.ks2', 'inlake.tmp','inlake.tmp2'))

## make a nice dataframe of results and save it ####
KS_tests <- KS_tests |>
  mutate(significance = ifelse(between(p_value, 0.001, 0.05), '*',
                               ifelse(between(p_value, 0.0001, 0.001), '**',
                                      ifelse(p_value < 0.0001, '***', NA))))
write.csv(KS_tests, 'Data/Results/KS_test_results.csv')


# 6. Make a nice plot ####
## format the datasets ####
bootstrap_all <- rbind(bootstrap_nut, bootstrap_stoich) |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  left_join(KS_tests |>
              filter(variables %in% c('lake-network', 'stream-network')) |>
              dplyr::select(param, significance))

k_densities_all <- rbind(k_densities_NUTS, k_densities_STOICH) |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL'),
         eco_type != 'glacier') |>
  left_join(KS_tests |>
              filter(variables == 'lake-stream') |>
              dplyr::select(param, significance))

invsout_all <- rbind(invsout_nuts, invsout_stoich) |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  left_join(KS_tests |>
              filter(variables == 'inlet-outlet') |>
              dplyr::select(param, significance))

# this madness is just making pretty labels
bootstrap_all$param <- factor(bootstrap_all$param, labels = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')'))) 



k_densities_all$param <- factor(k_densities_all$param, labels = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')'))) 


invsout_all$param <- factor(invsout_all$param, labels = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')')))

## plotting ####

library(colorblindr)
# Get the full Okabe-Ito palette
library(scales)
show_col(colorblindr::palette_OkabeIto_black)
okabe_ito_colors <- palette_OkabeIto_black


ggplot() +
  geom_ribbon(bootstrap_all,
              mapping=aes(bootstrapped_result, bs_kde, ymin=bs_kde-CI_lwr,
                          ymax=bs_kde+CI_upr, fill='Network - 95% CI')) +
  geom_line(k_densities_all |> filter(eco_type=='lake'),#filter(result < 500), 
            mapping=aes(result, total_type_density, color=eco_type)) +
  geom_line(invsout_all, #|> filter(result < 500), 
            mapping=aes(result, position_dens, color=position)) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x='', y='') +
  scale_color_manual(values=c(okabe_ito_colors[6], okabe_ito_colors[8], okabe_ito_colors[7]), labels=c('inlets','lakes','outlets')) +
  scale_fill_manual(values='grey80') #+
  # scale_linetype_manual(values=c('dotted', 'solid', 'longdash'), labels = c('Inlets', 'Lakes', 'Outlets'))

ggsave('Figures/k_densities_all_colors.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)

