#~~~~~~~~~~~~~~~~~~~~~~~~~#
# Comparing distributions
#~~~~~~~~~~~~~~~~~~~~~~~~~#

# Non-parametric kernal density estimation ####
# https://cran.r-project.org/web/packages/kdensity/index.html
source('Data/CALL_DATA_PACKAGES.R') 


# A. Nutrients ####
## set-up ####

nuts_param <- as.vector(unique(nuts$param))
nuts_eco <- as.vector(c('lake', 'stream', 'glacier'))
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


# final dataset - add outlet KDEs 
k_densities_NUTS <- left_join(k_densities, k_densities_tmp)



# B. Stoichiometry ####
## set-up ####

stoich_param <- as.vector(unique(stoich$param))
stoich_eco <- as.vector(c('lake', 'stream', 'glacier'))
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

# final dataset - add outlet KDEs 
k_densities_STOICH <- left_join(k_densities, k_densities_tmp)


rm(dens_df)
rm(k_densities)
rm(k_densities_tmp)
rm(e)
rm(inlets)
rm(outlets)
rm(nuts_eco)
rm(p)
rm(stoich_eco)
rm(tmp_f)


# C. Network nutrient distributions ####
# approach: bootstrap result for lakes and streams 500x each for each parameter, combine lake and strema bootstrapped data, and run kde function
set.seed(1)
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


bootstrap_nut <- bootstrap_nut_kde |>
  group_by(param) |>
  mutate(SE = std.error(bs_kde),
         CI_lwr = confintr::ci_sd(bs_kde)[['interval']][1],
         CI_upr = confintr::ci_sd(bs_kde)[['interval']][2])





# D. Network stoichiometry distributions ####
# approach: bootstrap result for lakes and streams 500x each for each parameter, combine, and and run kde function
set.seed(1)
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


bootstrap_stoich <- bootstrap_stoich_kde |>
  group_by(param) |>
  mutate(SE = std.error(bs_kde),
         CI_lwr = ci_sd(bs_kde)[['interval']][1],
         CI_upr = ci_sd(bs_kde)[['interval']][2])


rm(bootstrap_nut_kde)
rm(bootstrap_stoich_kde)
rm(dens_df)
rm(i)
rm(kde_fn)
rm(tmp)
rm(tmp1)
rm(tmp.a)
rm(N)
rm(nsim)
rm(nuts_param)
rm(p)
rm(s)
rm(sims)
rm(stoich_param)
rm(eco)


# E. Comparison: nutrients ####
## plot lake vs stream distributions ####
ggplot() +
  geom_ribbon(bootstrap_nut |> filter(!param %in% c('NH4_ueqL','NO3_ueqL')), 
              mapping=aes(bootstrapped_result, bs_kde, ymin=bs_kde-CI_lwr, 
                          ymax=bs_kde+CI_upr),fill='grey80') +
  geom_line(k_densities_NUTS |> filter(eco_type != 'glacier') |> 
              filter(!param %in% c('NH4_ueqL','NO3_ueqL')), 
            mapping=aes(result, total_type_density, linetype=eco_type)) +
  facet_wrap(.~param, scales='free') +
  theme_classic()


## plot inlets vs outlets ####
invsout_nuts <- k_densities_NUTS |>
  pivot_longer(cols = c(inlets_density, outlets_density), names_to = 'position', 
               values_to = 'position_dens') |>
  drop_na(position_dens)

ggplot() +
  geom_line(invsout_nuts |> 
              filter(!param %in% c('NH4_ueqL','NO3_ueqL')), 
            mapping=aes(result, position_dens, linetype=position)) +
  facet_wrap(.~param, scales='free') +
  theme_classic()

# E. Comparison: stoichiometry ####
## plot lake vs stream distributions ####
ggplot() +
  geom_ribbon(bootstrap_stoich, mapping=aes(bootstrapped_result, bs_kde,
                                         ymin=bs_kde-CI_lwr, ymax=bs_kde+CI_upr),
              fill='grey80') +
  # geom_ribbon(bootstrap_stoich, mapping=aes(bootstrapped_result, bs_kde, 
  #                                           ymin=bs_kde-SE, ymax=bs_kde+SE), 
  #             fill='grey80') +
  geom_line(k_densities_STOICH |> filter(eco_type != 'glacier'), 
            mapping=aes(result, total_type_density, linetype=eco_type)) +
  facet_wrap(.~param, scales='free') +
  theme_classic() 

## plot inlets vs outlets ####
invsout_stoich <- k_densities_STOICH |>
  pivot_longer(cols = c(inlets_density, outlets_density), names_to = 'position', 
               values_to = 'position_dens') |>
  drop_na(position_dens)

ggplot() +
  geom_line(invsout_stoich |> filter(result <1000), 
            mapping=aes(result, position_dens, linetype=position)) +
  facet_wrap(.~param, scales='free') +
  theme_classic()

# F. Kolmogorov-Smirnov tests ####
# non-parametric way to test null hypothesis: distributions are the same, alternative hypothesis: distributions are significantly different 

# set up 
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

rm(list=c('inout.ks', 'inout.ks2', 'inout.tmp', 'inout.tmp2', 'tmp', 'tmp2', 'tmp3', 'tmp4', 'tmp.obj', 'tmp.obj2', 'tmp.obj3', 'tmp.obj4', 'e', 'n', 'nuts_eco', 'nuts_param', 's', 'stoich_param'))

KS_tests <- KS_tests |>
  mutate(significance = ifelse(between(p_value, 0.001, 0.05), '*',
                               ifelse(between(p_value, 0.0001, 0.001), '**',
                                      ifelse(p_value < 0.0001, '***', NA))))

write.csv(KS_tests, 'Data/KS_test_results.csv')

# G. Make nice plots of everything we are interested in ####
# format the datasets
bootstrap_all <- rbind(bootstrap_nut, bootstrap_stoich) |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  left_join(KS_tests |>
              filter(variables %in% c('lake-network', 'stream-network')) |>
              select(param, significance))

k_densities_all <- rbind(k_densities_NUTS, k_densities_STOICH) |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL'),
         eco_type != 'glacier') |>
  left_join(KS_tests |>
              filter(variables == 'lake-stream') |>
              select(param, significance))

invsout_all <- rbind(invsout_nuts, invsout_stoich) |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  left_join(KS_tests |>
              filter(variables == 'inlet-outlet') |>
              select(param, significance))



## lake-stream-netwwork ####
# this madness is just making pretty labels
bootstrap_all$param <- factor(bootstrap_all$param, labels = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')'))) 



k_densities_all$param <- factor(k_densities_all$param, labels = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')'))) 



# note, I am cutting the x-axis to be less than 500. This changes the appearance of IN:IP, which has stream results extending beyond 3000 and bootstrapped results extending to 2000; PN:PP, which has stream results extending to 600; TDN:TDP, which has stream results extending beyond 9000 and bootstrapped results extending beyond 3000; and TN:TP, which has stream results extending beyond 600
ggplot() +
  geom_ribbon(bootstrap_all |> filter(bootstrapped_result <500),
              mapping=aes(bootstrapped_result, bs_kde, ymin=bs_kde-CI_lwr,
                                         ymax=bs_kde+CI_upr, color='Network - 95% CI'),fill='grey80') +
  geom_line(k_densities_all |> filter(result < 500), 
            mapping=aes(result, total_type_density, linetype=eco_type)) +
  # geom_text(k_densities_all |> filter(result<500), mapping=aes(label=significance, x=min(result), y=total_type_density,), vjust=0.8, nudge_y=0.2) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x='', y='')  +
  scale_linetype_discrete(labels = c('Lakes', 'Streams')) +
  scale_color_manual(values='grey80')

ggsave('Figures/k_density_plots/k_density_lake-stream-network.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)


## inlets-outlets ####
# this madness is just making pretty labels
invsout_all$param <- factor(invsout_all$param, labels = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')')))


# note, I am cutting the x-axis to be less than 500. This changes the appearance of IN:IP, which has inlet data extending to 1000 and outlet to >3000; TDN:TDP, which has outlet results extending beyond 9000 and inlet results extending beyond 500; and TN:TP, which has outlet results extending beyond 600
ggplot() +
  geom_line(invsout_all |> filter(result < 500), 
            mapping=aes(result, position_dens, linetype=position)) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x='', y='') +
  scale_linetype_discrete(labels = c('Inlets', 'Outlets'))

ggsave('Figures/k_density_plots/k_density_inlet-outlet.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)



## outlets-lakes ####
outletlake <- k_densities_all |>
  select(-inlets_density) |>
  filter(eco_type=='lake' | 
           !is.na(outlets_density)) |>
  mutate(eco_type = ifelse(eco_type == 'stream', 'outlet', eco_type)) |>
  mutate(total_type_density = ifelse(eco_type != 'lake', NA, 
                                     total_type_density)) |>
  mutate(dens = ifelse(eco_type == 'lake', total_type_density, outlets_density))


# this madness is just making pretty labels
invsout_all$param <- factor(invsout_all$param, labels = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')')))


ggplot() +
  geom_line(outletlake |> filter(result < 500), 
            mapping=aes(result, dens, linetype=eco_type)) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x='', y='') +
  scale_linetype_discrete(labels = c('Lakes', 'Outlets'))

ggsave('Figures/k_density_plots/k_density_outlet-lake.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)

