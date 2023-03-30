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
# approach: bootstrap result for lakes and streams 1000x for each parameter and run kde function
set.seed(1)
nsim=1000
sims <- data.frame()
bootstrap_nut_kde <- data.frame()

for(p in 1:length(nuts_param)) {
  
  for(i in 1:nsim) {
    
    tmp.a <- k_densities_NUTS |>
      filter(param == nuts_param[p],
             eco_type != 'glacier') # we only want the lakes and streams here
    
    N <- nrow(tmp.a)
    
    tmp <- sample(tmp.a$result, 1, replace=TRUE)
    
    tmp1 <- data.frame(tmp) |>
      rename(bootstrapped_result = tmp) |>
      mutate(param = nuts_param[p]) 
    
    sims <- rbind(sims, tmp1)
    
  }
  
  kde_fn <- kdensity((sims |> filter(param==nuts_param[p]))$bootstrapped_result, 
                     start='gumbel', kernel='gaussian')
  
  dens_df <- sims |>
    filter(param==nuts_param[p]) |>
    mutate(bs_kde = kde_fn(bootstrapped_result)) 
  
  bootstrap_nut_kde <- rbind(bootstrap_nut_kde, dens_df)
  
}


bootstrap_nut <- bootstrap_nut_kde |>
  group_by(param) |>
  mutate(SE = std.error(bs_kde),
         CI_lwr = confintr::ci_sd(bs_kde)[['interval']][1],
         CI_upr = confintr::ci_sd(bs_kde)[['interval']][2])





# D. Network stoichiometry distributions ####
# approach: bootstrap result for lakes and streams 1000x for each parameter and run kde function
set.seed(1)
nsim=1000
sims <- data.frame()
bootstrap_stoich_kde <- data.frame()

for(p in 1:length(stoich_param)) {
  
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
    
  }
  
  kde_fn <- kdensity((sims |> filter(param==stoich_param[p]))$bootstrapped_result, 
                     start='gumbel', kernel='gaussian')
  
  dens_df <- sims |>
    filter(param==stoich_param[p]) |>
    mutate(bs_kde = kde_fn(bootstrapped_result)) 
  
  bootstrap_stoich_kde <- rbind(bootstrap_stoich_kde, dens_df)
  
}


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
for(n in 1:length(nuts_param)) { # compare lakes vs stream nutrient concnetrations
  tmp.obj <- ks.test((k_densities_NUTS |> filter(param == nuts_param[n],
                                             eco_type == 'lake'))$result, 
                 (k_densities_NUTS |> filter(param == nuts_param[n],
                                             eco_type == 'stream'))$result)
  
  tmp <- data.frame(param = nuts_param[n], variables = 'lake-stream', d_stat = as.numeric(tmp.obj[['statistic']]), p_value = round(as.numeric(tmp.obj[['p.value']]),5))
  
  KS_tests <- rbind(KS_tests, tmp)
  
  for(s in 1:length(stoich_param)) { # compare lake vs stream stoichiometry
    
    tmp.obj2 <- ks.test((k_densities_STOICH |> filter(param == stoich_param[s],
                                                   eco_type == 'lake'))$result, 
                       (k_densities_STOICH |> filter(param == stoich_param[s],
                                                   eco_type == 'stream'))$result)
    
    tmp2 <- data.frame(param = stoich_param[s], variables = 'lake-stream', d_stat = as.numeric(tmp.obj2[['statistic']]), p_value = round(as.numeric(tmp.obj2[['p.value']]),5))
    
    KS_tests <- rbind(KS_tests, tmp2)
    
    
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

rm(list=c('tmp', 'tmp2', 'tmp.obj', 'tmp.obj3', 'e', 'n', 'nuts_eco', 'nuts_param', 'stoich_param'))

# G. Make nice plots of everything we are interested in ####
bootstrap_all <- rbind(bootstrap_nut, bootstrap_stoich) |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL'))

k_densities_all <- rbind(k_densities_NUTS, k_densities_STOICH) |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL'),
         eco_type != 'glacier')

## lake-stream-netwwork ####
# note, I am cutting the x-axis to be less than 500. This changes the appearance of IN:IP, which has stream results extending beyond 3000 and bootstrapped results extending to 2000; PN:PP, which has stream results extending to 600; TDN:TDP, which has stream results extending beyond 9000 and bootstrapped results extending beyond 3000; and TN:TP, which has stream results extending beyond 600
ggplot() +
  geom_ribbon(bootstrap_all |> filter(bootstrapped_result <500),
              mapping=aes(bootstrapped_result, bs_kde, ymin=bs_kde-CI_lwr,
                                         ymax=bs_kde+CI_upr),fill='grey80') +
  geom_line(k_densities_all |> filter(result < 500), 
            mapping=aes(result, total_type_density, linetype=eco_type)) +
  facet_wrap(.~param, scales='free') +
  theme_classic() +
  theme(legend.title = element_blank()) 

labeller = c(expression(phi*'(DON:DOP)'), expression(phi*'(DON'~mu*mol*L^-1*')'), expression(phi*'(DOP'~mu*mol*L^-1*')'), expression(phi*'(IN:IP)'), expression(phi*'(IN'~mu*mol*L^-1*')'), expression(phi*'(IP'~mu*mol*L^-1*')'), expression(phi*'(PN:PP)'), expression(phi*'(PN'~mu*mol*L^-1*')'), expression(phi*'(PP'~mu*mol*L^-1*')'), expression(phi*'(TDN:TDP)'), expression(phi*'(TDN'~mu*mol*L^-1*')'), expression(phi*'(TDP'~mu*mol*L^-1*')'),expression(phi*'(TN:TP)'), expression(phi*'(TN'~mu*mol*L^-1*')'), expression(phi*'(TP'~mu*mol*L^-1*')'))

