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
         CI_lwr = confintr::ci_sd(bs_kde)[['interval']][1],
         CI_upr = confintr::ci_sd(bs_kde)[['interval']][2])


rm(bootstrap_nut_kde)
rm(bootstrap_stoich_kde)
rm(dens_df)
rm(i)
rm(kde_fn)
rm(tmp)
rm(tmp1)
rm(tmp2)
rm(tmp.a)
rm(N)
rm(nsim)
rm(nuts_param)
rm(p)
rm(s)
rm(sims)
rm(stoich_param)


# E. Comparison: nutrients ####

ggplot() +
  geom_ribbon(bootstrap_nut, mapping=aes(bootstrapped_result, bs_kde, 
                                         ymin=bs_kde-CI_lwr, ymax=bs_kde+CI_upr), 
              fill='grey80') +
  geom_line(k_densities_NUTS |> filter(eco_type != 'glacier'), 
            mapping=aes(result, total_type_density, linetype=eco_type)) +
  facet_wrap(.~param, scales='free') +
  theme_classic()


# E. Comparison: stoichiometry ####

ggplot() +
  geom_ribbon(bootstrap_stoich, mapping=aes(bootstrapped_result, bs_kde, 
                                         ymin=bs_kde-CI_lwr, ymax=bs_kde+CI_upr), 
              fill='grey80') +
  geom_line(k_densities_STOICH |> filter(eco_type != 'glacier'), 
            mapping=aes(result, total_type_density, linetype=eco_type)) +
  facet_wrap(.~param, scales='free') +
  theme_classic() 
