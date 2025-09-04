#~~~~~~~~~~~~~~~~~#
# Remove outliers #
#~~~~~~~~~~~~~~~~~#

library(tidyverse)



# nutrients ####
gl_nuts_raw <- read.csv('Data/gl_network_nuts_raw.csv') |> select(-X) |> mutate(date=as.Date(date)) |>
  pivot_longer(TN:IP, names_to = 'param', values_to = 'result') |>
  drop_na(result) |>
  filter(result>0) |>
  mutate(uid = paste(local_site, location, sep='_'))



nuts_param <- as.vector(unique(gl_nuts_raw$param))
site_n <- as.vector(unique(gl_nuts_raw$uid))
nuts_rm <- data.frame()
nuts_n_removed <- data.frame()



for(i in 1:length(nuts_param)) {
  for(j in 1:length(site_n)) {
    remove <-  boxplot((gl_nuts_raw |> filter(param==nuts_param[i],
                                       uid==site_n[j]))$result, 
                       plot = FALSE)$out
    
    if (length(remove) > 0) {
      tmp <- (gl_nuts_raw |> filter(param==nuts_param[i],
                             uid==site_n[j]))[-which((gl_nuts_raw |> filter(param==nuts_param[i],
                                                                      uid==site_n[j]))$result %in% remove), ]
    } else {
      tmp <- (gl_nuts_raw |> filter(param==nuts_param[i], uid==site_n[j]))
    }
    
    nuts_rm <- rbind(nuts_rm, tmp)
    
    tmpn = data.frame(length(remove)) |>
      mutate(var = nuts_param[i],
             uid = site_n[j])
    
    nuts_n_removed = rbind(nuts_n_removed, tmpn)
  }
}


write.csv(nuts_rm, 'Data/gl_network_nuts.csv')




# discharge ####
gl_discharge_raw <- read.csv('Data/gl_network_discharge_raw.csv') |> select(-X) |> 
  mutate(date=as.Date(date)) |> 
  drop_na(discharge_vol_cm)  |>
  mutate(uid = paste(local_site, location, sep='_'))


site_n <- as.vector(unique(gl_discharge_raw$uid))
discharge_rm <- data.frame()
discharge_n_removed <- data.frame()




  for(j in 1:length(site_n)) {
    remove <- boxplot((gl_discharge_raw |> filter(uid==site_n[j]))$discharge_vol_cm, 
                       plot = FALSE)$out
    
    if (length(remove) > 0) {
      tmp <- (gl_discharge_raw |> filter(uid==site_n[j]))[-which((gl_discharge_raw |> filter(uid==site_n[j]))$discharge_vol_cm %in% remove), ]
    } else {
      tmp <- (gl_discharge_raw |> filter(uid==site_n[j]))
    }
    
    discharge_rm <- rbind(discharge_rm, tmp)
    
    tmpn = data.frame(length(remove)) |>
      mutate(var = 'discharge_vol_cm',
             uid = site_n[j])
    
    discharge_n_removed = rbind(discharge_n_removed, tmpn)
  }


write.csv(discharge_rm, 'Data/gl_network_discharge.csv')


