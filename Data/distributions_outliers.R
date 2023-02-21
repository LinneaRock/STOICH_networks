

# read in data
sites <- read.csv("Data/sites.csv") |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', '7','8','9','10','11','12','12a','13','14','15','16')))

gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))

# subset and format datasets for plotting ####
ions <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, 21:31) |>
  pivot_longer(7:17, names_to = 'param', values_to = 'result') |>
  drop_na(network_position) |>
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m)) |> # just look at photic zone
  group_by(site, network_position, eco_type, date, season, depth_m, param) |>
  summarise(result = mean(result))

nuts <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, 2:7, 10:14) |>
  pivot_longer(7:17, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)  |>
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m))  |> # just look at photic zone
  group_by(site, network_position, eco_type, date, season, depth_m, param) |>
  summarise(result = mean(result))

discharge_temp <- gl_network |>
  select(date, site, arik_flow_site, network_position, daily_dis_m3, mean_temp_C) |>
  mutate(site = ifelse(site == 'ARIKAREE', paste(site, arik_flow_site, sep = '_'), site),
         network_position = as.factor(network_position)) |>
  select(-arik_flow_site) |>
  drop_na(daily_dis_m3) |>
  pivot_longer(4:5, names_to = 'param', values_to = 'result') |>
  unique() |>
  drop_na()

#### frequency plots all data ####
ggplot(ions) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free') +
  labs(title = 'raw data')

ggplot(nuts) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free') +
  labs(title = 'raw data')


ggplot(discharge_temp) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'raw data')


#### loops to get rid of outliers ####
## we are determining outliers 

## loop through parameters 
ions_param <- as.vector(unique(ions$param))
nuts_param <- as.vector(unique(nuts$param))
discharge_temp_param <- as.vector(unique(discharge_temp$param))
site_n <- as.vector(unique(sites$site))
ions_rm <- data.frame()
ions_n_removed <- data.frame()
nuts_rm <- data.frame()
nuts_n_removed <- data.frame()
discharge_temp_rm <- data.frame()
discharge_temp_n_removed <- data.frame()

#i <- 1
for(i in 1:length(ions_param)) {
  for(j in 1:length(site_n)) {
    remove <-  boxplot((ions |> filter(param==ions_param[i],
                                       site==site_n[j]))$result, 
                       plot = FALSE)$out
    
    tmp <- (ions |> filter(param==ions_param[i],
                           site==site_n[j]))[-which((ions |> filter(param==ions_param[i],
                                                                      site==site_n[j]))$result %in% remove), ]
    
    ions_rm <- rbind(ions_rm, tmp)
    
    tmpn = data.frame(length(remove)) |>
      mutate(var = ions_param[i],
             site = site_n[j])
    
    ions_n_removed = rbind(ions_n_removed, tmpn)
  }
  }

for(i in 1:length(nuts_param)) {
  for(j in 1:length(site_n)) {
    remove <-  boxplot((nuts |> filter(param==nuts_param[i],
                                       site==site_n[j]))$result, 
                       plot = FALSE)$out
    
    tmp <- (nuts |> filter(param==nuts_param[i],
                           site==site_n[j]))[-which((nuts |> filter(param==nuts_param[i],
                                                                    site==site_n[j]))$result %in% remove), ]
    
    nuts_rm <- rbind(nuts_rm, tmp)
    
    tmpn = data.frame(length(remove)) |>
      mutate(var = nuts_param[i],
             site = site_n[j])
    
    nuts_n_removed = rbind(nuts_n_removed, tmpn)
  }
}

for(i in 1:length(discharge_temp_param)) {
  for(j in 1:length(site_n)) {
    remove <-  boxplot((discharge_temp |> filter(param==discharge_temp_param[i],
                                       site==site_n[j]))$result, 
                       plot = FALSE)$out
    
    tmp <- (discharge_temp |> filter(param==discharge_temp_param[i],
                           site==site_n[j]))[-which((discharge_temp |> filter(param==discharge_temp_param[i],
                                                                    site==site_n[j]))$result %in% remove), ]
    
    discharge_temp_rm <- rbind(discharge_temp_rm, tmp)
    
    tmpn = data.frame(length(remove)) |>
      mutate(var = discharge_temp_param[i],
             site = site_n[j])
    
    discharge_temp_n_removed = rbind(discharge_temp_n_removed, tmpn)
  }
}
# for(i in 1:length(ions_param)) {
#   
#   remove <-  boxplot((ions |> filter(param==ions_param[i]))$result, 
#                      plot = FALSE)$out
#   
#   tmp <- (ions |> filter(param==ions_param[i]))[-which((ions |> filter(param==ions_param[i]))$result %in% remove), ]
#   
#   ions_rm <- rbind(ions_rm, tmp)
#   
#   tmpn = data.frame(length(remove)) |>
#     mutate(var = ions_param[i])
#   
#   ions_n_removed = rbind(ions_n_removed, tmpn)
#   
# }
# 
# for(i in 1:length(nuts_param)) {
#   
#   remove <-  boxplot((nuts |> filter(param==nuts_param[i]))$result, 
#                      plot = FALSE)$out
#   
#   tmp <- (nuts |> filter(param==nuts_param[i]))[-which((nuts |> filter(param==nuts_param[i]))$result %in% remove), ]
#   
#   nuts_rm <- rbind(nuts_rm, tmp)
#   
#   tmpn = data.frame(length(remove)) |>
#     mutate(var = nuts_param[i])
#   
#   nuts_n_removed = rbind(nuts_n_removed, tmpn)
#   
# }
# 
# 
# for(i in 1:length(discharge_temp_param)) {
#   
#   remove <-  boxplot((discharge_temp |> filter(param==discharge_temp_param[i]))$result, 
#                      plot = FALSE)$out
#   
#   tmp <- (discharge_temp |> filter(param==discharge_temp_param[i]))[-which((discharge_temp |> filter(param==discharge_temp_param[i]))$result %in% remove), ]
#   
#   discharge_temp_rm <- rbind(discharge_temp_rm, tmp)
#   
#   tmpn = data.frame(length(remove)) |>
#     mutate(var = discharge_temp_param[i])
#   
#   discharge_temp_n_removed = rbind(discharge_temp_n_removed, tmpn)
#   
# }


#### writing the final datasets and deleting temporary ones ####
ions <- ions_rm
nuts <- nuts_rm
discharge <- discharge_temp_rm

N_outliers <- rbind(ions_n_removed, nuts_n_removed, discharge_temp_n_removed) |>
  group_by(var) |>
  mutate(count = sum(length.remove.))

rm(gl_network)
rm(sites)
rm(tmp)
rm(tmpn)
rm(ions_rm)
rm(nuts_rm)
rm(i)
rm(ions_param)
rm(nuts_param)
rm(remove)
rm(discharge_temp_param)
rm(discharge_temp_rm)
rm(ions_n_removed)
rm(nuts_n_removed)
rm(discharge_temp_n_removed)
rm(j)
rm(site_n)
rm(discharge_temp)

#### creating the stoich dataset ####
stoich <- nuts |>
  pivot_wider(id_cols = 1:6, names_from = 'param', values_from = 'result') |>
  mutate(tn.tp = TN_umolL/TP_umolL,
         don.dop = DON_umolL/DOP_umolL,
         tdn.tdp = TDN_umolL/TDP_umolL,
         pn.pp = PN_umolL/PP_umolL,
         in.ip = IN_umolL/IP_umolL) |>
  select(1:6, 18:22) |>
  pivot_longer(7:11, names_to = 'param', values_to = 'result') |>
  drop_na(result) |>
  filter(is.finite(result))



#### frequency plots outlier removed data ####
ggplot(ions) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free') +
  labs(title = 'outliers removed')


ggplot(nuts) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'outliers removed')

ggplot(discharge_temp) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free') +
  labs(title = 'outliers removed')# question, where did temps go? 

ggplot(stoich) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'outliers removed') # question, do I need to delete outliers from the stoich dataset too? 
