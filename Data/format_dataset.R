library(tidyverse)
library(lubridate)

lakes <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.10.3/glvwatsolu.dm.data.csv")

albdis <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.102.17/albdisch.nc.data.csv")
albwq <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.103.14/albisolu.nc.data.csv")

arikwq <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.104.13/ariksolu.nc.data.csv")
navdis <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.169.1/navdisch.nc.data.csv")

gl4dis <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.105.16/gl4disch.nc.data.csv")
gl4wq <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.108.12/gre4solu.nc.data.csv")

gl5wq <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.109.12/gre5solu.nc.data.csv")
gl5dis <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.170.1/gl5disch.nc.data.csv") 

albinwq <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.110.7/inlesolu.nc.data.csv")
flumewq <- read.csv("C:/Users/linne/OneDrive - University of Wyoming/Data/Niwot_Data/knb-lter-nwt.162.1/flumesolu.nc.data.csv")


lakes_format <- lakes |>
  dplyr::select(-LTER_site) |>
  mutate(site = paste0(local_site, "_", location)) |>
  rename(depth_m = depth) |>
  mutate(year = as.factor(year),
         date = as.Date(date)) |>
  #convert all parameters to as.numeric. This will create a lot of NAs where we had "<," but these are below the analytical detection so we should not use them anyway
  mutate(across(7:43, as.numeric)) |>
  dplyr::select(-Trit, -time, -D_excess, -dD_sdev, -dDeut, -d18O_sdev, -d18O, -T_sdev, -POC, -local_site, -location, -cat_sum, -an_sum, -chg_bal) |>
  relocate(site, depth_m) |>
  rename(SpC_uscm = conduct,
         ANC_ueqL = ANC,
         H_ueqL = H.,
         NH4_ueqL = NH4.,
         Ca_ueqL = Ca..,
         Mg_ueqL = Mg..,
         Na_ueqL = Na.,
         K_ueqL = K.,
         Cl_ueqL = Cl.,
         NO3_ueqL = NO3.,
         SO4_ueqL = SO4..,
         PO4_ueqL = PO4...,
         Si_umolL = Si,
         TN_umolL = TN,
         TDN_umolL = TDN,
         PN_umolL = PN,
         DON_umolL = DON,
         IN_ueqL = IN,
         TP_umolL = TP,
         TDP_umolL = TDP,
         PP_umolL = PP,
         DOP_umolL = DOP,
         IP_umolL = IP,
         TOC_mgL = TOC,
         DOC_mgL = DOC) |>
  relocate(c(TOC_mgL, DOC_mgL, TN_umolL, TDN_umolL, PN_umolL, IN_ueqL, DON_umolL, NH4_ueqL, NO3_ueqL, TP_umolL,TDP_umolL, PP_umolL, IP_umolL, DOP_umolL, PO4_ueqL))
  # get rid of rows without any nutrient data
lakes_format <- lakes_format |>
  mutate(check = rowSums(lakes_format[,c(1:15)], na.rm = TRUE)) |>
  filter(check != 0) |>
  dplyr::select(-check)

 
alb_camp_format <- left_join(albdis, albwq, by = "date") |>
  rename(daily_dis_m3 = discharge,
         mean_temp_C = temperature) |>
  mutate(site = "ALB_CAMP",
         date = as.Date(date),
         year = year(date),
         year = as.factor(year),
         depth_m = 0) |>
  relocate(site) |>
  dplyr::select(-notes, -LTER_site.x, -local_site.x, -LTER_site.y, -local_site.y, -time, - acid, - alkal, -Trit, -time, -D_excess, -dD_sdev, -dDeut, -d18O_sdev, -d18O, -T_sdev, -POC, -cat_sum, -an_sum, -chg_bal) |>
  mutate(across(c(3,4,6:31), as.numeric)) |>
  mutate(mean_temp_C = ifelse(is.nan(mean_temp_C), NA, mean_temp_C)) |>
  rename(SpC_uscm = cond,
         ANC_ueqL = ANC,
         H_ueqL = H.,
         NH4_ueqL = NH4.,
         Ca_ueqL = Ca..,
         Mg_ueqL = Mg..,
         Na_ueqL = Na.,
         K_ueqL = K.,
         Cl_ueqL = Cl.,
         NO3_ueqL = NO3.,
         SO4_ueqL = SO4..,
         PO4_ueqL = PO4...,
         Si_umolL = Si,
         TN_umolL = TN,
         TDN_umolL = TDN,
         PN_umolL = PN,
         DON_umolL = DON,
         IN_ueqL = IN,
         TP_umolL = TP,
         TDP_umolL = TDP,
         PP_umolL = PP,
         DOP_umolL = DOP,
         IP_umolL = IP,
         TOC_mgL = TOC,
         DOC_mgL = DOC) |>
  relocate(c(TOC_mgL, DOC_mgL, TN_umolL, TDN_umolL, PN_umolL, IN_ueqL, DON_umolL, NH4_ueqL, NO3_ueqL, TP_umolL,TDP_umolL, PP_umolL, IP_umolL, DOP_umolL, PO4_ueqL)) 
  # get rid of rows without any nutrient data
alb_camp_format <- alb_camp_format |>
  mutate(check = rowSums(alb_camp_format[,c(1:15)], na.rm = TRUE)) |>
  filter(check != 0) |>
  dplyr::select(-check)

nav_format <- navdis |>
  rename(arik_flow_site = local_site,
         daily_dis_m3 = discharge) |>
  mutate(date = as.Date(date)) |>
  dplyr::select(-LTER_site, -notes)

arik_format <- left_join(nav_format, arikwq |> mutate(date = as.Date(date)), by = "date") |>
  rename(site = local_site) |>
  relocate(site) |>
  mutate(year = year(date),
         year = as.factor(year)) |>
  dplyr::select(-LTER_site, -time, - acid, - alkal, -Trit, -time, -D_excess, -dD_sdev, -dDeut, -d18O_sdev, -d18O, -T_sdev, -POC, -cat_sum, -an_sum, -chg_bal) |>
  mutate(across(c(4,6:31), as.numeric)) |>
  rename(SpC_uscm = cond,
         ANC_ueqL = ANC,
         H_ueqL = H.,
         NH4_ueqL = NH4.,
         Ca_ueqL = Ca..,
         Mg_ueqL = Mg..,
         Na_ueqL = Na.,
         K_ueqL = K.,
         Cl_ueqL = Cl.,
         NO3_ueqL = NO3.,
         SO4_ueqL = SO4..,
         PO4_ueqL = PO4...,
         Si_umolL = Si,
         TN_umolL = TN,
         TDN_umolL = TDN,
         PN_umolL = PN,
         DON_umolL = DON,
         IN_ueqL = IN,
         TP_umolL = TP,
         TDP_umolL = TDP,
         PP_umolL = PP,
         DOP_umolL = DOP,
         IP_umolL = IP,
         TOC_mgL = TOC,
         DOC_mgL = DOC) |>
  relocate(c(TOC_mgL, DOC_mgL, TN_umolL, TDN_umolL, PN_umolL, IN_ueqL, DON_umolL, NH4_ueqL, NO3_ueqL, TP_umolL,TDP_umolL, PP_umolL, IP_umolL, DOP_umolL, PO4_ueqL))
  # get rid of rows without any nutrient data
arik_format <- arik_format |>
  mutate(check = rowSums(arik_format[,c(1:15)], na.rm = TRUE)) |>
  filter(check != 0) |>
  dplyr::select(-check)



gl4_format <- left_join(gl4dis, gl4wq, by = "date") |>
  rename(daily_dis_m3 = discharge,
         mean_temp_C = temperature) |>
  mutate(site = "GL4_OUTLET",
         date = as.Date(date),
         year = year(date),
         year = as.factor(year),
         depth_m = 0) |>
  relocate(site) |>
  dplyr::select(-notes, -LTER_site.x, -local_site.x, -LTER_site.y, -local_site.y, -time, - acid, - alkal, -Trit, -time, -D_excess, -dD_sdev, -dDeut, -d18O_sdev, -d18O, -T_sdev, -POC, -cat_sum, -an_sum, -chg_bal) |>
  mutate(across(c(3,4,6:31), as.numeric)) |>
  mutate(mean_temp_C = ifelse(is.nan(mean_temp_C), NA, mean_temp_C)) |>
  rename(SpC_uscm = cond,
         ANC_ueqL = ANC,
         H_ueqL = H.,
         NH4_ueqL = NH4.,
         Ca_ueqL = Ca..,
         Mg_ueqL = Mg..,
         Na_ueqL = Na.,
         K_ueqL = K.,
         Cl_ueqL = Cl.,
         NO3_ueqL = NO3.,
         SO4_ueqL = SO4..,
         PO4_ueqL = PO4...,
         Si_umolL = Si,
         TN_umolL = TN,
         TDN_umolL = TDN,
         PN_umolL = PN,
         DON_umolL = DON,
         IN_ueqL = IN,
         TP_umolL = TP,
         TDP_umolL = TDP,
         PP_umolL = PP,
         DOP_umolL = DOP,
         IP_umolL = IP,
         TOC_mgL = TOC,
         DOC_mgL = DOC) |>
  relocate(c(TOC_mgL, DOC_mgL, TN_umolL, TDN_umolL, PN_umolL, IN_ueqL, DON_umolL, NH4_ueqL, NO3_ueqL, TP_umolL,TDP_umolL, PP_umolL, IP_umolL, DOP_umolL, PO4_ueqL)) 
  # get rid of rows without any nutrient data
gl4_format <- gl4_format |>
  mutate(check = rowSums(gl4_format[,c(1:15)], na.rm = TRUE)) |>
  filter(check != 0) |>
  dplyr::select(-check)


gl5_format <- left_join(gl5dis, gl5wq, by = "date") |>
  rename(daily_dis_m3 = discharge) |>
         #mean_temp_C = temperature) |> no temperature data
  mutate(site = "GL5_OUTLET",
         date = as.Date(date),
         year = year(date),
         year = as.factor(year),
         depth_m = 0) |>
  relocate(site) |>
  dplyr::select(-notes, -LTER_site.x, -local_site.x, -LTER_site.y, -local_site.y, -time, - acid, - alkal, -Trit, -time, -D_excess, -dD_sdev, -dDeut, -d18O_sdev, -d18O, -T_sdev, -POC, -cat_sum, -an_sum, -chg_bal) |>
  mutate(across(c(3,5:30), as.numeric)) |>
  #mutate(mean_temp_C = ifelse(is.nan(mean_temp_C), NA, mean_temp_C)) |>
  rename(SpC_uscm = cond,
         ANC_ueqL = ANC,
         H_ueqL = H.,
         NH4_ueqL = NH4.,
         Ca_ueqL = Ca..,
         Mg_ueqL = Mg..,
         Na_ueqL = Na.,
         K_ueqL = K.,
         Cl_ueqL = Cl.,
         NO3_ueqL = NO3.,
         SO4_ueqL = SO4..,
         PO4_ueqL = PO4...,
         Si_umolL = Si,
         TN_umolL = TN,
         TDN_umolL = TDN,
         PN_umolL = PN,
         DON_umolL = DON,
         IN_ueqL = IN,
         TP_umolL = TP,
         TDP_umolL = TDP,
         PP_umolL = PP,
         DOP_umolL = DOP,
         IP_umolL = IP,
         TOC_mgL = TOC,
         DOC_mgL = DOC) |>
  relocate(c(TOC_mgL, DOC_mgL, TN_umolL, TDN_umolL, PN_umolL, IN_ueqL, DON_umolL, NH4_ueqL, NO3_ueqL, TP_umolL,TDP_umolL, PP_umolL, IP_umolL, DOP_umolL, PO4_ueqL)) 
  # get rid of rows without any nutrient data
gl5_format <- gl5_format |>
  mutate(check = rowSums(gl5_format[,c(1:15)], na.rm = TRUE)) |>
  filter(check != 0) |>
  dplyr::select(-check)


albin_format <- albinwq |>
  mutate(site = "ALB_INLET",
         date = as.Date(date),
         year = year(date),
         year = as.factor(year),
         depth_m = 0) |>
  relocate(site) |>
  dplyr::select(-LTER_site, -local_site, -time, - acid, - alkal, -Trit, -time, -D_excess, -dD_sdev, -dDeut, -d18O_sdev, -d18O, -T_sdev, -POC, -cat_sum, -an_sum, -chg_bal) |>
  mutate(across(c(4:29), as.numeric)) |>
  #mutate(mean_temp_C = ifelse(is.nan(mean_temp_C), NA, mean_temp_C)) |>
  rename(SpC_uscm = cond,
         ANC_ueqL = ANC,
         H_ueqL = H.,
         NH4_ueqL = NH4.,
         Ca_ueqL = Ca..,
         Mg_ueqL = Mg..,
         Na_ueqL = Na.,
         K_ueqL = K.,
         Cl_ueqL = Cl.,
         NO3_ueqL = NO3.,
         SO4_ueqL = SO4..,
         PO4_ueqL = PO4...,
         Si_umolL = Si,
         TN_umolL = TN,
         TDN_umolL = TDN,
         PN_umolL = PN,
         DON_umolL = DON,
         IN_ueqL = IN,
         TP_umolL = TP,
         TDP_umolL = TDP,
         PP_umolL = PP,
         DOP_umolL = DOP,
         IP_umolL = IP,
         TOC_mgL = TOC,
         DOC_mgL = DOC) |>
  relocate(c(TOC_mgL, DOC_mgL, TN_umolL, TDN_umolL, PN_umolL, IN_ueqL, DON_umolL, NH4_ueqL, NO3_ueqL, TP_umolL,TDP_umolL, PP_umolL, IP_umolL, DOP_umolL, PO4_ueqL))
  # get rid of rows without any nutrient data
albin_format <- albin_format |>
  mutate(check = rowSums(albin_format[,c(1:15)], na.rm = TRUE)) |>
  filter(check != 0) |>
  dplyr::select(-check)



flume_format <- flumewq |>
  mutate(site = "FLUME",
         date = as.Date(date),
         year = year(date),
         year = as.factor(year),
         depth_m = 0) |>
  relocate(site) |>
  dplyr::select(-LTER_site, -local_site, -time, - acid, - alkal, -Trit, -time, -D_excess, -dD_sdev, -dDeut, -d18O_sdev, -d18O, -T_sdev, -POC, -cat_sum, -an_sum, -chg_bal) |>
  mutate(across(c(4:29), as.numeric)) |>
  #mutate(mean_temp_C = ifelse(is.nan(mean_temp_C), NA, mean_temp_C)) |>
  rename(SpC_uscm = cond,
         ANC_ueqL = ANC,
         H_ueqL = H.,
         NH4_ueqL = NH4.,
         Ca_ueqL = Ca..,
         Mg_ueqL = Mg..,
         Na_ueqL = Na.,
         K_ueqL = K.,
         Cl_ueqL = Cl.,
         NO3_ueqL = NO3.,
         SO4_ueqL = SO4..,
         PO4_ueqL = PO4...,
         Si_umolL = Si,
         TN_umolL = TN,
         TDN_umolL = TDN,
         PN_umolL = PN,
         DON_umolL = DON,
         IN_ueqL = IN,
         TP_umolL = TP,
         TDP_umolL = TDP,
         PP_umolL = PP,
         DOP_umolL = DOP,
         IP_umolL = IP,
         TOC_mgL = TOC,
         DOC_mgL = DOC) |>
  relocate(c(TOC_mgL, DOC_mgL, TN_umolL, TDN_umolL, PN_umolL, IN_ueqL, DON_umolL, NH4_ueqL, NO3_ueqL, TP_umolL,TDP_umolL, PP_umolL, IP_umolL, DOP_umolL, PO4_ueqL)) 

flume_format <- flume_format |>
  mutate(check = rowSums(flume_format[,c(1:15)], na.rm = TRUE)) |>
  filter(check != 0) |>
  dplyr::select(-check)

greenlakesnetwork <- bind_rows(alb_camp_format, albin_format, arik_format, flume_format, gl4_format, gl5_format, lakes_format) 

write.csv(greenlakesnetwork, "Data/greenlakes_network.csv")

gl_network <- read.csv("Data/greenlakes_network.csv") |>
  dplyr::select(-X) |>
  mutate(date = as.Date(date)) |>
  mutate(IP_umolL = ifelse(IP_umolL < 0, NA, IP_umolL))|>
  mutate(DON_umolL = ifelse(DON_umolL < 0, NA, IP_umolL)) |>
  mutate(IP_umolL = ifelse(is.na(IP_umolL), PO4_ueqL/3, IP_umolL)) |>
  mutate(IN_ueqL = ifelse(is.na(IN_ueqL), NH4_ueqL + NO3_ueqL, IN_ueqL)) |>
  rename(IN_umolL = IN_ueqL)

write.csv(gl_network, "Data/greenlakes_network.csv")


# Add seasons to dataset ####
library(lubridate)
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  mutate(date = as.Date(date, format='%m/%d/%Y')) |>
  mutate(mon = month(date)) |> #and add seasons to the dataframe
  mutate(season = case_when(mon %in% c(12,1,2,3,4,5) ~ "Winter",
                            mon %in% c(6,7)  ~ "Snowmelt runoff",
                            mon %in% c(8,9,10,11) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) 
write.csv(gl_network, "Data/greenlakes_network.csv")


#| umol/L IP is umol PO4 as P  -- convert to as P by dividing PO4 ueq/L by 3
#| ueq/L NO3 and ueq/L NH4 is umol NO3 as N and NH4 as N


#   mutate(IP_umolL = ifelse(IP_umolL < 0, NA, IP_umolL))|>
#   mutate(DON_umolL = ifelse(DON_umolL < 0, NA, IP_umolL)) |>
#   mutate(NO3_N_mgL = NO3_ueqL/16.13 * 0.2259) |>
#   mutate(NH4_N_mgL = NH4_ueqL/55.44 * 0.7765) |>
#   mutate(PO4_P_mgL = PO4_ueqL/31.59 * 0.3261)
# 
# 
# library(biogas)
# 
# Pmol <- molMass("P") * 1000 #mg/mol
# Nmol <- molMass("N") * 1000 #mg/mol
# 
# gl_network <- gl_network |>
#   mutate(NO3_N_umolL = NO3_N_mgL/Nmol * 1000 * 1000) |>
#   mutate(NH4_N_umolL = NH4_N_mgL/Nmol * 1000 * 1000) |>
#   mutate(PO4_P_umolL = PO4_P_mgL/Pmol * 1000 * 1000) |>
#   relocate(NO3_N_umolL, NH4_N_umolL, PO4_P_umolL)


# sites ####
# gl_network_sites <- read.csv("Data/greenlakes_network.csv") |>
#   dplyr::select(site, arik_flow_site) |>
#   unique()
# write.csv(gl_network_sites, "Data/sites.csv")
# saved and manually added lat long and network position