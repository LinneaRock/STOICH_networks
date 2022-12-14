#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to make a ternary plot figures 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# The limitation key ####
library(tidyverse)
library(ggtern)

ggtern() +
  theme_bw()+
  theme_showarrows() +
  Rlab("", labelarrow = "Carbon (%)") +
  Tlab("", labelarrow = "Redfield-normalized N (%)") +
  Llab("", labelarrow = "Redfield-normalized P (%)") +
  geom_Rline(Rintercept = 0.2, color = "goldenrod1") +
  geom_Tline(Tintercept = 0.2, color = "red3") +
  geom_Lline(Lintercept = 0.2, color = "blue4") +
  geom_text(aes(x=40, y = 40, z=40), label = "Balanced") +
  geom_text(aes(x=90, y = 10, z=10), label = "C & N \n Limitation") +
  geom_text(aes(x=10, y = 10, z=90), label = "N & P \n Limitation") +
  geom_text(aes(x=40, y = 40, z=10), label = "C Limitation") +
  geom_text(aes(x=10, y = 40, z=40), label = "P Limitation") +
  geom_text(aes(x=90, y = 20, z=90), label = "N Limitation") +
  geom_text(aes(x=40, y = 350, z=40), label = "C & P \n Limitation") 
ggsave("Figures/ggterns/limitation_tern.png", height = 4.5, width = 6.5, units = "in", dpi = 500)


ggtern() +
  theme_bw()+
  theme_showarrows() +
  Rlab("", labelarrow = "Carbon (%)") +
  Tlab("", labelarrow = "Redfield-normalized N (%)") +
  Llab("", labelarrow = "Redfield-normalized P (%)")
ggsave("Figures/ggterns/ratio_tern.png", height = 4.5, width = 6.5, units = "in", dpi = 500)
# Save then add lines in PP because I can't figure out how to add lines with two different slopes here :(


# call in data and any other libraries ####
library(lubridate)
library(biogas)

Cmol <- molMass("C") * 1000 #mg/mol

sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))


# Plotting reactives ####

reactives <- gl_network |>
  select(DOC_mgL, IN_umolL, IP_umolL, site, date, year, depth_m, network_position, eco_type, lat, long) |>
  drop_na() |>
  mutate(DOC_molar = DOC_mgL/Cmol, # convert to mol/L
         DIN_molar = IN_umolL/1000, # convert to mol/L
         DIP_molar = IP_umolL/1000) |> # convert to mol/L
  mutate(DIN_molar_R = DIN_molar * 6.625, # redfield normalization 106/16 = 6.625
         DIP_molar_R = DIP_molar * 106, # redfield normalizaiton 106/1
         DOC_molar_R = DOC_molar) |> # --- DIC_molar_R == DIC_molar because 106/106
  group_by(site, date, depth_m) |>
  mutate(TOTAL_REDFIELD = sum(DOC_molar, DIN_molar_R, DIP_molar_R)) |>
  ungroup() |>
  mutate(percent_DOC = (DOC_molar_R/TOTAL_REDFIELD) * 100,
         percent_DIN = (DIN_molar_R/TOTAL_REDFIELD) *100,
         percent_DIP = (DIP_molar_R/TOTAL_REDFIELD) * 100) |>
  #mutate(network_position=as.factor(network_position)) |>
  mutate(season = factor(season, levels = c('Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Dec')))
  # group_by(season, year, site, network_position, eco_type, parameter) |>
  # summarise(meanconc_umolL = mean(concentration_umol_L)) |>
  # ungroup() 


# Ternary plots ####

## Checking euphotic data by network position ####
ggtern(reactives |> filter(depth_m < 3), aes(percent_DIP, percent_DIN, percent_DOC, color = network_position)) +
  geom_point() +
  theme_bw()+
  # facet_wrap(~month(date, label = TRUE), ncol=4) +
  scale_color_viridis_c("Network Position") +
  theme_showarrows() +
  #labs(title = "2019") +
  Rlab("", labelarrow = "%DOC") +
  Tlab("", labelarrow = "%DIN") +
  Llab("", labelarrow = "%DIP") +
  geom_Rline(Rintercept = 0.2, color = "goldenrod1") +
  geom_Tline(Tintercept = 0.2, color = "red3") +
  geom_Lline(Lintercept = 0.2, color = "blue4")  
ggsave("Figures/ggterns/DOC_DIN_DIP_tern.png", width = 6.5, height = 4.5, units = "in", dpi = 500)

## Checking euphotic data by season ####
ggtern(reactives |> filter(depth_m < 3), aes(percent_DIP, percent_DIN, percent_DOC, color = season)) +
  geom_point() +
  theme_bw()+
  # facet_wrap(~month(date, label = TRUE), ncol=4) +
 # scale_color_viridis_c("Season") +
  theme_showarrows() +
  #labs(title = "2019") +
  Rlab("", labelarrow = "%DOC") +
  Tlab("", labelarrow = "%DIN") +
  Llab("", labelarrow = "%DIP") +
  geom_Rline(Rintercept = 0.2, color = "goldenrod1") +
  geom_Tline(Tintercept = 0.2, color = "red3") +
  geom_Lline(Lintercept = 0.2, color = "blue4")  
ggsave("Figures/ggterns/season_tern.png", width = 6.5, height = 4.5, units = "in", dpi = 500)

## Checking by depth ####
ggtern(reactives, aes(percent_DIP, percent_DIN, percent_DOC, color = depth_m)) +
  geom_point() +
  theme_bw()+
  # facet_wrap(~month(date, label = TRUE), ncol=4) +
  scale_color_viridis_c("Depth (m)") +
  theme_showarrows() +
  #labs(title = "2019") +
  Rlab("", labelarrow = "%DOC") +
  Tlab("", labelarrow = "%DIN") +
  Llab("", labelarrow = "%DIP") +
  geom_Rline(Rintercept = 0.2, color = "goldenrod1") +
  geom_Tline(Tintercept = 0.2, color = "red3") +
  geom_Lline(Lintercept = 0.2, color = "blue4")  
ggsave("Figures/ggterns/depth_tern.png", width = 6.5, height = 4.5, units = "in", dpi = 500)





# Plotting organics ####
organics <- gl_network |>
  select(DOC_mgL, DON_umolL, DOP_umolL, site, date, year, depth_m, network_position, season, eco_type, lat, long) |>
  drop_na() |>
  mutate(DOC_molar = DOC_mgL/Cmol, # convert to mol/L
         DON_molar = DON_umolL/1000, # convert to mol/L
         DOP_molar = DOP_umolL/1000) |> # convert to mol/L
  mutate(DON_molar_R = DON_molar * 6.625, # redfield normalization 106/16 = 6.625
         DOP_molar_R = DOP_molar * 106, # redfield normalizaiton 106/1
         DOC_molar_R = DOC_molar) |> # --- DIC_molar_R == DIC_molar because 106/106
  group_by(site, date, depth_m) |>
  mutate(TOTAL_REDFIELD = sum(DOC_molar, DON_molar_R, DOP_molar_R)) |>
  ungroup() |>
  mutate(percent_DOC = (DOC_molar_R/TOTAL_REDFIELD) * 100,
         percent_DON = (DON_molar_R/TOTAL_REDFIELD) *100,
         percent_DOP = (DOP_molar_R/TOTAL_REDFIELD) * 100) 


# Ternary plots ####

## Checking euphotic data by network position ####
ggtern(organics |> filter(depth_m < 3), aes(percent_DOP, percent_DON, percent_DOC, color = network_position)) +
  geom_point() +
  theme_bw()+
  # facet_wrap(~month(date, label = TRUE), ncol=4) +
  scale_color_viridis_c("Network Position") +
  theme_showarrows() +
  #labs(title = "2019") +
  Rlab("", labelarrow = "%DOC") +
  Tlab("", labelarrow = "%DON") +
  Llab("", labelarrow = "%DOP") +
  geom_Rline(Rintercept = 0.2, color = "goldenrod1") +
  geom_Tline(Tintercept = 0.2, color = "red3") +
  geom_Lline(Lintercept = 0.2, color = "blue4")  
ggsave("Figures/ggterns/DOC_DON_DOP_tern.png", width = 6.5, height = 4.5, units = "in", dpi = 500)

## Checking euphotic data by season ####
ggtern(organics |> filter(depth_m < 3), aes(percent_DOP, percent_DON, percent_DOC, color = season)) +
  geom_point() +
  theme_bw()+
  # facet_wrap(~month(date, label = TRUE), ncol=4) +
  # scale_color_viridis_c("Season") +
  theme_showarrows() +
  #labs(title = "2019") +
  Rlab("", labelarrow = "%DOC") +
  Tlab("", labelarrow = "%DON") +
  Llab("", labelarrow = "%DOP") +
  geom_Rline(Rintercept = 0.2, color = "goldenrod1") +
  geom_Tline(Tintercept = 0.2, color = "red3") +
  geom_Lline(Lintercept = 0.2, color = "blue4")  
ggsave("Figures/ggterns/season_tern_organics.png", width = 6.5, height = 4.5, units = "in", dpi = 500)

## Checking by depth ####
ggtern(organics, aes(percent_DOP, percent_DON, percent_DOC, color = depth_m)) +
  geom_point() +
  theme_bw()+
  # facet_wrap(~month(date, label = TRUE), ncol=4) +
  scale_color_viridis_c("Depth (m)") +
  theme_showarrows() +
  #labs(title = "2019") +
  Rlab("", labelarrow = "%DOC") +
  Tlab("", labelarrow = "%DON") +
  Llab("", labelarrow = "%DOP") +
  geom_Rline(Rintercept = 0.2, color = "goldenrod1") +
  geom_Tline(Tintercept = 0.2, color = "red3") +
  geom_Lline(Lintercept = 0.2, color = "blue4")  
ggsave("Figures/ggterns/depth_tern_organics.png", width = 6.5, height = 4.5, units = "in", dpi = 500)
