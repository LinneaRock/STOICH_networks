#::::::::::::::::::::
# Nutrient limitation
#::::::::::::::::::::   

library(tidyverse)
library(lubridate)

sites <- read.csv("Data/sites.csv") |>
  drop_na(long)
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  mutate(network_position = as.factor(network_position))

medians <- gl_network |>
  group_by(site, year(date), season) |>
  mutate(yearlymedTP_umolL = median(TP_umolL, na.rm = TRUE),
         yearlymedIN_umolL = median(IN_umolL, na.rm = TRUE),
         yearlymedTN_umolL = median(TN_umolL, na.rm = TRUE),
         yearlymedIP_umolL = median(IP_umolL, na.rm = TRUE)) |>
  ungroup() |>
  select(site, season, `year(date)`, yearlymedTP_umolL, yearlymedIP_umolL, yearlymedIN_umolL, yearlymedTN_umolL) |>
  distinct() |>
  group_by(site, season) |>
  mutate(MEDTP_umolL = median(yearlymedTP_umolL, na.rm = TRUE),
         MEDTN_umolL = median(yearlymedTN_umolL, na.rm = TRUE),
         MEDIN_umolL = median(yearlymedIN_umolL, na.rm = TRUE),
         MEDIP_umolL = median(yearlymedIP_umolL, na.rm = TRUE)) |>
  ungroup() |>
  select(site, season, MEDTP_umolL, MEDTN_umolL, MEDIP_umolL, MEDIN_umolL) |>
  distinct()
  


# DIN:TP limitation ####
ggplot() +
  geom_point(gl_network, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), fill = season), shape = 21, alpha = 0.8) +
  geom_point(medians, mapping = aes(log10(MEDTP_umolL), log10(MEDIN_umolL/MEDTP_umolL), fill = season), color ='black', size = 3, shape = 21) +
  geom_abline(slope = 0, intercept = log10(3.4), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5), linetype = "dashed") +  # bergstrom N limitation line
  #geom_abline(slope = 0, intercept = log(16, base = 10), color = "red4") + # redfield
  theme_minimal() +
  labs(y = "Log DIN:TP", x = "Log TP") +
  #annotate('text', label = 'Redfield 16:1 line', x = 0.5, y = 1.1, hjust = 0, size = 2, color = "red4") +
  annotate('text', label = 'Predicted N limitation below dashed line \n (Bergström, 2010)', x = -1, y = 0.1, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation above dashed line \n (Bergström, 2010)', x = 0.25, y = 0.5, hjust = 0, size = 2) 
  

# TN:TP limitation ####
ggplot(gl_network) +
  geom_point(aes(log10(TP_umolL), log10(TN_umolL/TP_umolL), fill = season), size = 2.5, shape = 21, alpha = 0.5) +
  geom_abline(slope = 0, intercept = log10(41), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(19), linetype = "dashed") +  # bergstrom N limitation line
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red4") + # redfield
  theme_minimal() +
  labs(y = "Log TN:TP", x = "Log TP") +
  annotate('text', label = 'Redfield 16:1 line', x = 0.5, y = 1.1, hjust = 0, size = 2, color = "red4") +
  annotate('text', label = 'Predicted N limitation\n below dashed line \n (Bergström, 2010)', x = -1, y = 1.0, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation\n above dashed line \n (Bergström, 2010)', x = 0.25, y = 1.75, hjust = 0, size = 2)


# DIN:DIP limitation ####
ggplot(gl_network) +
  geom_point(aes(log10(IP_umolL), log10((IN_umolL/IP_umolL)), fill = season), size = 2.5, shape = 21, alpha = 0.5) +
  geom_abline(slope = 0, intercept = log10(100), linetype = "dashed") + # Keck and Lepori P limitation line
  geom_abline(slope = 0, intercept = log10(1), linetype = "dashed") +  # keck and lepori N limitation line
  geom_abline(slope = 0, intercept = log10(16), color = "red4") + # redfield
  theme_minimal() +
  labs(y = "Log DIN:DIP", x = "Log TP") +
  annotate('text', label = 'Redfield 16:1 line', x = -0.5, y = 1.1, hjust = 0, size = 2, color = "red4") +
  annotate('text', label = 'Predicted N limitation below dashed line \n (Keck & Lepori, 2012)', x = -2, y = -0.1, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation\n above dashed line \n (Keck & Lepori, 2012)', x = -2.5, y = 2, hjust = 0, size = 2)

