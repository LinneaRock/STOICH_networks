# call in packages

library(tidyverse)
library(dataRetrieval) # for water year
library(lubridate)
library(scales)
library(ggpubr)
library(multcompView)
library(ggtern)
library(biogas)
library(colorblindr)
library(lme4)
library(ggpubr)
library(plotrix) # gets standard error
library(GGally)
library(ggmap)
library(leaflet)
#library(nhdplusTools)
library(sf)
library(ggspatial)

ions <- source('Data/ions_outliers_removed.R')
nuts <- source('Data/nuts_outliers_removed.R')
stoich <- source('Data/stoich_after_outliers_removed.R')

#### frequency plots outlier removed data ####
ggplot(ions) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free') +
  labs(title = 'outliers removed')


ggplot(nuts) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'outliers removed')

ggplot(discharge) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free') +
  labs(title = 'outliers removed')

ggplot(stoich) +
  geom_density(aes(result)) +
  facet_wrap(.~param, scales = 'free')+
  labs(title = 'outliers removed') # question, do I need to delete outliers from the stoich dataset too? 
