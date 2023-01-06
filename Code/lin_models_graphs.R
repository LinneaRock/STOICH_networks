#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to explore relationships
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


library(tidyverse)
library(lme4) 
library(ggpubr)

sites <- read.csv("Data/sites.csv") |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', '7','8','9','10','11','12','12a','13','14','15','16')))
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))

plot_log <- function(param, name) {
  ggplot(gl_network, aes(network_position, param)) +
    geom_point(aes(color = season)) +
    scale_y_log10() +
    geom_smooth() +
    labs(y = name)
  }

plot_log(gl_network$TN_umol, 'TN_umolL')
summary(lm(log10(TN_umolL)~network_position, gl_network
           |> filter(TN_umolL > 0 & !is.na(TN_umolL))))  # r=0.3
summary(lmer(log10(TN_umolL)~network_position + (1|season), gl_network
           |> filter(TN_umolL > 0 & !is.na(TN_umolL)))) 

plot_log(gl_network$IN_umol, 'IN_umolL')
summary(lm(log10(IN_umolL)~network_position, gl_network 
           |> filter(IN_umolL > 0 & !is.na(IN_umolL)))) # r=0.2

plot_log(gl_network$DON_umol, 'DON_umolL')
summary(lm(log10(DON_umolL)~network_position, gl_network 
           |> filter(DON_umolL > 0 & !is.na(DON_umolL)))) # r=0.1

plot_log(gl_network$TDN_umol, 'TDN_umolL')
summary(lm(log10(TDN_umolL)~network_position, gl_network 
           |> filter(TDN_umolL > 0 & !is.na(TDN_umolL)))) # r=0.2

plot_log(gl_network$PN_umol, 'PN_umolL')
summary(lm(log10(PN_umolL)~network_position, gl_network 
           |> filter(PN_umolL > 0 & !is.na(PN_umolL)))) # r=0.005






plot_log(gl_network$TP_umol, 'TP_umolL')
summary(lm(log10(TP_umolL)~network_position, gl_network
           |> filter(TP_umolL > 0 & !is.na(TP_umolL))))  # r=0.1

plot_log(gl_network$IP_umol, 'IP_umolL')
summary(lm(log10(IP_umolL)~network_position, gl_network 
           |> filter(IP_umolL > 0 & !is.na(IP_umolL)))) # r=0.1

plot_log(gl_network$DOP_umol, 'DOP_umolL')
summary(lm(log10(DOP_umolL)~network_position, gl_network 
           |> filter(DOP_umolL > 0 & !is.na(DOP_umolL)))) # r=0.04

plot_log(gl_network$TDP_umol, 'TDP_umolL')
summary(lm(log10(TDP_umolL)~network_position, gl_network 
           |> filter(TDP_umolL > 0 & !is.na(TDP_umolL)))) # r=0.1

plot_log(gl_network$PP_umol, 'PP_umolL')
summary(lm(log10(PP_umolL)~network_position, gl_network 
           |> filter(PP_umolL > 0 & !is.na(PP_umolL)))) # r=0.002
  



ggplot(gl_network, aes(log10(NO3_ueqL), log10(DOC_mgL), color = log10(PO4_ueqL))) +
  geom_point()

ggplot(gl_network, aes(TP_umolL, TN_umolL, color = network_position, shape = season)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

ggplot(gl_network, aes( TDP_umolL, TDN_umolL, color = network_position, shape = season)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

ggplot(gl_network, aes(IP_umolL, IN_umolL, color = network_position, shape = season)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

ggplot(gl_network, aes(DOP_umolL, DON_umolL, color = network_position, shape = season)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

ggplot(gl_network, aes(PP_umolL, PN_umolL, color = network_position, shape = season)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()



ggplot(gl_network |>
         filter(!is.na(long)), aes(season, IN_umolL/IP_umolL, color = eco_type)) +
  geom_boxplot() +
 # stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('Jan-Mar','Apr-Jun'),c('Jan-Mar', 'Jul-Sep'), c('Jan-Mar','Oct-Dec'), c('Apr-Jun','Jul-Sep'), c('Apr-Jun','Oct-Dec'), c('Jul-Sep', 'Oct-Dec'))) +
  # ^^ Kruskal-Wallis test --- y.label.npc does not work ???!!!!!! 
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  scale_y_log10()



m <- aov(IN_umolL/IP_umolL ~ season * eco_type, gl_network |> filter((IN_umolL/IP_umolL) > 0 & !is.na(IN_umolL/IP_umolL) & is.finite(IN_umolL/IP_umolL)))
anova(m)
TukeyHSD(m, conf.level = 0.95)
str(gl_network)
