
library(tidyverse)
sites <- read.csv("Data/sites.csv")
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))

# Basic timeries ####
# not very much TOC data
ggplot(gl_network, aes(date, TOC_mgL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year>1996), aes(date, DOC_mgL, color = network_position)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year>1995 & year<2009), aes(date, TN_umolL, color = network_position)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year > 1995), aes(date, TDN_umolL, color = network_position)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year > 1995& year<2009), aes(date, PN_umolL, color = network_position)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year>1993), aes(date, IN_umolL, color = network_position)) +
  geom_point() +
  geom_line()

ggplot(gl_network|> filter(year > 1995), aes(date, DON_umolL, color = network_position)) +
  geom_point() +
  geom_line()



ggplot(gl_network |> filter(year>1995 & year<2009), aes(date, TP_umolL, color = network_position)) +
  geom_point() +
  geom_line() + ylim(0,3)


ggplot(gl_network |> filter(year > 1995), aes(date, TDP_umolL, color = network_position)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year > 1995& year<2012), aes(date, PP_umolL, color = network_position)) +
  geom_point() +
  geom_line() + ylim(0,3)


ggplot(gl_network |> filter(year>1997), aes(date, IP_umolL, color = network_position)) +
  geom_point() +
  geom_line()

ggplot(gl_network|> filter(year > 1995), aes(date, DOP_umolL, color = network_position)) +
  geom_point() +
  geom_line()


ggplot(gl_network, aes(date, NO3_ueqL, color = network_position)) +
  geom_point() +
  geom_line()

ggplot(gl_network, aes(date, NH4_ueqL, color = network_position)) +
  geom_point() +
  geom_line()

ggplot(gl_network, aes(date, PO4_ueqL, color = network_position)) +
  geom_point() +
  geom_line()


ggplot(gl_network, aes(date, daily_dis_m3, color = network_position)) +
  geom_point() +
  geom_line()

# Basic linear regressions ####
ggplot(gl_network |> filter(year>1996), aes(network_position, DOC_mgL, group= site, color = eco_type)) +
  geom_boxplot() 


ggplot(gl_network |> filter(year>1995 & year<2009), aes(network_position, TN_umolL, group= site, color = eco_type)) +
  geom_boxplot() 


ggplot(gl_network |> filter(year > 1995), aes(network_position, TDN_umolL, group= site, color = eco_type)) +
  geom_boxplot() 



ggplot(gl_network |> filter(year > 1995& year<2009), aes(network_position, PN_umolL, group= site, color = eco_type)) +
  geom_boxplot() 



ggplot(gl_network |> filter(year>1993), aes(network_position, IN_umolL, group= site, color = eco_type)) +
  geom_boxplot() 


ggplot(gl_network|> filter(year > 1995), aes(network_position, DON_umolL, group= site, color = eco_type)) +
  geom_boxplot() 




ggplot(gl_network |> filter(year>1995 & year<2009), aes(network_position, TP_umolL, group= site, color = eco_type)) +
  geom_boxplot() # + ylim(0,3)


ggplot(gl_network |> filter(year > 1995), aes(network_position, TDP_umolL, group= site, color = eco_type)) +
  geom_boxplot() 



ggplot(gl_network |> filter(year > 1995& year<2012), aes(network_position, PP_umolL, group= site, color = eco_type)) +
  geom_boxplot() #+ ylim(0,3)


ggplot(gl_network |> filter(year>1997), aes(network_position, IP_umolL, group= site, color = eco_type)) +
  geom_boxplot() 



ggplot(gl_network|> filter(year > 1995), aes(network_position, DOP_umolL, group= site, color = eco_type)) +
  geom_boxplot() 



ggplot(gl_network, aes(network_position, NO3_ueqL, group= site, color = eco_type)) +
  geom_boxplot() 


ggplot(gl_network, aes(network_position, NH4_ueqL, group= site, color = eco_type)) +
  geom_boxplot() 

ggplot(gl_network, aes(network_position, PO4_ueqL, group= site, color = eco_type)) +
  geom_boxplot() 


ggplot(gl_network, aes(network_position, daily_dis_m3, group= site, color = eco_type)) +
  geom_boxplot() 


#:::::::::::::::::

library(GGally)

ggcorr(gl_network[2:15], method = c("na.or.complete", "pearson"))

