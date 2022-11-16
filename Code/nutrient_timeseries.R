
library(tidyverse)

gl_network <- read.csv("Data/greenlakes_network.csv") |>
  select(-X) |>
  mutate(date = as.Date(date)) |>
  mutate(IP_umolL = ifelse(IP_umolL < 0, NA, IP_umolL))

# not very much TOC data
ggplot(gl_network, aes(date, TOC_mgL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year>1996), aes(date, DOC_mgL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year>1995 & year<2009), aes(date, TN_umolL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year > 1995), aes(date, TDN_umolL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year > 1995& year<2009), aes(date, PN_umolL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year>1993), aes(date, IN_ueqL, color = site)) +
  geom_point() +
  geom_line()

ggplot(gl_network|> filter(year > 1995), aes(date, DON_umolL, color = site)) +
  geom_point() +
  geom_line()



ggplot(gl_network |> filter(year>1995 & year<2009), aes(date, TP_umolL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year > 1995), aes(date, TDP_umolL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year > 1995& year<2012), aes(date, PP_umolL, color = site)) +
  geom_point() +
  geom_line()


ggplot(gl_network |> filter(year>1997), aes(date, IP_umolL, color = site)) +
  geom_point() +
  geom_line()

ggplot(gl_network|> filter(year > 1995), aes(date, DOP_umolL, color = site)) +
  geom_point() +
  geom_line()
