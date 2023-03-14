

source('Data/CALL_DATA_PACKAGES.R') 



ggplot(nuts) +
  geom_histogram(aes(result, fill = network_position)) +
  facet_wrap(.~param, scales = 'free')

ggplot(nuts) +
  geom_density(aes(result, fill = network_position)) +
  facet_wrap(.~param, scales = 'free')

nuts_count <- nuts |>
  group_by(site, param) |>
  summarise(N = length(result))
# ugh


ggplot(nuts) +
  geom_line(aes(date, result, color = network_position)) +
  facet_wrap(.~param, scales = 'free')

nuts_monthlyave <- nuts |>
  mutate(year = year(date),
         month = month(date, label=TRUE)) |>
  group_by(site, network_position, eco_type, season, year, month, param) |>
  summarise(mean.result = mean(result)) |>
  unite(col = month.year, month, year, sep='-') |>
  mutate(month.year = as.Date(paste0('01-',month.year), format='%d-%b-%Y'))|>
  ungroup()


nuts_count_mo <- nuts_monthlyave |>
  group_by(site, param) |>
  summarise(N = length(mean.result))
# ugh

ggplot(nuts_monthlyave |> filter(network_position != '1')) +
  geom_line(aes(month.year, mean.result, color = network_position)) +
  facet_wrap(.~param, scales = 'free')


median_N <- nuts_monthlyave |> 
  filter(network_position != '1')|>
  filter(param %in% c('DON_umolL', 'IN_umolL', 'PN_umolL', 'TDN_umolL', "TN_umolL")) |>
 # pivot_wider(names_from = param, values_from = mean.result) |>
 # drop_na() |>
 # pivot_longer(6:10, names_to='param', values_to='mean.result') |>
  group_by(site, network_position, eco_type, season, param) |>
  summarize(median = median(mean.result)) |>
  ungroup() |>
  mutate(param = factor(param, levels=c('TN_umolL','TDN_umolL','PN_umolL','IN_umolL','DON_umolL')))


ggplot(median_N) +
  geom_area(aes(as.numeric(network_position), median, fill=param, group=param)) +
  geom_point(aes(as.numeric(network_position),0), color = 'black') +
  facet_grid(.~season) +
  scale_fill_viridis_d('') +
  theme_classic()

show_col(viridis(5))

ggplot(median_N |> filter(param == 'DON_umolL')) +
  geom_area(aes(as.numeric(network_position), median), fill = '#FDE725FF') +
  geom_point(aes(as.numeric(network_position),0), color = 'black') +
  facet_grid(.~season) +
  theme_classic()



median_P <- nuts_monthlyave |> 
  filter(network_position != '1')|>
  filter(param %in% c('DOP_umolL', 'IP_umolL', 'PP_umolL', 'TDP_umolL', "TP_umolL")) |>
  # pivot_wider(names_from = param, values_from = mean.result) |>
  # drop_na() |>
  # pivot_longer(6:10, names_to='param', values_to='mean.result') |>
  group_by(site, network_position, eco_type, season, param) |>
  summarize(median = median(mean.result)) |>
  ungroup()|>
  mutate(param = factor(param, levels=c('TP_umolL','TDP_umolL','PP_umolL','IP_umolL','DOP_umolL')))


ggplot(median_P) +
  geom_area(aes(as.numeric(network_position), median, fill=param, group=param)) +
  geom_point(aes(as.numeric(network_position),0), color = 'black') +
  facet_grid(.~season)  +
  scale_fill_viridis_d('') +
  theme_classic()
