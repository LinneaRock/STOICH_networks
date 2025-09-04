# Calling raw data wihtout outliers removed

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
  filter(depth_m <=3 | is.na(depth_m)) # just look at photic zone

nuts <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, 2:7, 10:14) |>
  pivot_longer(7:17, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)  |>
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m)) # just look at photic zone
