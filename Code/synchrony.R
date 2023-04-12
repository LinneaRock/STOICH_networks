

source('Data/CALL_DATA_PACKAGES.R') 

nuts.setup <- nuts |>
  mutate(year=year(date)) |>
  select(-site, -depth_m, -date) |>
  group_by(network_position, eco_type, season, year,param) |>
  summarise(med=median(result)) |>
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL)

TN_corr_dat <- nuts.setup |>
  select(1:4, TN_umolL) |>
  drop_na() |>
  group_by(network_position, eco_type, season) |>
  summarise(mean_TN = mean(TN_umolL)) |>
  ungroup() |>
  filter(season=='Jul-Sep')
