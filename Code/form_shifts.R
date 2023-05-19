#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# How do nutrient forms change over the network?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#### Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

nuts_wide <- nuts |>
  filter(param %in% c('DON_umolL','IN_umolL','PN_umolL',
                      'DOP_umoL', 'IP_umolL', 'PP_umolL')) |>
  group_by(param, network_position, date, season) |>
  summarise(result=mean(result)) |> # average depths 0-3 where appropriate 
  ungroup() |>
  pivot_wider(names_from = 'network_position', values_from = 'result')
