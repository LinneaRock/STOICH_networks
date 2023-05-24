#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# How do nutrient forms change over the network?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#### Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

nuts.setup <- nuts |>
  filter(param %in% c('TN_umolL','DON_umolL','IN_umolL','PN_umolL',
                      'DOP_umoL', 'IP_umolL', 'PP_umolL')) |>
  group_by(param, network_position, date, season) |>
  summarise(result=mean(result)) |> # average depths 0-3 where appropriate 
  ungroup() 


Nforms <- nuts.setup |>
  filter(param %in% c('TN_umolL','DON_umolL','IN_umolL','PN_umolL')) |>
  pivot_wider(id_cols = c('network_position','date'), names_from = 'param', values_from = 'result') |>
  drop_na(TN_umolL) |>
  mutate(perc_organic = DON_umolL/TN_umolL * 100,
         perc_inorganic = IN_umolL/TN_umolL * 100,
         perc_particulate = PN_umolL/TN_umolL * 100) |>
  mutate(checkperc = perc_organic + perc_inorganic + perc_particulate)

# my percents are over 100% in a few cases so this probably is not the right way to do this

