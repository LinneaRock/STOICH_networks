#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to calculate production and consumption along the network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


source('Data/CALL_DATA_PACKAGES.R') 

### REDO THIS SCRIPT

# sites <- read.csv("Data/sites.csv") |>
#   mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', '7','8','9','10','11','12','12a','13','14','15','16')))
# gl_network <- read.csv("Data/greenlakes_network.csv") |>
#   left_join(sites) |>
#   mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
#   mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))


# subset, format datasets, calculate production/consumption ####
## Note these are done in a lot of steps to allow examination at each step 



## nutrients ####

### Lakes - 3 lakes have the data for this ####
nuts_prod_lakes <- nuts |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', '7','8','9',
                                                                '10','11','12','12a','13','14','15','16'))) |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  mutate(GL5 = GL5_OUTLET - GL5_INLET,
         GL4 = GL4_OUTLET - GL4_INLET,
         GL3 = GL3_OUTLET - GL3_INLET,
         GL2 = ALB_INLET - GL3_OUTLET,
         ALBION = ALB_OUTLET - ALB_INLET) |>
  ungroup() |>
  select(2:4, 21:25) |>
  pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result) |>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream, param, significance) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in lakes
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 
  


### streams - 4 reaches have the data for this ####
nuts_prod_streams <- nuts |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', '7','8','9',
                                                                '10','11','12','12a','13','14','15','16'))) |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec'))) |>
  group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  # mutate(reach1 = GL5_LAKE - ARIKAREE,
  #        reach2 = GL4_LAKE - GL5_LAKE,
  #        reach3 = GL3_LAKE - GL4_LAKE,
  #        reach4 = GL2_LAKE - GL3_LAKE,
  #        reach5 = ALB_LAKE - GL2_LAKE,
  #        reach5a = ALB_LAKE - GL1_LAKE,
  #        reach6 = ALB_CAMP - ALB_LAKE) |>
  mutate(reach1 = GL5_INLET - ARIKAREE,
         reach2 = GL4_INLET - GL5_OUTLET,
         reach3 = GL3_INLET - GL4_OUTLET,
         reach4 = GL2_LAKE - GL3_OUTLET,
         reach5 = ALB_INLET - GL2_LAKE,
         reach5a = ALB_LAKE - GL1_LAKE,
         reach6 = ALB_CAMP - ALB_OUTLET) |>
  ungroup() |>
  select(2:4, 21:27) |>
  pivot_longer(4:10, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result)|>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  #group_by(lake_stream, param, significance) |> # used to look at trend along network
  group_by(param, significance) |> # used to look at overall trend in streams
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 






## Stoichiometry ####

### Lakes ####
stoich_prod_lakes <- stoich |>
  group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  mutate(GL5 = GL5_OUTLET - GL5_INLET,
         GL4 = GL4_OUTLET - GL4_INLET,
         GL3 = GL3_OUTLET - GL3_INLET,
         GL2 = ALB_INLET - GL3_OUTLET,
         ALBION = ALB_OUTLET - ALB_INLET) |>
  ungroup() |>
  select(2:4, 21:25) |>
  pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result) |>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream, param, significance) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in lakes
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 





### Streams ####
stoich_prod_streams <- stoich |>
group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  # mutate(reach1 = GL5_LAKE - ARIKAREE,
  #        reach2 = GL4_LAKE - GL5_LAKE,
  #        reach3 = GL3_LAKE - GL4_LAKE,
  #        reach4 = GL2_LAKE - GL3_LAKE,
  #        reach5 = ALB_LAKE - GL2_LAKE,
  #        reach5a = ALB_LAKE - GL1_LAKE,
  #        reach6 = ALB_CAMP - ALB_LAKE) |>
  mutate(reach1 = GL5_INLET - ARIKAREE,
         reach2 = GL4_INLET - GL5_OUTLET,
         reach3 = GL3_INLET - GL4_OUTLET,
         reach4 = GL2_LAKE - GL3_OUTLET,
         reach5 = ALB_INLET - GL2_LAKE,
         reach5a = ALB_LAKE - GL1_LAKE,
         reach6 = ALB_CAMP - ALB_OUTLET) |>
  ungroup() |>
  select(2:4, 21:27) |>
  pivot_longer(4:10, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result)|>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(lake_stream, param, significance) |> # used to look at trend along network
  #group_by(param, significance) |> # used to look at overall trend in streams
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 


## Ions ####

### Lakes ####
ions_prod_lakes <- ions |>
  group_by(site, eco_type, date, season, param) |>
  summarise(result = mean(result)) |>
  ungroup() |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  mutate(GL5 = GL5_OUTLET - GL5_INLET,
         GL4 = GL4_OUTLET - GL4_INLET,
         GL3 = GL3_OUTLET - GL3_INLET,
         GL2 = ALB_INLET - GL3_OUTLET,
         ALBION = ALB_OUTLET - ALB_INLET) |>
  ungroup() |>
  select(2:4, 21:25) |>
  pivot_longer(4:8, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result) |>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(param, significance) |>
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 





### Streams ####
ions_prod_streams <- ions |>
  pivot_wider(names_from = site, values_from = result) |>
  group_by(eco_type, date, season, param) |>
  # mutate(reach1 = GL5_LAKE - ARIKAREE,
  #        reach2 = GL4_LAKE - GL5_LAKE,
  #        reach3 = GL3_LAKE - GL4_LAKE,
  #        reach4 = GL2_LAKE - GL3_LAKE,
  #        reach5 = ALB_LAKE - GL2_LAKE,
  #        reach5a = ALB_LAKE - GL1_LAKE,
  #        reach6 = ALB_CAMP - ALB_LAKE) |>
  mutate(reach1 = GL5_INLET - ARIKAREE,
         reach2 = GL4_INLET - GL5_OUTLET,
         reach3 = GL3_INLET - GL4_OUTLET,
         reach4 = GL2_LAKE - GL3_OUTLET,
         reach5 = ALB_INLET - GL2_LAKE,
         reach5a = ALB_LAKE - GL1_LAKE,
         reach6 = ALB_CAMP - ALB_OUTLET) |>
  ungroup() |>
  select(2:4, 21:27) |>
  pivot_longer(4:10, names_to = 'lake_stream', values_to = 'result') |>
  drop_na(result)|>
  group_by(param) |>
  # using two-tailed wilcoxon test to test if productivity is different from zero 
  mutate(p.value = (wilcox.test(result, alternative = 'two.sided'))$p.value,
         significance = ifelse(p.value > 0.05, '-',
                               ifelse(between(p.value, 0.01, 0.05), '<0.05',
                                      ifelse(between(p.value, 0.001, 0.01), '<0.01',
                                             ifelse(p.value <= 0.001, '<0.001', significance))))) |>
  ungroup() |>
  group_by(param, significance) |>
  summarise(mean = mean(result),
            median = median(result),
            min = min(result),
            max = max(result),
            SD = sd(result),
            n = n()) |>
  ungroup() 
