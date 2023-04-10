#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Random Forest 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Data/CALL_DATA_PACKAGES.R') 

nuts.setup <- nuts |>
  mutate(year=year(date)) |>
  select(-site, -depth_m, -date) |>
  group_by(network_position, eco_type, season, year,param) |>
  summarise(med=median(result)) |>
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  select(-NH4_ueqL, - NO3_ueqL, -DOC_mgL)

nuts.setup$network_position <- factor(nuts.setup$network_position)
nuts.setup$season <- factor(nuts.setup$season)
nuts.setup$eco_type <- factor(nuts.setup$eco_type)

nuts.setup <- rfImpute(network_position~., nuts.setup)

rf_nuts <- randomForest(network_position~.,
                        data = nuts.setup,
                        importance=TRUE
                        #ntree=1000,
                        #mtry=10
                        )
rf_nuts
plot(rf_nuts)
varImpPlot(rf_nuts, type = 1, scale = FALSE,
           n.var = ncol(nuts.setup) - 1, cex = 0.8,
           main = "Variable importance")
