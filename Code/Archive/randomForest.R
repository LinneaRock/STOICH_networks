#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Random Forest models 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Data/CALL_DATA_PACKAGES.R') 
library(randomForest)
# library(tidymodels)
# library(yardstick)
# library(missRanger)
library(rsample)
library(caret)



# RF Q?: Can outlet stoichiometry be predicted from inlets?  ####

### summarise the data ####
# prep land cover variables for each group
drainage <- greenlakes_LC |>
  filter(Layer_1 == 'Drainage Area') |>
  select(-Layer_1) |>
  rename(drn = 2)
lu <- greenlakes_LC |>
  left_join(drainage) |>
  mutate(percent=ifelse(Layer_1 != 'Drainage Area', LandCoverArea_km2 / drn *100,
                        LandCoverArea_km2)) |>
  select(-LandCoverArea_km2) |>
  pivot_wider(names_from = Layer_1, values_from = percent) |>
  mutate_all(~replace_na(.,0))  # a couple of the subcatchements have 0% land cover 

rf.data <- stoich |>
  left_join(as.data.frame(sites |> select(site,WS_Group) |> mutate(geometry=NULL)))|>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  mutate(position = ifelse(grepl('LAKE', site), 'lake',
                           ifelse(grepl('INLET', site), 'inlet', 'outlet'))) |>
  filter(param=='tn.tp') |>
  group_by(WS_Group, date, season, position) |>
  summarise(result=mean(result)) |>
  ungroup() |>
  pivot_wider(id_cols=c('WS_Group','date', 'season'), names_from='position', values_from='result') |>
  ungroup() |>
  group_by(WS_Group, season) |>
  mutate(inlet=ifelse(is.na(inlet), mean(inlet,na.rm=TRUE), inlet),
         outlet=ifelse(is.na(outlet), mean(outlet,na.rm=TRUE), outlet),
         lake=ifelse(is.na(lake), mean(lake,na.rm=TRUE), lake)) |>
  ungroup() |>
  drop_na() |>
    # group_by(WS_Group,site, param) |>
  # summarise(mean=mean(result, na.rm=TRUE)) |>
  # ungroup() |>
  # pivot_wider(names_from='param', values_from='mean') |>
  # mutate(WS_Group = factor(rev(WS_Group))) |>
  left_join(lu) |>
  #select(-`Lake Area`) |>
 # pivot_wider(names_from='Layer_1', values_from='percent') |>
    # mutate(position = factor(position)) |>
  mutate(WS_Group = factor(WS_Group)) |>
  rename(Grassland = 'Grasslands/Herbaceous',
         Barren = 'Barren Land (Rock/Sand/Clay)',
         Shrub = 'Shrub/Scrub',
         Ice= 'Perennial Ice/Snow',
         Water = 'Open Water',
         Evergreen = 'Evergreen Forest',
         Wetlands = 'Woody Wetlands',
         Drainage = 'Drainage Area',
         LakeArea = 'Lake Area',
         DrainageArea = 'drn') |>
  select(-Drainage, -date, - WS_Group) 
  

# use 80% data for training, 20% for testing
set.seed(62693)
split <- initial_split(rf.data, prop=0.80)
training.dat <- training(split) |> mutate_if(is.numeric, round, digits=2)
testing.dat <- testing(split) |> mutate_if(is.numeric, round, digits=2) 


# model with default parameters
rf_default <- train(outlet ~.,
                    training.dat,
                    metric='RMSE',
                    method='rf',
                    tuneGrid=expand.grid(.mtry=ncol(training.dat)/3),
                    ntree=500,
                    trControl=trainControl(method='cv', number=10))

rf_default #rmse 61.92



# find best mtry
set.seed(62693)
rf_mtry <- train(outlet~.,
                 data = training.dat,
                 method = "rf",
                 metric = "RMSE",
                 tuneGrid = expand.grid(.mtry = c(1: 10)),
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          search = "grid"))

print(rf_mtry) 
plot(rf_mtry) 
# 9 for TN:TP, with RMSE of 62.83



# find best ntrees
store_maxtrees <- list()
for (ntree in c(100, 150, 250, 300, 350, 400, 450, 500, 800, 1000, 2000)) {
  set.seed(62693)
  rf_maxtrees <- train(outlet~.,
                       data = training.dat,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = expand.grid(.mtry = c(1: 10)),
                       trControl = trainControl(method = "cv",
                                                number = 10,
                                                search = "grid"),
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)

summary(results_tree) # looks like 100 is best




# fit the model with the best hyperparameters
fit_rf <- randomForest(outlet~.,
                       training.dat,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = expand.grid(.mtry = c(1: 10)),
                       trControl = trainControl(method = "cv",
                                                number = 10),
                       importance = TRUE,
                       mtry = 9,
                       ntree = 100)
# get predicted values
testing.dat$prediction <- predict(fit_rf, testing.dat)

fit_rf

varImpPlot(fit_rf)
plot(fit_rf)

varImpPlot(fit_rf, type = 1, scale = TRUE,
           n.var = ncol(rf.data) - 1, cex = 0.8,
           main = "Variable importance")

fit_rf$importance

library(reprtree)
plot.getTree(fit_rf)

ggplot(testing.dat) +
  geom_point(aes(prediction, outlet)) +
  theme_classic() +
  labs(x='Predicted TP concentration', y='Actual Values') +
  geom_abline(slope=1, intercept=0, color='red4')




# RF Q?: Can outlet nutrients be predicted from inlets?  ####
## TP or TN ####
### summarise the data ####
rf.data <- nuts |>
  left_join(as.data.frame(sites |> select(site,WS_Group) |> mutate(geometry=NULL)))|>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  mutate(position = ifelse(grepl('LAKE', site), 'lake',
                           ifelse(grepl('INLET', site), 'inlet', 'outlet'))) |>
  filter(param=='TP_umolL') |>
  group_by(WS_Group, date, season, position) |>
  summarise(result=mean(result)) |>
  ungroup() |>
  pivot_wider(id_cols=c('WS_Group','date', 'season'), names_from='position', values_from='result') |>
  ungroup() |>
  group_by(WS_Group, season) |>
  mutate(inlet=ifelse(is.na(inlet), mean(inlet,na.rm=TRUE), inlet),
         outlet=ifelse(is.na(outlet), mean(outlet,na.rm=TRUE), outlet),
         lake=ifelse(is.na(lake), mean(lake,na.rm=TRUE), lake)) |>
  ungroup() |>
  drop_na() |>
  # group_by(WS_Group,site, param) |>
  # summarise(mean=mean(result, na.rm=TRUE)) |>
  # ungroup() |>
  # pivot_wider(names_from='param', values_from='mean') |>
  # mutate(WS_Group = factor(rev(WS_Group))) |>
  left_join(lu) |>
  #select(-`Lake Area`) |>
  # pivot_wider(names_from='Layer_1', values_from='percent') |>
  # mutate(position = factor(position)) |>
  mutate(WS_Group = factor(WS_Group)) |>
  rename(Grassland = 'Grasslands/Herbaceous',
         Barren = 'Barren Land (Rock/Sand/Clay)',
         Shrub = 'Shrub/Scrub',
         Ice= 'Perennial Ice/Snow',
         Water = 'Open Water',
         Evergreen = 'Evergreen Forest',
         Wetlands = 'Woody Wetlands',
         Drainage = 'Drainage Area',
         LakeArea = 'Lake Area',
         DrainageArea = 'drn') |>
  select(-Drainage, -WS_Group, -date) 


# use 80% data for training, 20% for testing - stratified by eco_type
set.seed(62693)
split <- initial_split(rf.data, prop=0.80)
training.dat <- training(split) |> mutate_if(is.numeric, round, digits=2)
testing.dat <- testing(split) |> mutate_if(is.numeric, round, digits=2) 

# model with default parameters
rf_default <- train(outlet ~.,
                training.dat,
                metric='RMSE',
                method='rf',
                tuneGrid=expand.grid(.mtry=ncol(training.dat)/3),
                ntree=500,
                trControl=trainControl(method='cv', number=10))

rf_default



# find best mtry
set.seed(62693)
rf_mtry <- train(outlet~.,
                 data = training.dat,
                 method = "rf",
                 metric = "RMSE",
                 tuneGrid = expand.grid(.mtry = c(1: 10)),
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          search = "grid"))

print(rf_mtry) 
plot(rf_mtry) 
# 4 for TP
# 10 for TN


# find best ntrees
store_maxtrees <- list()
for (ntree in c(100, 150, 250, 300, 350, 400, 450, 500, 800, 1000, 2000)) {
  set.seed(62693)
  rf_maxtrees <- train(outlet~.,
                       data = training.dat,
                       method = "rf",
                       metric = "RMSE",
                       trControl = trainControl(method = "cv",
                                                number = 10,
                                                search = "grid"),
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)

summary(results_tree) # looks like 400 is best for TP
# for TN, 100 is best




# fit the model with the best hyperparameters
fit_rf <- randomForest(outlet~.,
                       training.dat,
                       method = "rf",
                       metric = "RMSE",
                       trControl = trainControl(method = "cv",
                                                number = 10),
                       importance = TRUE,
                       mtry = 4,
                       ntree = 400)
# get predicted values
testing.dat$prediction <- predict(fit_rf, testing.dat)

fit_rf

varImpPlot(fit_rf)
plot(fit_rf)

varImpPlot(fit_rf, type = 1, scale = TRUE,
           n.var = ncol(rf.data) - 1, cex = 0.8,
           main = "Variable importance")

fit_rf$importance

library(reprtree)
plot.getTree(fit_rf)

ggplot(testing.dat) +
  geom_point(aes(prediction, outlet)) +
  theme_classic() +
  labs(x='Predicted TP concentration', y='Actual Values') +
  geom_abline(slope=1, intercept=0, color='red4')








# # RF Q?: What drives variation along the network?  ####
# ### how many of each parameter are measured at each site? ####
# nuts_n <- nuts |>
#   group_by(site, param) |>
#   add_count() |>
#   select(site, param, n) |>
#   distinct() |>
#   left_join(as.data.frame(sites |> select(site,WS_Group) |> mutate(geometry=NULL)))
# 
# ### summarise the data to means per site ####
# # prep land cover variables for each group
# lu <- greenlakes_LC |>
#   pivot_wider(names_from = Layer_1, values_from = LandCoverArea_km2)
# 
# rf.data <- nuts |>
#   bind_rows(stoich) |>
#   left_join(as.data.frame(sites |> select(site,WS_Group) |> mutate(geometry=NULL)))|>
#   mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
#   group_by(WS_Group,site, param) |>
#   summarise(mean=mean(result, na.rm=TRUE)) |>
#   ungroup() |>
#   pivot_wider(names_from='param', values_from='mean') |>
#   mutate(WS_Group = factor(rev(WS_Group))) |>
#   left_join(lu) |>
#   select(-`Lake Area`) |>
#   mutate(RT = `Drainage Area`/ `Open Water`) |>
#   mutate_all(~replace_na(.,0)) |> # a couple of the subcatchements have 0% land cover 
#   mutate(position = ifelse(grepl('LAKE', site), 'lake',
#                            ifelse(grepl('INLET', site), 'inlet', 'outlet'))) |>
#  # mutate(position = factor(position)) |>
#   mutate(WS_Group = factor(WS_Group)) |>
#   rename(Grassland = 'Grasslands/Herbaceous',
#          Barren = 'Barren Land (Rock/Sand/Clay)',
#          Shrub = 'Shrub/Scrub',
#          Ice= 'Perennial Ice/Snow',
#          Water = 'Open Water',
#          Evergreen = 'Evergreen Forest',
#          Wetlands = 'Woody Wetlands',
#          Drainage = 'Drainage Area') |>
#   select(-site)
# 
# # use 80% data for training, 20% for testing - stratified by eco_type
# set.seed(62693)
# split <- initial_split(rf.data, strata = WS_Group, prop=0.80)
# training.dat <- training(split) |> mutate_if(is.numeric, round, digits=2)
# testing.dat <- testing(split) |> mutate_if(is.numeric, round, digits=2) 
# 
# # rf_fit <- randomForest(WS_Group ~ .,
# #                         data = training.dat,
# #                         importance=TRUE
# #                         #ntree=1000,
# #                         #mtry=10
# # )
# rf_fit <- train(WS_Group ~.,
#                 training.dat,
#                 importance=TRUE,
#                 method='rf',
#                 trControl=trainControl(method='cv', number=5))
# summary(rf_fit)
# plot(rf_fit)
# # varImpPlot(rf_fit, type = 1, scale = FALSE,
# #            n.var = ncol(rf.data) - 1, cex = 0.8,
# #            main = "Variable importance")
# 
# rf_fit # mtry=14
# 
# 
# 
# 
# 
# # Define the control: K-fold cross validation
# trControl <- trainControl(method = "cv",
#                           number = 5,
#                           search = "grid")
# 
# 
# 
# # find best mtry
# set.seed(62693)
# tuneGrid <- expand.grid(.mtry = c(1: 10))
# rf_mtry <- train(WS_Group~.,
#                  data = training.dat,
#                  method = "rf",
#                  metric = "Accuracy",
#                  tuneGrid = tuneGrid,
#                  trControl = trControl,
#                  importance = TRUE,
#                  nodesize = 14,
#                  ntree = 300)
# print(rf_mtry) 
# plot(rf_mtry) # 2 best
# best_mtry <- rf_mtry$bestTune$mtry # this says 7 is best?
# max(rf_mtry$results$Accuracy) #0.55 is best accuracy ?
# 
# 
# # find best maxnodes
# store_maxnode <- list()
# tuneGrid <- expand.grid(.mtry = best_mtry)
# for (maxnodes in c(5:15)) {
#   set.seed(62693)
#   rf_maxnode <- train(WS_Group~.,
#                       data = training.dat,
#                       method = "rf",
#                       metric = "Accuracy",
#                       tuneGrid = tuneGrid,
#                       trControl = trControl,
#                       importance = TRUE,
#                       nodesize = 14,
#                       maxnodes = maxnodes,
#                       ntree = 300)
#   current_iteration <- toString(maxnodes)
#   store_maxnode[[current_iteration]] <- rf_maxnode
# }
# results_mtry <- resamples(store_maxnode)
# summary(results_mtry)
# # they are all equal???
# 
# 
# 
# # find best ntrees
# store_maxtrees <- list()
# for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
#   set.seed(5678)
#   rf_maxtrees <- train(WS_Group~.,
#                        data = training.dat,
#                        method = "rf",
#                        metric = "Accuracy",
#                        tuneGrid = tuneGrid,
#                        trControl = trControl,
#                        importance = TRUE,
#                        nodesize = 14,
#                        maxnodes = 9,
#                        ntree = ntree)
#   key <- toString(ntree)
#   store_maxtrees[[key]] <- rf_maxtrees
# }
# results_tree <- resamples(store_maxtrees)
# summary(results_tree) #all thesame??
# 
# 
# # train the model wiht the hyperparameters
# fit_rf <- randomForest(WS_Group~.,
#                 training.dat,
#                 method = "rf",
#                 metric = "Accuracy",
#                 tuneGrid = tuneGrid,
#                 trControl = trControl,
#                 importance = TRUE,
#                 nodesize = 8,
#                 ntree = 250,
#                 maxnodes = 9)
# 
# testing.dat$prediction <- predict(fit_rf, testing.dat)
# confusionMatrix(testing.dat$prediction, testing.dat$WS_Group)
# class(fit_rf)
# varImpPlot(fit_rf)
# plot(fit_rf)
# 
# varImpPlot(fit_rf, type = 1, scale = FALSE,
#            n.var = ncol(rf.data) - 1, cex = 0.8,
#            main = "Variable importance")
# 
# fit_rf$importance
# 
# library(reprtree)
# plot.getTree(fit_rf)
# 
# 

