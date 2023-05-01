#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Random Forest models 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Data/CALL_DATA_PACKAGES.R') 
library(randomForest)
# library(tidymodels)
# library(yardstick)
library(missRanger)
library(rsample)
library(caret)

# Q1: can nutrient dynamics alone predict lake vs stream? And can we pick out source/sink information from this classification? ####

# summarise the data to seasonal, yearly medians at each site
nuts_setup <- nuts |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  mutate(year=year(date)) |>
  select(-site, -depth_m, -date) |>
  group_by(network_position, eco_type, year, season, param) |>
  summarise(med=median(result, na.rm=TRUE)) |>
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  mutate(eco_type = factor(eco_type))

# impute missing values using missRanger package 
nuts_imp <- rfImpute(eco_type~., nuts_setup)
# count the number of non-missing values per row
#non_miss <- rowSums(!is.na(nuts_setup))
#nuts_imp2 <- missRanger(nuts_setup, num.trees = 20, pmm.k = 3, seed = 5, case.weights = non_miss)


rf.data <- nuts_imp |>
  dplyr::select(-network_position, -year, - season) 

# use 60% data for training, 40% for testing - stratified by eco_type
set.seed(62693)
split <- initial_split(rf.data, strata = eco_type, prop=0.60)
training.dat <- training(split) |> mutate_if(is.numeric, round, digits=2)
testing.dat <- testing(split) |> mutate_if(is.numeric, round, digits=2) 

rf_fit <- randomForest(eco_type ~ .,
                        data = training.dat,
                        importance=TRUE
                        #ntree=1000,
                        #mtry=10
)
summary(rf_fit)
plot(rf_fit)
varImpPlot(rf_fit, type = 1, scale = FALSE,
           n.var = ncol(nuts_imp2) - 1, cex = 0.8,
           main = "Variable importance")



# Define the control: K-fold cross validation
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")


set.seed(62693)
rf_default <- train(eco_type~.,
                    data=training.dat,
                    method='rf',
                    metric='Accuracy',
                    trControl=trControl)
print(rf_default)


# find best mtry
set.seed(62693)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(eco_type~.,
                 data = training.dat,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)

best_mtry <- rf_mtry$bestTune$mtry

max(rf_mtry$results$Accuracy)


# find best maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(62693)
  rf_maxnode <- train(eco_type~.,
                      data = training.dat,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
# i think 7 is the best maxnode



# find best ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(eco_type~.,
                       data = training.dat,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 9,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)


# train the model wiht the hyperparameters
fit_rf <- randomForest(eco_type~.,
                training.dat,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 250,
                maxnodes = 9)

prediction <- predict(fit_rf, testing.dat)
confusionMatrix(prediction, testing.dat$eco_type)
class(fit_rf)
varImpPlot(fit_rf)
plot(fit_rf)

varImpPlot(fit_rf, type = 1, scale = FALSE,
           n.var = ncol(rf.data) - 1, cex = 0.8,
           main = "Variable importance")

fit_rf$importance

library(reprtree)
plot.getTree(fit_rf)

library(rpart)
library(yardstick)
cart <- rpart(eco_type~., training.dat, method='class', cp=0.01)
testing.dat$predict <- predict(cart, testing.dat, 'class')
conf_mat(testing.dat, eco_type, predict)
accuracy(testing.dat, eco_type, predict)
printcp(cart)
bestcp <- cart$cptable[which.min(cart$cptable[,'xerror']), 'CP']
CART_mod_pruned <- prune(cart, cp=bestcp)

plot(CART_mod_pruned)
text(CART_mod_pruned, cex=0.8, use.n=TRUE, xpd=TRUE)




# Q2: what drives differences between inlets and outlets among various nutrients? ####

# summarise the data to seasonal, yearly medians at each site
nuts_setup <- nuts |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL'),
         eco_type=='stream') |>
  mutate(position = ifelse(grepl('INLET', site), 'inlet', 'outlet')) |>
  mutate(year=year(date)) |>
  select(-site, -depth_m, -date) |>
  group_by(network_position, position, year, season, param) |>
  summarise(med=median(result, na.rm=TRUE)) |>
  ungroup() |>
  pivot_wider(names_from='param', values_from='med') |>
  mutate(position=factor(position))

# impute missing values using missRanger package 
nuts_imp <- rfImpute(position~., nuts_setup)
# count the number of non-missing values per row
#non_miss <- rowSums(!is.na(nuts_setup))
#nuts_imp2 <- missRanger(nuts_setup, num.trees = 20, pmm.k = 3, seed = 5, case.weights = non_miss)


rf.data <- nuts_imp |>
  dplyr::select(-network_position, -year, - season) 

# use 60% data for training, 40% for testing - stratified by eco_type
set.seed(62693)
split <- initial_split(rf.data, strata = position, prop=0.60)
training.dat <- training(split) |> mutate_if(is.numeric, round, digits=2)
testing.dat <- testing(split) |> mutate_if(is.numeric, round, digits=2) 

rf_fit <- randomForest(position ~ .,
                       data = training.dat,
                       importance=TRUE
                       #ntree=1000,
                       #mtry=10
)
summary(rf_fit)
plot(rf_fit)
varImpPlot(rf_fit, type = 1, scale = FALSE,
           n.var = ncol(nuts_imp2) - 1, cex = 0.8,
           main = "Variable importance")



# Define the control: K-fold cross validation
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")


set.seed(62693)
rf_default <- train(position~.,
                    data=training.dat,
                    method='rf',
                    metric='Accuracy',
                    trControl=trControl)
print(rf_default)


# find best mtry
set.seed(62693)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(position~.,
                 data = training.dat,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)

best_mtry <- rf_mtry$bestTune$mtry

max(rf_mtry$results$Accuracy)


# find best maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(62693)
  rf_maxnode <- train(position~.,
                      data = training.dat,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
# i think 7 is the best maxnode



# find best ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(position~.,
                       data = training.dat,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 9,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)


# train the model wiht the hyperparameters
fit_rf <- randomForest(position~.,
                       training.dat,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       ntree = 250,
                       maxnodes = 9)

prediction <- predict(fit_rf, testing.dat)
confusionMatrix(prediction, testing.dat$position)
class(fit_rf)
varImpPlot(fit_rf)
plot(fit_rf)

varImpPlot(fit_rf, type = 1, scale = FALSE,
           n.var = ncol(rf.data) - 1, cex = 0.8,
           main = "Variable importance")

fit_rf$importance[,1:3]/fit_rf$importanceSD[,1:3]

library(reprtree)
plot.getTree(fit_rf)

