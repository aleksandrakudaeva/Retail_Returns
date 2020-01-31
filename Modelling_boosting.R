#boosting
#modelling
library(caret)
library(dplyr)
library(Hmisc)
library(rpart)

#read data
train <- read.csv2("BADS_WS1920_known.csv", 
                   sep = ",", 
                   stringsAsFactors = FALSE)
test  <- read.csv2("BADS_WS1920_unknown.csv", 
                   sep = ",", 
                   stringsAsFactors = FALSE)

#add all new variables
train <- train %>% 
  dplyr::mutate(item_price = as.numeric(item_price), 
                price_group = cut2(item_price, c(seq(1,41,5), seq(51, 201, 10), 999))) %>%
  dplyr::filter(is.na(delivery_date) == FALSE) %>%
  dplyr::mutate(unsized = ifelse(item_size == "unsized", 1, 0),
                user_yob  = substr(user_dob, 1, 4), 
                user_age  = 2017-as.numeric(user_yob), 
                age_group = case_when(user_age > 80|user_age < 18 ~ "dummy",
                                      is.na(user_age) == TRUE  ~ "missing",
                                      80 >= user_age & user_age > 55 ~ "55 - 80",
                                      55 >= user_age & user_age > 50 ~ "50 - 55",
                                      50 >= user_age & user_age > 35 ~ "35 - 50",
                                      35 >= user_age & user_age > 30 ~ "30 - 35",
                                      30 >= user_age & user_age > 25 ~ "25 - 30",
                                      25 >= user_age & user_age > 18 ~ "18 - 25"
                )) %>%
  dplyr::group_by(price_group) %>% 
  dplyr::mutate(mean_price = mean(as.numeric(item_price))) %>% 
  dplyr::group_by(user_id, order_date) %>%
  dplyr::mutate(items_pro_order = n()) %>%
  dplyr::group_by(user_id) %>%
  dplyr::mutate(items = n(), 
                colors = n_distinct(item_color)) %>%
  dplyr::group_by(user_id, item_color, items, colors) %>%
  dplyr::mutate(n = n(), 
                rc = n/items,
                cd = colors/items)%>%
  group_by(user_id, delivery_date, item_id) %>% 
  dplyr::mutate(same_items = n(),
                sizes_o = n_distinct(item_size), 
                colors_o = n_distinct(item_color)) %>%
  dplyr::ungroup() %>%
  mutate(description = case_when(same_items>1&sizes_o == 1 ~ "1",
                                 same_items>1&sizes_o > 1&colors_o > 1 ~ "3",
                                 same_items>1&sizes_o > 1&colors_o == 1 ~ "4",
                                 same_items == 1 ~ "mono"), 
         dummy_delivery = ifelse(delivery_date == "1994-12-31", 1, 0), 
         days = as.Date(user_reg_date) - as.Date(order_date), 
         to_deliv = ifelse(is.na(delivery_date), 
                           999,
                           as.Date(delivery_date) - as.Date(order_date)), 
         delivery_date = as.Date(delivery_date), 
         order_date = as.Date(order_date), 
         user_dob = as.Date(user_dob), 
         item_size = as.character(item_size), 
         user_yob = as.numeric(user_yob)
  )

#for test dataset
#add all new variables
test <- test %>% 
  dplyr::mutate(item_price = as.numeric(item_price), 
                price_group = cut2(item_price, c(seq(1,41,5), seq(51, 201, 10), 999))) %>%
  dplyr::mutate(unsized = ifelse(item_size == "unsized", 1, 0),
                user_yob  = substr(user_dob, 1, 4), 
                user_age  = 2017-as.numeric(user_yob), 
                age_group = case_when(user_age > 80|user_age < 18 ~ "dummy",
                                      is.na(user_age) == TRUE  ~ "missing",
                                      80 >= user_age & user_age > 55 ~ "55 - 80",
                                      55 >= user_age & user_age > 50 ~ "50 - 55",
                                      50 >= user_age & user_age > 35 ~ "35 - 50",
                                      35 >= user_age & user_age > 30 ~ "30 - 35",
                                      30 >= user_age & user_age > 25 ~ "25 - 30",
                                      25 >= user_age & user_age > 18 ~ "18 - 25"
                )) %>%
  dplyr::group_by(price_group) %>% 
  dplyr::mutate(mean_price = mean(as.numeric(item_price))) %>% 
  dplyr::group_by(user_id, order_date) %>%
  dplyr::mutate(items_pro_order = n()) %>%
  dplyr::group_by(user_id) %>%
  dplyr::mutate(items = n(), 
                colors = n_distinct(item_color)) %>%
  dplyr::group_by(user_id, item_color, items, colors) %>%
  dplyr::mutate(n = n(), 
                rc = n/items,
                cd = colors/items)%>%
  group_by(user_id, delivery_date, item_id) %>% 
  dplyr::mutate(same_items = n(),
                sizes_o = n_distinct(item_size), 
                colors_o = n_distinct(item_color)) %>%
  dplyr::ungroup() %>%
  mutate(description = case_when(same_items>1&sizes_o == 1 ~ "1",
                                 same_items>1&sizes_o > 1&colors_o > 1 ~ "3",
                                 same_items>1&sizes_o > 1&colors_o == 1 ~ "4",
                                 same_items == 1 ~ "mono"), 
         dummy_delivery = ifelse(delivery_date == "1994-12-31", 1, 0), 
         days = as.Date(user_reg_date) - as.Date(order_date), 
         to_deliv = ifelse(is.na(delivery_date), 
                           999,
                           as.Date(delivery_date) - as.Date(order_date)), 
         delivery_date = as.Date(delivery_date), 
         order_date = as.Date(order_date), 
         user_dob = as.Date(user_dob), 
         item_size = as.character(item_size), 
         user_yob = as.numeric(user_yob)
  )






#separate data into train and test 80:20
idx.test <- createDataPartition(y = train$return, p = 0.2, list = FALSE) 
ts <- train[idx.test, ]  # test set 
tr <- train[-idx.test, ] # train set

#Example - step-by-step gradient boosting for regression
ensemble <- list()
MSE_train <- numeric()
MSE_test <- numeric()

# Start with simple prediction
dt <- rpart(return ~ item_color + 
              item_id + 
              brand_id + 
              item_price + 
              user_title + 
              user_state + 
              unsized + 
              age_group + 
              items_pro_order + 
              items + 
              colors + 
              n + 
              rc + 
              cd + 
              same_items + 
              sizes_o + 
              colors_o + 
              description + 
              days + 
              to_deliv + 
              dummy_delivery, data=tr, cp = 0.0005, minbucket = 20, maxdepth=2)
plot(dt)
text(dt)

pred <- predict(dt, newdata=tr)
residuals <- (tr$return - pred) # Error of the prediction
ggplot(data.frame(residuals), aes(y=residuals, x="")) + geom_violin()
MSE_train[1] <- mean(residuals^2)

ensemble[[1]] <- dt
ensemble[[2]] <- rpart(residuals~item_color + 
                         item_id + 
                         brand_id + 
                         item_price + 
                         user_title + 
                         user_state + 
                         unsized + 
                         age_group + 
                         items_pro_order + 
                         items + 
                         colors + 
                         n + 
                         rc + 
                         cd + 
                         same_items + 
                         sizes_o + 
                         colors_o + 
                         description + 
                         days + 
                         to_deliv + 
                         dummy_delivery, data=tr, cp = 0.0005, minbucket = 20, maxdepth=2)
correction <- predict(ensemble[[2]], newdata=tr)

# Make an overall prediction by adding the base learners
pred_old <- predict(ensemble[[1]], newdata=tr)
pred <- predict(ensemble[[1]], newdata=tr) + correction
# See how the correction works?
cbind("true"=tr$return, pred_old, correction, pred)[1:10,]

# The improved model gives us better predictions, but it is still off
residuals <- (tr$return - pred)
ggplot(data.frame(residuals), aes(y=residuals, x="")) + geom_violin()
mean(residuals^2)

# We'll repeat this correction process, but include a shrinkage factor/learning rate
# to make smaller corrections and avoid overfitting
learning_rate = 0.35

#careful, takes time to run
for(i in 3:50){
  # Training procedure starting from the trees above
  ensemble[[i]] <- rpart(residuals~item_color + 
                           item_id + 
                           brand_id + 
                           item_price + 
                           user_title + 
                           user_state + 
                           unsized + 
                           age_group + 
                           items_pro_order + 
                           items + 
                           colors + 
                           n + 
                           rc + 
                           cd + 
                           same_items + 
                           sizes_o + 
                           colors_o + 
                           description + 
                           days + 
                           to_deliv + 
                           dummy_delivery, data=tr,cp = 0.0005, minbucket = 20, maxdepth=2)
  single_predictions <- sapply(ensemble, function(single_tree) predict(single_tree, newdata=tr))
  ensemble_prediction <- single_predictions[,1] + learning_rate * rowSums(single_predictions[,2:length(ensemble)])
  residuals <- (tr$return - ensemble_prediction)
  MSE_train[i] <- mean(residuals^2)
  
  # Test error for comparison
  test_single_predictions <- sapply(ensemble, function(single_tree) predict(single_tree, newdata=ts))
  test_ensemble_prediction <- test_single_predictions[,1] + learning_rate * rowSums(test_single_predictions[,2:length(ensemble)])
  MSE_test[i] <- mean( (ts$return - test_ensemble_prediction)^2 )
  print(paste("Finished with model number: ", i)) 
  
}
# Let's plot the residuals again for comparison
ggplot(data.frame(residuals), aes(y=residuals, x="")) + geom_violin()

#analyse performance on test set
library(hmeasure)
perf <- HMeasure(true.class=ts$return, 
                 scores = test_ensemble)


#for test dataset
# Test prediction
test_single <- sapply(ensemble, function(single_tree) predict(single_tree, newdata=test))
test_ensemble <- test_single[,1] + learning_rate * rowSums(test_single[,2:length(ensemble)])


#change the number of column to produce new model results
prediction <- data.frame("order_item_id" = test$order_item_id, 
                         "odd" = test$delivery_date,
                         "return" = test_ensemble) #change here

prediction$return <- ifelse(is.na(prediction$odd) == TRUE, 0, prediction$return) 
prediction <- prediction %>% 
  dplyr::select(-odd)%>%
  mutate(return = ifelse(return<0|is.na(return), 0, return))

# Save predictions
write.csv(prediction, file = "prediction_boosting.csv", row.names = FALSE)

#### Gradient boosting ####

library(mlr)
train$return <- as.factor(train$return)
train$item_color <- as.factor(train$item_color)
train$user_title <- as.factor(train$user_title)
train$user_state <- as.factor(train$user_state)
train$age_group <- as.factor(train$age_group)
train$description <- as.factor(train$description)
train$days <- as.numeric(train$days)
train$brand_id <- as.factor(train$brand_id)
# Prepare the mlr task
#filter train dataset
train <- train %>% 
  select(return, brand_id, item_price, user_title, user_state, 
         unsized, age_group, items_pro_order, items, colors, n, rc, cd, same_items,
         sizes_o, colors_o, description, days, to_deliv, dummy_delivery)

# Xgboost doesn't take categorical variables as input
train_dummy <- mlr::createDummyFeatures(train, target="return")
#replace NAs
for (i in names(train_dummy)) {
  train_dummy[[i]] <- ifelse(is.na(train_dummy[[i]]), 999, train_dummy[[i]])
}


#separate data into train and test 80:20
idx.test <- createDataPartition(y = train_dummy$return, p = 0.2, list = FALSE) 
ts <- train_dummy[idx.test, ]  # test set 
tr <- train_dummy[-idx.test, ] # train set

task <- makeClassifTask(data = tr, target = "return", positive = "1")

library("xgboost")
xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", # prediction type needs to be specified for the learner
                           par.vals = list("verbose" = 0,
                                           "early_stopping_rounds"=20)) # early stopping when no improvement for k iterations
xgb.learner

# Set tuning parameters
xgb.parms <- makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.05), 
  makeIntegerParam("nrounds", lower=80, upper=400), 
  makeIntegerParam("max_depth", lower=2, upper=6),
  makeDiscreteParam("gamma", values = 0),
  makeDiscreteParam("colsample_bytree", values = 1),
  makeDiscreteParam("min_child_weight", values = 1),
  makeDiscreteParam("subsample", values = 1)
)

# How dense should the parameters be selected from the ranges?
tuneControl <- makeTuneControlRandom(maxit=10, tune.threshold = FALSE)

# We do 3-fold cross-validation, given the small data more folds might be better
rdesc <- makeResampleDesc(method = "RepCV", rep = 3, folds=2, stratify = TRUE)

#To make sure we are not spending too much time, let's parallelise it
library("parallelMap")
parallelStartSocket(10, level = "mlr.tuneParams")
library("parallel")
RNGkind("L'Ecuyer-CMRG")  
clusterSetRNGStream()
#careful, takes time
# Tune parameters as before
xgb.tuning <- tuneParams(xgb.learner, 
                         task = task, 
                         resampling = rdesc,
                         par.set = xgb.parms, 
                         control = tuneControl, 
                         measures = mlr::auc)

parallelStop()

# Extract optimal parameter values after tuning 
xgb.tuning$x

# Update the learner to the optimal hyperparameters
xgb.learner <- setHyperPars(xgb.learner, par.vals = c(xgb.tuning$x, "verbose" = 0))
xgb.learner

# Train the model on the full training data (not only a CV-fold)
model_library <- list()
model_library[["xgb"]] <- mlr::train(xgb.learner, task = task)

# Also train gradient boosting with a better set of hyperparameters found by extensive grid search
xgb.learner <- setHyperPars(xgb.learner, par.vals = list("eta"=0.03,"nrounds"=300, "max_depth"=4, "verbose" = 0))
model_library[["xgb_gridsearch"]] <- mlr::train(xgb.learner, task = task)

# Train randome forest (ideally with parameters from before)

rf.learner <- makeLearner("classif.randomForest", 
                          predict.type = "prob", # prediction type needs to be specified for the learner 
                          par.vals = list(
                            "mtry" = 2, "sampsize" = 250, "ntree" = 1000,
                            "replace" = TRUE, "importance" = FALSE))
model_library[["rf"]] <- mlr::train(rf.learner, task = task)

# Make prediction on test data
pred <- sapply(model_library, predict, newdata = ts, simplify=FALSE)

# Calculate AUC performance on test set 
auc <- sapply(pred, mlr::performance, measures = mlr::auc)
# Compare the gradient boosting performance to last week's random forest
auc
# Gradient boosted trees are currently the most popular models in competitions, combining high performance and speed
# A worse performance could mean that our parameter tuning is not comprehensive enough or that the model
# just does't do well on this specific dataset

#### Heterogeneous Ensembles ####

# Collect the predictions of all models 
pred$xgb
pred_matrix <- sapply(pred, function(x) x$data$prob.1)
head(pred_matrix)
# Combine the predictions 
# This is where more complex combination algorithms exist
pred_ensemble <- rowMeans(pred_matrix[, c("xgb","xgb_gridsearch")])
cbind(head(pred_matrix), head(pred_ensemble))
# Compare the performance of the combined predictions to individual predictions
auc
ModelMetrics::auc(actual=ts$return, 
                  predicted=pred_ensemble)
