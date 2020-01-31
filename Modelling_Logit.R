#modelling
library(caret)

#read data
train <- read.csv2("BADS_WS1920_known.csv", 
                   sep = ",", 
                   stringsAsFactors = FALSE)
test  <- read.csv2("BADS_WS1920_unknown.csv", 
                   sep = ",", 
                   stringsAsFactors = FALSE)

#add all new variables
train$item_price <- as.numeric(train$item_price)
train$price_group <- cut2(train$item_price, c(seq(1,41,5), seq(51, 201, 10), 999))

train <- train %>% 
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
  mutate(description = case_when(same_items>1&sizes_o == 1 ~ "1",
                                 same_items>1&sizes_o > 1&colors_o > 1 ~ "3",
                                 same_items>1&sizes_o > 1&colors_o == 1 ~ "4",
                                 same_items == 1 ~ "mono"), 
         dummy_delivery = ifelse(delivery_date == "1994-12-31", 1, 0))

#separate data into train and test 80:20
idx.test <- createDataPartition(y = train$return, p = 0.2, list = FALSE) 
ts <- train[idx.test, ]  # test set 
tr <- train[-idx.test, ] # train set


#logistic regression
model1 <- glm("return ~ item_price + items_pro_order + unsized + age_group", 
              data=tr,
              family = "binomial")

library(hmeasure)
performance1 <- HMeasure(true.class=ts$return, 
                         scores = predict(model1, 
                                          newdata = ts, 
                                          type="response"))
performance1$metrics

#logistic regression
model2 <- glm("return ~ item_price + items_pro_order + unsized + age_group + cd + description", 
              data=tr,
              family = "binomial")

library(hmeasure)
performance2 <- HMeasure(true.class=ts$return, 
                         scores = predict(model2, 
                                          newdata = ts, 
                                          type="response"))
performance2$metrics

#logistic regression
model3 <- glm("return ~ item_price + items_pro_order + unsized + age_group + cd + description + dummy_delivery", 
              data=tr,
              family = "binomial")

library(hmeasure)
performance3 <- HMeasure(true.class=ts$return, 
                         scores = predict(model2, 
                                          newdata = ts, 
                                          type="response"))
performance3$metrics

#logistic regression4
model4 <- glm("return ~ item_price + 
              items_pro_order + 
              unsized + 
              age_group + 
              cd + 
              description + 
              dummy_delivery +
              to_deliv", 
              data=tr,
              family = "binomial")

library(hmeasure)
performance4 <- HMeasure(true.class=ts$return, 
                         scores = predict(model4, 
                                          newdata = ts, 
                                          type="response"))
performance4$metrics

summary(model4)

#analyze false predictions
#change the number of column to produce new model results
model_test <- glm("return ~ item_price + items_pro_order + unsized + age_group + cd + description", 
                  data=train[!is.na(train$delivery_date),],
                  family = "binomial")

analysis <- data.frame("order_item_id" = train$order_item_id, 
                       "dd" = train$delivery_date,
                       "real" = train$return,
                       "result" = predict(model_test, 
                                          newdata = train, 
                                          type="response")) 
analysis$result_bin <- ifelse(analysis$result >=0.5, 1, 0)
analysis$True <- ifelse(analysis$result_bin == analysis$real, 1, 0)


#test
#calculate new variables for test set
#mean price
test$item_price <- as.numeric(test$item_price)
test$price_group <- cut2(test$item_price, c(seq(1,41,5), seq(51, 201, 10), 999))

test <- test %>% 
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
  mutate(description = case_when(same_items>1&sizes_o == 1 ~ "1",
                                 same_items>1&sizes_o > 1&colors_o > 1 ~ "3",
                                 same_items>1&sizes_o > 1&colors_o == 1 ~ "4",
                                 same_items == 1 ~ "mono"))

#prediction
pred_unknown1 <- predict(model1, newdata = test, type = "response")
pred_unknown2 <- predict(model2, newdata = test, type = "response")

#change the number of column to produce new model results
prediction <- data.frame("order_item_id" = test$order_item_id, 
                         "odd" = test$delivery_date,
                         "return" = pred_unknown2) #change here

prediction$return <- ifelse(is.na(prediction$odd) == TRUE, 0, prediction$return)
prediction <- prediction %>% 
  dplyr::select(-odd)

# Save predictions
write.csv(prediction, file = "prediction_model2.csv", row.names = FALSE)
