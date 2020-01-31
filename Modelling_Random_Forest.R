library(randomForest)
library(dplyr)
library(Hmisc)
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

train <- train %>% 
  dplyr::filter(is.na(delivery_date) == FALSE) %>%
  dplyr::mutate(unsized = ifelse(item_size == "unsized", 1, 0),
                user_yob  = substr(user_dob, 1, 4), 
                user_age  = 2017-as.numeric(user_yob), 
                age_group = case_when(user_age > 80|user_age <= 18 ~ "dummy",
                                      is.na(user_age) == TRUE  ~ "missing",
                                      80 >= user_age & user_age > 55 ~ "55 - 80",
                                      55 >= user_age & user_age > 50 ~ "50 - 55",
                                      50 >= user_age & user_age > 35 ~ "35 - 50",
                                      35 >= user_age & user_age > 30 ~ "30 - 35",
                                      30 >= user_age & user_age > 25 ~ "25 - 30",
                                      25 >= user_age & user_age > 18 ~ "18 - 25"
                )) %>%
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

#separate data into train and test 80:20
idx.test <- createDataPartition(y = train$return, p = 0.2, list = FALSE) 
ts <- train[idx.test, ]  # test set 
tr <- train[-idx.test, ] # train set

rf <- randomForest(
  return ~ item_price + items_pro_order + age_group + cd,
  data=tr, 
  na.action = na.omit
)

library(hmeasure)
performance3 <- HMeasure(true.class=ts$return, 
                         scores = predict(rf, 
                                          newdata = ts,
                                          type="response"))
performance3$metrics

unknown <- predict(rf, newdata = test, type = "response")

#change the number of column to produce new model results
prediction <- data.frame("order_item_id" = test$order_item_id, 
                         "odd" = test$delivery_date,
                         "return" = unknown) #change here

prediction$return <- ifelse(is.na(prediction$odd) == TRUE, 0, prediction$return)
prediction <- prediction %>% 
  dplyr::select(-odd)

# Save predictions
write.csv(prediction, file = "prediction_rf.csv", row.names = FALSE)

