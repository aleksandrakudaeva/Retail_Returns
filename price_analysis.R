library(Hmisc)
#price analysis
train$price_group <- as.numeric(train$item_price) %>%
  cut2(., c(seq(1,41,5), seq(51, 201, 10), 999))

price <- train %>%
  group_by(price_group) %>%
  summarise(n = n(), 
            rr = sum(return) / n)

ggplot(price, aes(price_group, n)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(price, aes(price_group, rr)) +
  geom_point(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

train <- train %>%
  group_by(price_group) %>% 
  mutate(Price_WoE = log((n() -sum(return))/sum(return)), 
         mean_price = mean(as.numeric(item_price)))

