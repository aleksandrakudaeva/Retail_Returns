#order analysis

order <- train %>%
  group_by(user_id, delivery_date) %>% 
  dplyr::summarize(items_in_order = n(),
                   return = sum(return),
                   sizes = n_distinct(item_size), 
                   colors = n_distinct(item_color)) %>%
  mutate(description = case_when(items_in_order>1&sizes > colors ~ "1", 
                                 items_in_order>1&sizes < colors ~ "2", 
                                 items_in_order>1&sizes == colors ~ "3", 
                                 items_in_order == 1 ~ "mono"))

t4 <- order %>% 
  group_by(description) %>%
  summarise(n = sum(items_in_order), 
            return = sum(return)) %>% 
  mutate(rr = return/n)

#detect same items in one order
same <- train %>%
  group_by(user_id, delivery_date, item_id) %>% 
  dplyr::summarize(same_items = n(),
                   return = sum(return),
                   sizes = n_distinct(item_size), 
                   colors = n_distinct(item_color)) %>% 
  dplyr::mutate(rr = return/same_items)%>%
  mutate(description = case_when(same_items>1&sizes == 1 ~ "1",
                                 same_items>1&sizes > 1&colors > 1 ~ "3",
                                 same_items>1&sizes > 1&colors == 1 ~ "4",
                                 same_items == 1 ~ "mono"))

t5 <- same %>% 
  group_by(description) %>%
  summarise(n = sum(same_items), 
            return = sum(return)) %>% 
  mutate(rr = return/n)

inside_model <- glm("return ~ same_items + colors + sizes", 
                    data=same,
                    family = "gaussian")
summary(inside_model)
with(summary(inside_model), 1 - deviance/null.deviance)

