#read data
library("dplyr")
library("stringr")

train <- read.csv2("BADS_WS1920_known.csv", 
                   sep = ",", 
                   stringsAsFactors = FALSE)
test  <- read.csv2("BADS_WS1920_unknown.csv", 
                   sep = ",", 
                   stringsAsFactors = FALSE)
backup <- train

####sizes####
table(train$item_size)

size <- train %>% 
  group_by(item_size) %>%
  summarise(n = n(), 
            rr = sum(return)/n)

View(size)

universal <- train %>% 
  filter(item_size %in% c("xs", "s", "m", "l", "xl", "xxl", "xxxl", "unsized"))%>% 
  group_by(return, item_size) %>% 
  summarize(n = n())

#plot histogram
ggplot(universal, aes(fill = return, y = n, x = item_size)) +
  geom_bar(position = "fill", stat = "identity")+
  theme(legend.position = "none")

universal <- train %>% 
  filter(item_size %in% c("xs", "s", "m", "l", "xl", "xxl", "xxxl", "unsized"))

ggplot(universal, aes(item_size)) + geom_bar(stat="count")

#plus sizes
train <- train %>%
  mutate(size_group = ifelse(str_detect(item_size, "[+]"), "plus", ""))

plus <- train %>%
  filter(size_group == "plus")%>% 
  group_by(return, item_size) %>% 
  summarize(n = n())

#plot histogram
ggplot(plus, aes(fill = return, y = n, x = item_size)) +
  geom_bar(position = "fill", stat = "identity")+
  theme(legend.position = "none")

#create dummy if unsized
train <- train %>%
  mutate(unsized = ifelse(item_size == "unsized", 1, 0))


#see sizing scales of different brands
#get brands and sizes from train and test samples to get more extensive table 
t1 <- backup %>% 
  dplyr::select(brand_id, item_size)
t2 <- test %>% 
  dplyr::select(brand_id, item_size)

brand_scale <- rbind(t1, t2) %>%
  dplyr::group_by(brand_id) %>% 
  dplyr::summarise(n = n(), 
                   n_sizes = n_distinct(item_size), 
                   unique_sizes = paste(shQuote(unique(item_size), type = "cmd2"), collapse = ", "))

for (i in 1:dim(brand_scale)[1]) {
  a = unlist(strsplit(brand_scale$unique_sizes[i], split=", "))
}

EU_schuhgroesse <- c(34,35,36,37,38,39,40,41,42,43,44,45,46,47,48)
US_schuhgroesse <- c(6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5)
Aus_schuhgroesse<- c(6,7,8,9,10,11,12)
GB_schuhgroesse <- c(4,5,6,7,8,9,10)
