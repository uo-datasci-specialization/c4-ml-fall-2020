library(tidyverse)
ccd <- read_csv(here::here("data", "ccd_sch_059_1819_l_1a_091019.csv")) %>% 
  janitor::clean_names() %>% 
  filter(st == "OR") %>% 
  mutate(ncessch = as.double(ncessch))

train <- read_csv(here::here("data", "train.csv"))

joined <- left_join(train, ccd) 

summary(joined$teachers)

joined %>% 
  count(is.na(teachers))
