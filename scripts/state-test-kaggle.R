library(tidyverse)
library(tidymodels)

d <- read_csv(here::here("data", "state-test-simulated.csv")) 

set.seed(8675309)
split <- initial_split(d)

train <- training(split)
test <- testing(split)

use <- replicate(nrow(test), 
                 sample(c("Public", "Private"), 1, prob = c(.3, .7)))

test <- test %>% 
  mutate(Usage = use) 

test %>% 
  select(Id = id, Expected = score, Usage) %>% 
  write_csv(here::here("data", "solutions.csv"))

test %>% 
  mutate(Predicted = map_dbl(score, ~.x + rnorm(1, sd = 200))) %>% 
  select(Id = id, Predicted) %>% 
  write_csv(here::here("data", "sample-submission.csv"))

test_data <- test %>% 
  select(-score, -classification, -Usage)

write_csv(test_data, here::here("data", "test.csv"))
write_csv(train, here::here("data", "train.csv"))
