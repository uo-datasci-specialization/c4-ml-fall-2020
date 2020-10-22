library(tidyverse)
library(tidymodels)

full_train <- read_csv("data/train.csv")
splt <- initial_split(full_train)
train <- training(splt)
train_cv <- vfold_cv(train)

# basic recipe
rec <- recipe(score ~ ., data = train)  %>% 
  step_mutate(tst_dt = lubridate::mdy_hms(tst_dt)) %>%
  update_role(contains("id"), ncessch, new_role = "id vars")  %>% 
  step_unknown(all_nominal())  %>% 
  step_nzv(all_predictors(), freq_cut = 0, unique_cut = 0) %>% 
  step_medianimpute(all_numeric(), -all_outcomes(), -has_role("id vars"))  %>% 
  step_dummy(all_predictors(), -all_numeric(), -tst_dt)  %>% 
  step_nzv(all_predictors())

# linear regression model
mod <- linear_reg()  %>% 
  set_mode("regression")  %>% 
  set_engine("lm")

fit1 <- fit_resamples(mod, rec, train_cv)
saveRDS(fit1, "fit1.Rds")
