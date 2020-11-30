library(tidyverse)
library(tidymodels)
library(xgboost)
library(tictoc)

set.seed(41920)
d <- read_csv("data/train.csv") %>% 
  select(-classification) %>% 
  sample_frac(0.05)

splt <- initial_split(d)
train <- training(splt)
cv <- vfold_cv(train)

rec <- recipe(score ~ ., train) %>% 
  step_mutate(tst_dt = as.numeric(lubridate::mdy_hms(tst_dt))) %>% 
  update_role(contains("id"), -ncessch, new_role = "id vars") %>%
  step_zv(all_predictors()) %>% 
  step_novel(all_nominal()) %>% 
  step_unknown(all_nominal()) %>% 
  step_medianimpute(all_numeric(), -all_outcomes(), -has_role("id vars"))  %>% 
  step_dummy(all_nominal(), -has_role("id vars"),
             one_hot = TRUE) %>% 
  step_nzv(all_predictors(), freq_cut = 995/5)

mod <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("regression") %>% 
  set_args(stop_iter = 20) # early stopping

tune_lr <- mod %>% 
  set_args(trees = 5000,
           learn_rate = tune(),
           stop_iter = 20)

wf_tune_lr <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(tune_lr)

## xgb stuff
processed_train <- rec %>% 
  prep() %>% 
  bake(train)

features <- processed_train %>% 
  select(-score, -contains("id"), -ncessch) %>% 
  as.matrix()

outcome <- processed_train$score


### Grid
grd <- expand.grid(learn_rate = seq(0.001, 0.3, length.out = 15))

# Tidymodels fit
tic()
tune_tree_lr <- tune_grid(wf_tune_lr, cv, grid = grd)
toc()

# xgb fit
tic()
tune_tree_lr_xgb <- map(grd$learn_rate, ~ {
  xgb.cv(
    data = features,
    label = outcome,
    nrounds = 5000, # number of trees
    objective = "reg:squarederror", # 
    early_stopping_rounds = 20, 
    nfold = 10,
    params = list(eta = .x), #<<
    verbose = FALSE
  ) 
})
toc()

