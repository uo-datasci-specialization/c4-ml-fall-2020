library(tidyverse)
library(tidymodels)
library(xgboost)
library(tictoc)
library(sds)

set.seed(41920)
full_train <- get_data("state-test") %>% 
  slice_sample(prop = 0.5) %>% 
  select(-classification)

splt <- initial_split(full_train)
train <- training(splt)
cv <- vfold_cv(train)

rec <- recipe(score ~ ., train) %>% 
  step_mutate(tst_dt = as.numeric(lubridate::mdy_hms(tst_dt))) %>% 
  update_role(contains("id"), -ncessch, new_role = "id vars") %>%
  step_zv(all_predictors()) %>% 
  step_novel(all_nominal()) %>% 
  step_unknown(all_nominal()) %>% 
  step_medianimpute(all_numeric(), 
                    -all_outcomes(), 
                    -has_role("id vars"))  %>% 
  step_dummy(all_nominal(), 
             -has_role("id vars"),
             one_hot = TRUE) %>% 
  step_nzv(all_predictors(), freq_cut = 995/5)

mod <- boost_tree() %>% 
  set_engine("xgboost") %>% # will use the maximum number of threads
  set_mode("regression") %>% 
  set_args(nthread = 8,
           trees = 5000,
           stop_iter = 20,
           validation = 0.2,
           learn_rate = tune())

## xgb stuff
processed_train <- rec %>% 
  prep() %>% 
  bake(train)

features <- processed_train %>% 
  select(-score, -contains("id"), -ncessch) %>% 
  as.matrix()


### Grid
grd <- expand.grid(learn_rate = seq(0.001, 0.2, length.out = 20))

# Tidymodels fit
tic()
tune_tree_lr_tm <- tune_grid(mod, rec, cv, grid = grd)
toc()

saveRDS(tune_tree_lr_tm, "tune_tree_lr_tm.Rds")

# # xgb fit
# tic()
# tune_tree_lr_xgb <- map(grd$learn_rate, ~ {
#  xgb.cv(
#    data = features,
#    label = processed_train$score,,
#    nrounds = 5000, # number of trees
#    objective = "reg:squarederror", # 
#    early_stopping_rounds = 20, 
#    nfold = 10,
#    verbose = FALSE,
#    params = list(eta = .x) # learning rate = eta
#  ) 
# })
# toc()

# saveRDS(tune_tree_lr_xgb, "tune_tree_lr_xgb.Rds")
