library(xgboost)
tic()
def_mod <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 5000,
  objective = "reg:linear",
  early_stopping_rounds = 20, 
  nfold = 10,
  verbose = 0
) 
toc()
def_mod$evaluation_log[def_mod$best_iteration, ]

pull_eval <- function(m) {
  m[["evaluation_log"]] %>% 
    pivot_longer(-iter,
                 names_to = c("set", NA, "stat"),
                 names_sep = "_",
                 values_to = "val") %>% 
    pivot_wider(names_from = "stat", 
                values_from = "val") 
}

def_mod %>% 
  pull_eval() %>% 
  filter(iter > 7) %>% 
  ggplot(aes(iter, mean, color = set)) +
  geom_line() +
  geom_point()

######### Train learning rate 
lr <- seq(0.0001, 0.3, length.out = 30)

lr_mods <- map(lr, function(learn_rate) {
  xgb.cv(
    data = X,
    label = Y,
    nrounds = 5000,
    objective = "reg:linear",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = learn_rate,
      nthread = 16
    ) 
  )  
}) 

names(lr_mods) <- lr
evals_lr <- map_df(lr_mods, pull_eval, .id = "learning_rate")

rmse_lr <- evals_lr %>% 
  group_by(learning_rate, set) %>% 
  summarize(min = min(mean)) %>% 
  pivot_wider(names_from = set, values_from = min) %>% 
  arrange(test)

rmse_lr %>% 
  ungroup() %>% 
  mutate(learning_rate = as.numeric(learning_rate)) %>% 
  filter(test < 120) %>% 
  ggplot(aes(learning_rate, test)) +
  geom_point()

# Check learning curves
lr_mods[[rmse_lr$learning_rate[1]]] %>% 
  pull_eval() %>% 
  filter(mean < 120) %>% 
  ggplot(aes(iter, mean, color = set)) +
  geom_line() +
  geom_point()

# Set learning rate, tune tree specific parameters
grid <- grid_max_entropy(min_n(c(0, 50)), # min_child_weight
                         tree_depth(), # max_depth
                         size = 30)

tree_mods <- map2(grid$min_n, grid$tree_depth, ~{
  xgb.cv(
    data = X,
    label = Y,
    nrounds = 5000,
    objective = "reg:linear",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = as.numeric(rmse_lr$learning_rate[1]),
      max_depth = .x,
      min_child_weight = .y,
      nthread = 16
    ) 
  )  
}) 
