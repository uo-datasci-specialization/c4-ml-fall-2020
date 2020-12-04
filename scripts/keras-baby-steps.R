library(tidyverse)
library(keras)

# Simple linear regression example
n <- 1000   # n observations
b <- 30     # intercept
a <- 5      # slope

set.seed(123)
sim <- tibble(
  x = runif(n, min = -1, max = 1),
  y = b + a*x + rnorm(n)
)

# visualize the simulated data
ggplot(sim, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm")

# estimate with OLS
ols_model <- lm(y ~ x, sim)
coef(ols_model)
sigma(ols_model)

########### Estimate with Keras

# Transform X to a matrix
x <- matrix(sim$x, ncol = 1)

# specify a model with a single layer
sim_mod <- keras_model_sequential() %>% 
  layer_dense(units = 1, 
              activation = "linear",
              input_shape = ncol(x))

# compile the model
sim_mod %>% 
  compile(optimizer = "sgd", # stochastic gradient descent
          loss = "mse") # mean square error

# fit using mini-batch gradient descent
history <- sim_mod %>% 
  fit(x, sim$y, # data
      batch_size = 16, # mini-batch size
      epochs = 20) # n times through full training data

# check coefs w/OLS solution
get_weights(sim_mod)
coef(ols_model)
history
sigma(ols_model)


############# Non-linear example

# simulate data from a sin curve
set.seed(123)
df <- tibble(
  x = seq(from = -1, to = 2 * pi, length = n),
  e = rnorm(n, sd = 0.2),
  y = sin(x) + e
)

# plot true relation
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(aes(y = sin(x)),
            color = "cornflowerblue")

# Try to fit a model that approximates the true function

# first randomize the data
df <- df %>% 
  slice_sample(n = nrow(df)) 

# transform the x variable into a matrix
x <- matrix(df$x, ncol = 1)

# specify the model
# try varying the model width and depth
sin_mod <- keras_model_sequential() %>% 
  layer_dense(units = ___, 
              activation = ___,
              input_shape = ___) %>% 
  layer_dense(units = 1, 
              activation = "linear")

# compile and fit
sin_mod %>% 
  compile(optimizer = "sgd", 
          loss = "mse")

sin_history <- sin_mod %>% 
  fit(x, df$y,
      batch_size = ___, # mini-batch size
      epochs = ___,
      validation_split = 0.2)

# Check how well you did
# try different options above, then re-run the code below
df %>%
  mutate(pred = predict(sin_mod, x) %>% as.vector()) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_line(aes(y = sin(x)),
            color = "cornflowerblue") +
  geom_line(aes(y = pred), 
            lty = "dashed", 
            color = "red")