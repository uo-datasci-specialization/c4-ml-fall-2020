# ReLU is a great starting point for activation functions for hidden layers. 
# For the output layer, you need a linear, sigmoid, or softmax activatin 
# function, depending on if you have a regression, binomial classification, or 
# multiclass classification problem, respectively. This script basically just 
# shows that it doesn't work well for output layers, but does work well for
# hidden layers 

library(tidyverse)
library(keras)

# Simulate some data that are all negative
set.seed(42820)
n <- 500

x <- rnorm(n)

a <- -200
b <- -5
e <- 10

y <- a + b*x + rnorm(n, sd = e)

X <- matrix(x, ncol = 1)
  
tibble(x, y) %>% 
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm")

coef(lm(y ~ x))

# Try to model this with a ReLU output layer
model <- keras_model_sequential() %>% 
  layer_dense(units = 1, 
              activation = "relu", 
              input_shape = ncol(X)) 

model %>% 
  compile(loss = "mse",
          optimizer = "sgd")

history <- model %>% 
  fit(X, y,
      batch_size = 32,
      epochs = 100,
      validation_split = .2)


plot(history)
# As seen above, the model does not really fit at all

# We end up with zero for all our predictions
cbind(y, predict(model, x))

# Try the same model with a linear output layer
model2 <- keras_model_sequential() %>% 
  layer_dense(units = 1, 
              activation = "linear", 
              input_shape = ncol(X)) 

model2 %>% 
  compile(loss = "mse",
          optimizer = "sgd")

history2 <- model2 %>% 
  fit(X, y,
      batch_size = 32,
      epochs = 100,
      validation_split = .2)

history2

# Plot looks much better
plot(history2)

# Coefficients basically match
get_weights(model2)

# Predictions are much closer
tibble(x, y) %>% 
  mutate(pred = predict(model2, x) %>% as.vector()) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_line(aes(y = pred),
            color = "red")



######## Use ReLU for a hidden layer
# This is just a straight linear model, so we're not going to increase
# performance by including hidden layers, but just to prove to ourselves
# the ReLU activation functions work on hidden layers, let's fit a small
# multi-layer perceptron network

# Unfortunately, however, this still doesn't work unless we center the outcome
# This shouldn't really matter, because we can just add it back on at the end
# but it is sort of interesting that it won't work (or at least I can't get it 
# to work) without the centering trick.


# First without centering

model3 <- keras_model_sequential() %>% 
  layer_dense(units = 8, activation = "relu", input_shape = ncol(X)) %>% 
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 1, activation = "linear") 

model3 %>% 
  compile(loss = "mse",
          optimizer = "sgd")

history3 <- model3 %>% 
  fit(X, y,
      batch_size = 256,
      epochs = 500,
      validation_split = 0.2)

# plot predictions
tibble(x, y) %>% 
  mutate(pred = predict(model3, x) %>% as.vector()) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_line(aes(y = pred),
            color = "red")

# Notice that the prediction is negative, but it's also a constant,
# which is obviously not good. We can fix this by subtracting a constant
# from each value. This doesn't actually have to be the mean.


model4 <- keras_model_sequential() %>% 
  layer_dense(units = 8, activation = "relu", input_shape = ncol(X)) %>% 
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 1, activation = "linear") 

model4 %>% 
  compile(loss = "mse",
          optimizer = "sgd")

history4 <- model4 %>% 
  fit(X, y - -200, # subtract negative 200
      batch_size = 256,
      epochs = 500,
      validation_split = 0.2)

# plot predictions
tibble(x, y) %>% 
  mutate(pred = predict(model4, x) %>% as.vector() - 200) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_line(aes(y = pred),
            color = "red")

# Voila

# So ReLU *is* working here, but it seems to have issues when 
# *every* value in the outcome is very negative. I found the 
# centering trick from searching around online for why this was
# happening.

# Let's try one more time with a ReLU output using the centering trick


model5 <- keras_model_sequential() %>%
  layer_dense(units = 1, activation = "relu") 

model5 %>% 
  compile(loss = "mse",
          optimizer = "sgd")

history5 <- model5 %>% 
  fit(X, y - -200, # subtract negative 200
      batch_size = 256,
      epochs = 500,
      validation_split = 0.2)

# plot predictions
tibble(x, y) %>% 
  mutate(pred = predict(model5, x) %>% as.vector() - 200) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_line(aes(y = pred),
            color = "red")

# Hey! Look at that. Sure looks a lot like ReLU! (although inverse, of course)