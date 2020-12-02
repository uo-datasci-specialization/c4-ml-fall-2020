library(tidyverse)
library(keras)
n <- 1000   # n observations
b <- 30     # intercept
a <- 5      # slope

set.seed(123)
df <- tibble(
  x = seq(from = -1, to = 2 * pi, length = n),
  e = rnorm(n, sd = 0.2),
  y = sin(x) + e
)

x <- matrix(df$x, ncol = 1)

sin_mod <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "relu", input_shape = ncol(x)) %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 1, activation = "linear") 

sin_mod %>% 
  compile(optimizer = optimizer_sgd(lr = 0.01, momentum = 0.9), #<< 
          loss = "mse")

history <- sin_mod %>% 
  fit(x, df$y,
      batch_size = 16, 
      epochs = 50,
      validation_split = .2,
      callbacks = callback_reduce_lr_on_plateau(factor = 0.1, #<<
                                                patience = 5))

df %>%
  mutate(pred = predict(sin_mod, x) %>% as.vector()) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_line(aes(y = sin(x)),
            color = "cornflowerblue") +
  geom_line(aes(y = pred), 
            lty = "dashed", 
            color = "red")


### Try again
df2 <- df[sample(seq_len(nrow(df))), ] #<<
x <- matrix(df2$x, ncol = 1) ##<<

sin_mod <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "relu", input_shape = ncol(x)) %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 1, activation = "linear") 

sin_mod %>% 
  compile(optimizer = optimizer_sgd(lr = 0.01, momentum = 0.9),
          loss = "mse")

history <- sin_mod %>% 
  fit(x, df2$y,
      batch_size = 16, 
      epochs = 50,
      validation_split = .2,
      callbacks = callback_reduce_lr_on_plateau(factor = 0.1, 
                                                patience = 5)) 

df %>%
  mutate(pred = predict(sin_mod, x) %>% as.vector()) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_line(aes(y = sin(x)),
            color = "cornflowerblue") +
  geom_line(aes(y = pred), 
            lty = "dashed", 
            color = "red")
