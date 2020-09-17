set.seed(8675309)
n <- 1000
x <- rnorm(n)

a <- 5
b1 <- 2.3
b2 <- 0.5
b3 <- 1.2

e <- 6

y <- a + b1*x + b2*x^2 + b3*x^3 + rnorm(n, sd = e)

tibble(x, y) %>% 
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3))


gd <- function(x1, x2, x3, y, a, b1, b2, b3, learning_rate) {
  n <- length(y)
  yhat <- a + (b1 * x1) + (b2 * x2) + (b3 * x3)
  resid <- y - yhat
  
  # update theta
  a_update <- a - ((1/n) * sum(-2*resid)) * learning_rate
  b1_update <- b1 - ((1/n) * sum(-2*x1*resid)) * learning_rate
  b2_update <- b2 - ((1/n) * sum(-2*x2*resid)) * learning_rate
  b3_update <- b3 - ((1/n) * sum(-2*x3*resid)) * learning_rate
  
  # Return updated parameter estimates
  c(a_update, b1_update, b2_update, b3_update)
}  

mse <- function(x1, x2, x3, y, a, b1, b2, b3) {
  pred <- a + (b1 * x1) + (b2 * x2) + (b3 * x3)
  resid2 <- (y - pred)^2
  1/length(y)*sum(resid2)
}

estimate_gradient <- function(x1, x2, x3, y, a, b1, b2, b3, learning_rate, iter) {
  pars <- gd(x1, x2, x3, y, a, b1, b2, b3, learning_rate)
  
  c(iter, pars[1], pars[2], pars[3], pars[4], 
    mse(x1, x2, x3, y, a, b1, b2, b3))
}

# Estimate parameters w/gradient descent
iter <- 1200

# set up empty data frame
estimates <- data.frame(iteration = integer(iter),
                        intercept = double(iter),
                        b1 = double(iter),
                        b2 = double(iter),
                        b3 = double(iter),
                        cost = double(iter))

# store first row of estimates
estimates[1, ] <- estimate_gradient(x, x^2, x^3, y, a = 0, b1 = 0, b2 = 0, b3 = 0, learning_rate =  0.01, 1)

# Estimate remain rows, using previous row as input
for(i in 2:iter) {
  estimates[i, ] <- estimate_gradient(x, x^2, x^3, y,
                                      a = estimates$intercept[i - 1],
                                      b1 = estimates$b1[i - 1],
                                      b2 = estimates$b2[i - 1],
                                      b3 = estimates$b3[i - 1],
                                      learning_rate = 0.01,
                                      iter = i)
}
tail(estimates)
coef(lm(y ~ poly(x, 3, raw = TRUE)))

ggplot(estimates, aes(iteration, cost)) +
  geom_point() +
  geom_line()

pred_frame <- tibble(x = seq(-3.5, 3.5, 0.1),
                     x2 = x^2,
                     x3 = x^3)

compute_pred <- function(a, b1, b2, b3) {
  pred <- a + b1*pred_frame$x + b2*pred_frame$x2 + b3*pred_frame$x3
  tibble(x = pred_frame$x, pred)
}

predictions <- estimates %>% 
  group_by(iteration) %>% 
  nest() %>% 
  mutate(pred = map(data, ~compute_pred(.x$intercept, .x$b1, .x$b2, .x$b3))) %>% 
  select(iteration, pred) %>% 
  unnest(pred)

library(gganimate)

sim_d <- tibble(x, y)

ggplot(predictions[1:(300*71), ]) +
  geom_point(aes(x, y), sim_d) +
  geom_smooth(aes(x, y), sim_d, 
              method = "lm", 
              formula = y ~ poly(x, 3),
              se = FALSE) +
  geom_line(aes(x = x, y = pred, group = iteration),
              color = "#de4f60") +
  transition_manual(frames = iteration)

anim_save(here::here("slides", "img", "poly-gd.gif"))
