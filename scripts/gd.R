set.seed(8675309)
n <- 1000
x <- rnorm(n)
a <- 5
b <- 1.3
e <- 4
y <- a + b*x + rnorm(n, sd = e)

gd <- function(x, y, a, b, learning_rate) {
  n <- length(y)
  yhat <- a + (b * x)
  resid <- y - yhat
  a_update <- a - ((1/n) * sum(-2*resid)) * learning_rate
  b_update <- b - ((1/n) * sum(-2*x*resid)) * learning_rate
  # Return updated parameter estimates
  c(a_update, b_update)
}

params <- gd(x, y, a = 0, b = 0, 0.05)
params <- gd(x, y, params[1], params[2], 0.01)
params
