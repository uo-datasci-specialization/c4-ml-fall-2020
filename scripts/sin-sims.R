library(gganimate)
library(tidyverse)
theme_set(theme_minimal(25))

generate_data <- function() {
  n <- 200 # number of data points
  x <- seq(0, 3.5, length.out = n)
  a <- 5
  b <- 2
  error <- rnorm(n)
  amp <- 3
  
  y <- a*sin(b*x)+error*amp 

  tibble(x = x, y = y)
}
d <- rerun(30, generate_data()) %>%
  bind_rows(.id = "iteration")

line_d <- tibble(x = seq(0, 3.5, length.out = 200),
                 y = 5*sin(2*x))

p1 <- ggplot(d, aes(x, y)) + 
  geom_point(color = "gray70") +
  geom_smooth(method = "lm", 
              se = FALSE,
              fullrange = TRUE) +
  geom_line(data = line_d,
            aes(color = "Truth"),
            size = 1.2) +
  xlim(0, 3.5) +
  scale_color_manual(name = "", values = "#FF71BF") +
  transition_states(iteration,
                    transition_length = 0.5,
                    state_length = 0.5) +
  ease_aes('linear') +
  ggtitle("Sample: {closest_state}")

animate(p1, width = 1248, height = 672)
anim_save(here::here("slides", "img", "linear-sin.gif"))

p2 <- ggplot(d, aes(x, y)) + 
  geom_point(color = "gray70") +
  geom_smooth(method = "lm", 
              se = FALSE,
              formula = y ~ poly(x, 10),
              fullrange = TRUE) +
  geom_line(data = line_d,
            aes(color = "Truth"),
            size = 1.2) +
  xlim(0, 3.5) +
  scale_color_manual(name = "", values = "#FF71BF") +
  transition_states(iteration,
                    transition_length = 0.5,
                    state_length = 0.5) +
  ease_aes('linear') +
  ggtitle("Sample: {closest_state}")

animate(p2, width = 1248, height = 672)
anim_save(here::here("slides", "img", "cray-poly-sin.gif"))
