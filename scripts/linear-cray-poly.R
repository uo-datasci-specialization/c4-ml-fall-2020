library(gganimate)
library(tidyverse)

diamond_samps <- replicate(10, 
                     diamonds[sample(seq_len(nrow(diamonds)), size = 100), ],
                     simplify = FALSE) %>%
  bind_rows(.id = "sample") %>%
  mutate(sample = as.numeric(sample))

ggplot(diamond_samps, aes(carat, price)) + 
  geom_point(color = "gray70") +
  geom_smooth(method = "lm", 
              se = FALSE,
              fullrange = TRUE) +
  xlim(0, 3) +
  transition_states(sample,
                    transition_length = 0.5,
                    state_length = 0.5) +
  ease_aes('linear') +
  ggtitle("Sample: {closest_state}")

anim_save("linear.gif")

ggplot(diamond_samps, aes(carat, price)) + 
  geom_point(color = "gray70") + 
  geom_smooth(method = "lm", 
              se = FALSE,
              formula = y ~ poly(x, 10),
              fullrange = TRUE) +
  xlim(0, 3) +
  ylim(0, 17000) +
  transition_states(sample,
                    transition_length = 0.5,
                    state_length = 0.5) +
  ease_aes('linear') +
  ggtitle("Sample: {closest_state}")

anim_save("cray-poly.gif")