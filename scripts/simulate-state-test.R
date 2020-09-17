library(tidyverse)

d <- read_csv(here::here("data", "raw-data", "math1718.csv"))
names(d)
# d %>% 
#   count(is.na(rit_tot))

# d %>% 
#   filter(rit_tot < 10) %>% 
#   View()

d <- d %>% 
  filter(rit_tot > 0,
         srt_tst_typ != "X") %>% 
  .[ ,map_dbl(., ~length(unique(.x))) > 1]

rnorm_v <- function(mean_v, sd_v) {
  if(length(mean) != length(sd)) {
    stop("vectors are of different lengths")
  }
  map2_dbl(mean_v, sd_v, rnorm, n = 1)
}

set.seed(8675309)
d <- d %>% 
  mutate(score = rnorm_v(rit_tot, sem_tot))

# Do some double checking
ggplot(d, aes(score, rit_tot)) +
  geom_point()

d %>% 
  group_by(attnd_schl_inst_id) %>% 
  summarize(mean_raw = mean(rit_tot),
            mean_sim = mean(score)) %>% 
  ggplot(aes(mean_sim, mean_raw)) +
  geom_point()

d %>% 
  group_by(attnd_dist_inst_id) %>% 
  summarize(mean_raw = mean(rit_tot),
            mean_sim = mean(score)) %>% 
  ggplot(aes(mean_sim, mean_raw)) +
  geom_point()

# fix it up for final output
## redo cut scores
maxes <- d %>% 
  group_by(tst_bnch, pl5b_tot) %>% 
  summarize(cut = max(rit_tot)) %>% 
  filter(pl5b_tot < 4) #%>% 
 # filter(!(grepl("^X", tst_bnch) & pl5b_tot == 2)) 
  
classify <- function(score, cuts) {
  # if(length(cuts) == 1) {
  #   if(score <= cuts) { 
  #     out <- 1
  #   } else {
  #     out <- 2 
  #   }
  # }

  if(length(cuts) > 1) {
    if(score <= cuts[1]) { 
      out <- 1
    } else if(score > cuts[1] & score <= cuts[2]) {
      out <- 2
    } else if(score > cuts[2] & score <= cuts[3]) {
      out <- 3
    } else {
      out <- 4
    } 
  }
  out
}

cuts <- maxes %>% 
  group_by(tst_bnch) %>% 
  nest() %>% 
  mutate(cuts = map(data, ~sort(.x$cut))) %>% 
  select(-data)

d <- left_join(d, cuts) %>% 
  mutate(score = round(score),
         classification = map2_dbl(score, cuts, classify))

d %>% 
  count(pl5b_tot, classification) %>% 
  ggplot(aes(classification, pl5b_tot)) +
  geom_tile(aes(fill = n)) +
  colorspace::scale_fill_continuous_diverging(palette = "Blue-Red 3") +
  theme_minimal()



geo <- read_csv("https://raw.githubusercontent.com/datalorax/ach-gap-variability/master/data/achievement-gaps-geocoded.csv") %>% 
  as_tibble() %>% 
  filter(year == "1718",
         state == "OR")
  
geo <- geo %>% 
  select(district_id, school_id, ncessch, lat, lon) %>% 
  mutate(school_id = as.numeric(str_remove(school_id, "^0+"))) %>% 
  distinct(.keep_all = TRUE)


d_geo <- d %>% 
  left_join(geo, by = c(attnd_dist_inst_id = "district_id", 
                        attnd_schl_inst_id = "school_id"))
  

# write out file
d_geo %>% 
  rowid_to_column("id") %>% 
  select(-rit_tot, -cuts, -sem_tot, -pl5b_tot) %>% 
  write_csv(here::here("data", "state-test-simulated.csv"))





