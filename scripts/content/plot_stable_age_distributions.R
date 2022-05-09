######################################################
#title#
######################################################
# 
# Purpose
#
######################################################


library(here)
library(tidyverse)

stable <- readRDS(here("data", "processed", "primers.rds")) %>% 
  mutate(age = map(max_age, ~1:.x)) %>% 
  select(species, age, w) %>% 
  unnest(c(age, w))

n_stable <- ggplot(data = stable, aes(x = age, y = w)) +
  geom_col(fill = "steelblue", color = "black") +
  facet_wrap(~species, scale = "free", ncol = 2) +
  theme_bw() +
  labs(x = "Age class",
       y = "Proportion of population in age-class") +
  theme(strip.background = element_blank())
  
ggsave(plot = n_stable,
       filename = here("results", "img", "stable_age_distributions.pdf"),
       width = 6,
       height = 6)
