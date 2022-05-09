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
  select(species, age, v) %>% 
  unnest(c(age, v))

r_value <- ggplot(data = stable, aes(x = age, y = v)) +
  geom_line(color = "steelblue") +
  facet_wrap(~species, scale = "free", ncol = 2) +
  theme_bw() +
  labs(x = "Age class",
       y = "Reproductiive value (relateive age 1)") +
  theme(strip.background = element_blank())

ggsave(plot = r_value,
       filename = here("results", "img", "rep_value.pdf"),
       width = 6,
       height = 6)
