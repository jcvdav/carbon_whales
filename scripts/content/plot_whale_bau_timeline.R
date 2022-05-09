######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(tidyverse)

params <- readRDS(here("data", "processed", "primers.rds")) %>% 
  select(species, KN) %>% 
  unnest(KN)

# Load data
bau <- readRDS(here("data", "output", "value_by_density.rds")) %>% 
  ungroup() %>% 
  filter(age_touched == 1,
         d_type == "Abundance") %>%
  select(species, time, N_bau) %>% 
  distinct() %>% 
  left_join(params, by = "species")

bau_timeline <- ggplot(data = bau,
       mapping = aes(x = time, y = N_bau / KN, color = species)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "Time",
       y = "Relative whale abundance (N/K)",
       color = "Species") +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0),
        legend.background = element_blank())

ggsave(plot = bau_timeline,
       filename = here("results", "img", 'whale_bau_timeline.pdf'),
       width = 6,
       height = 4.5)
