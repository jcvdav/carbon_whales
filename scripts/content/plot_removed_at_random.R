######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(ggridges)
library(cowplot)
library(tidyverse)

# Load data
rnd_mort <- readRDS(here("data", "output", "removed_at_random.rds"))

# FIGURES ######################################################################
mean_cost <- rnd_mort %>% 
  mutate(species = fct_reorder(species, -V_disc_dif)) %>% 
  ggplot(mapping = aes(x = species, y = V_disc_dif)) +
  stat_summary(geom = "pointrange",
               fun.data = mean_sdl,
               fun.args = list(mult = 1),
               color = "black",
               shape = 21,
               fill = "steelblue",
               size = 1) +
  theme_bw() +
  labs(x = "Species",
       y = "Implied carbon cost (Thousand USD)")

dens <- ggplot(data = rnd_mort,
       mapping = aes(x = age_touched, y = species, fill = species)) +
  geom_density_ridges(bandwidth = 5, fill = "steelblue", alpha = 0.5) +
  theme_bw() +
  labs(x = "Age harvested",
       y = "Density")

plot <- 
  plot_grid(mean_cost, dens,
            ncol = 1,
            rel_heights = c(1.5, 1),
            labels = "AUTO")

ggsave(plot = plot,
       filename = here("results", "img", "value_by_species.pdf"),
       height = 6,
       width = 8)

ggsave(plot = plot,
       filename = here("results", "img", "value_by_species.png"),
       height = 6,
       width = 8)


# Data for text
# rnd_mort %>% 
#   group_by(species) %>% 
#   summarize(m = mean(V_disc_dif),
#             sd = sd(V_disc_dif)) %>% 
#   arrange(desc(m))
