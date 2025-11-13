######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

pacman::p_load(
  here,
  ggridges,
  cowplot,
  tidyverse
)

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
       y = "Implied carbon cost\n(Thousand USD)")

dens <- ggplot(data = rnd_mort,
       mapping = aes(x = age_touched, y = species)) +
  geom_density_ridges(fill = "steelblue",
                      alpha = 0.5,
                      stat = "binline") +
  theme_bw() +
  labs(x = "Age-at-mortality (years)",
       y = "Density")

plot <- 
  plot_grid(mean_cost, dens,
            ncol = 1,
            rel_heights = c(1.5, 1),
            align = "hv",
            labels = "AUTO")

ggsave(plot = plot,
       filename = here("results", "img", "value_by_species.pdf"),
       width = 6,
       height = 4.5)

ggsave(plot = plot,
       filename = here("results", "img", "value_by_species.png"),
       width = 6,
       height = 4.5)

ggsave(plot = mean_cost,
       filename = here("results", "img", "value_by_species_top.pdf"),
       width = 6,
       height = 3.5)

ggsave(plot = mean_cost,
       filename = here("results", "img", "value_by_species_top.png"),
       width = 6,
       height = 3.5)


# Data for text
rnd_mort %>%
  group_by(species) %>%
  summarize(m = mean(V_disc_dif),
            sd = sd(V_disc_dif)) %>%
  arrange(desc(m))
