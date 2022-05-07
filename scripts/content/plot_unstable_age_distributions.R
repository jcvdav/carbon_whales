######################################################
#title#
######################################################
# 
# Purpose
#
######################################################


library(here)
library(tidyverse)

unstable <- readRDS(here("data", "output", "runs_to_1000.rds")) %>% 
  select(species, first_run) %>% 
  unnest(first_run) %>% 
  filter((time %% 10) == 0)

n_unstable <- ggplot(data = unstable, aes(x = time, y = N, color = age, group = age)) +
  geom_line() +
  facet_wrap(~species, scales = "free_y", ncol = 2) +
  scale_y_log10() +
  theme_bw() +
  scale_color_viridis_c() +
  guides(color = guide_colorbar(frame.colour = "black",
                                ticks.colour = "black")) +
  labs(x = "Time",
       y = bquote(log[10]~"(Population size)"),
       color = "Age class") +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0),
        strip.background = element_blank())

ggsave(plot = n_unstable,
       filename = here("results", "img", "unstable_age_distributions.pdf"),
       width = 6,
       height = 6)
