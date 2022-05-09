######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(cowplot)
library(tidyverse)

spp <- "Gray"

value_by_density <- readRDS(here("data", "output", "value_by_density.rds")) %>% 
  filter(species == spp)

params <- readRDS(here("data", "processed", "primers.rds")) %>% 
  filter(species == spp)

# Make figures #################################################################

# NPV
npv <- value_by_density %>%
  group_by(species, d_type, age_touched) %>%
  summarize(V_disc_dif = sum(V_disc_dif)) %>%
  ungroup() %>% 
  ggplot(aes(x = age_touched, y = - V_disc_dif / 1e3, color = d_type)) +
  geom_vline(data = params, aes(xintercept = mature_age), linetype = "dotted") +
  geom_vline(data = params, aes(xintercept = (log(0.5) / -k) + a0), linetype = "dashed") +
  geom_vline(data = params, aes(xintercept = max_age)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Age harvested",
       y = "Implied carbon cost (Thousand USD)",
       color = "Density-dependence") +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_blank(),
        strip.background = element_blank())

# C_sources
c_source <- value_by_density %>% 
  group_by(species, d_type, age_touched) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  select(species, age_touched, d_type, C_b_dif, C_p_dif, C_s_dif) %>% 
  pivot_longer(cols = contains("C_"), names_to = "C_source", values_to = "C") %>% 
  mutate(C_source = case_when(C_source == "C_b_dif" ~ "In-body carbon",
                              C_source == "C_p_dif" ~ "Productivity stimulation",
                              C_source == "C_s_dif" ~ "Sequestered")) %>% 
  ggplot(aes(x = age_touched, y = C, color = d_type)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~C_source, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "Age harvested",
       y = bquote(C[hvt] - C[bau])) +
  theme(legend.position = "None",
        strip.background = element_blank())

value_by_density_plot <- 
  plot_grid(npv, c_source,
            ncol = 1,
            rel_heights = c(1.5, 1),
            labels = "AUTO")

ggsave(plot = value_by_density_plot,
       filename = here("results", "img", "value_by_density.pdf"),
       width = 8,
       height = 6)
