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

value_by_age <- readRDS(here("data", "output", "value_by_age.rds")) %>% 
  filter(species == "Gray")

# Make figures #################################################################
# BAU TIMESERIES
# whale_in_time <- ggplot(data = bau %>% 
#                           filter(d_type == "KN"), aes(x = time, y = N_bau / 1e3, color = species)) +
#   geom_line(size = 1) +
#   theme_bw() +
#   scale_color_brewer(palette = "Set1") +
#   labs(x = "Time (years)",
#        y = "Whale abundance (thousands)",
#        color = "Species") +
#   theme(legend.justification = c(1, 0.5),
#         legend.position = c(1, 0.5)) +
#   theme(legend.background = element_blank())
# 
# ggsave(plot = whale_in_time,
#        filename = here("results", "img", "whale_bau_timeline.pdf"),
#        width = 6,
#        height = 3)

# NPV
npv <- value_by_age %>%
  group_by(species, d_type, age_touched) %>%
  summarize(V_disc_dif = sum(V_disc_dif),
            mature_age = unique(mature_age),
            a_m50 = unique(a_m50)) %>%
  ungroup() %>% 
  ggplot(aes(x = age_touched, y = - V_disc_dif / 1e3, color = d_type)) +
  geom_vline(aes(xintercept = mature_age), linetype = "dashed") +
  geom_vline(aes(xintercept = a_m50)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  # facet_wrap(~species, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Age harvested",
       y = "Implied carbon cost (Thousand USD)",
       color = "Density-dependence") +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_blank(),
        strip.background = element_blank())

# C_sources
c_source <- value_by_age %>% 
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
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~C_source, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "Age harvested",
       y = bquote(C[hvt] - C[bau]),
       color = "Density-dependence") +
  theme(legend.position = "bottom",
        strip.background = element_blank())

value_by_age_plot <- 
  plot_grid(npv, c_source,
            ncol = 1,
            rel_heights = c(1.5, 1),
            labels = "AUTO")

ggsave(plot = value_by_age_plot,
       filename = here("results", "img", "value_by_age.pdf"),
       width = 8,
       height = 5.3)

# value_by_age %>% 
#   filter(species == "Gray") %>% 
#   select(time, age_touched, d_type, C_b_dif, C_p_dif, C_s_dif) %>% 
#   filter(age_touched %in% seq(1, 97, by = 10)) %>% 
#   pivot_longer(cols = contains("C_"), names_to = "C_source", values_to = "C") %>% 
#   mutate(C_source = case_when(C_source == "C_b_dif" ~ "In-body carbon",
#                               C_source == "C_p_dif" ~ "Productivity stimulation",
#                               C_source == "C_s_dif" ~ "Sequestered")) %>% 
#   ggplot(aes(x = time, y = C, color = age_touched, group = paste(d_type, age_touched))) +
#   geom_line() +
#   facet_wrap(~C_source, scale = "free_y") +
#   scale_color_viridis_c() +
#   theme_bw()
