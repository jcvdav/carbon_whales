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

mort_src <- readRDS(here("data", "output", "value_by_mortality_source.rds")) %>% 
  filter(species == spp)

params <- readRDS(here("data", "processed", "primers.rds")) %>% 
  filter(species == spp)


# Make figures #################################################################

npv <- mort_src %>% 
  group_by(species, type, age_touched) %>%
  summarize(V_disc_dif = sum(V_disc_dif)) %>%
  ggplot(aes(x = age_touched, y = - V_disc_dif / 1e3, color = type)) +
  geom_vline(data = params, aes(xintercept = mature_age), linetype = "dotted") +
  geom_vline(data = params, aes(xintercept = (log(0.5) / -k) + a0), linetype = "dashed") +
  geom_vline(data = params, aes(xintercept = max_age)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Age harvested",
       y = "Implied carbon cost (Thousand USD)",
       color = "Source of mortality") +
  theme(legend.justification = c(0.5, 0),
        legend.position = c(0.5, 0),
        legend.background = element_blank(),
        strip.background = element_blank())

c_source <- mort_src %>% 
  group_by(species, type, age_touched) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  select(species, age_touched, type, C_b_dif, C_p_dif, C_s_dif) %>% 
  pivot_longer(cols = contains("C_"), names_to = "C_source", values_to = "C") %>% 
  mutate(C_source = case_when(C_source == "C_b_dif" ~ "In-body carbon",
                              C_source == "C_p_dif" ~ "Productivity stimulation",
                              C_source == "C_s_dif" ~ "Sequestered")) %>% 
  ggplot(aes(x = age_touched, y = C, color = type)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~C_source, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "Age harvested",
       y = bquote(C[mrt] - C[bau])) +
  theme(legend.position = "None",
        strip.background = element_blank())

npv_change <- mort_src %>% 
  group_by(species, type, age_touched) %>%
  summarize(V_disc_dif = -sum(V_disc_dif)) %>% 
  pivot_wider(names_from = type, values_from = V_disc_dif) %>% 
  mutate(dif = Whaling - Strikes) %>% 
  ggplot(aes(x = age_touched, y = dif)) +
  geom_line() +
  theme_bw() +
  labs(x = "Age harvested",
       y = "Difference (Whaling - Strikes; USD)")



c_dif_time <- mort_src %>%
  filter(species == spp) %>%
  filter(age_touched %in% c(1, 57, 97)) %>% 
  ggplot( mapping = aes(x = time,
                        y = C_t_dif,
                        color = factor(age_touched),
                        group = age_touched)) +
  geom_line() +
  facet_wrap( ~ type) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Time",
       y = bquote(C[mrt] - C[bau]),
       color = "Age harvested") +
  theme(strip.background = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(1, 0),
        legend.background = element_blank())


value_by_mortality <- 
  plot_grid(
    plot_grid(
      npv,
      npv_change,
      ncol = 2,
      labels = "AUTO"),
    c_source,
    ncol = 1,
    rel_heights = c(1.5, 1),
    labels = c("", "C"))

ggsave(plot = value_by_mortality,
       filename = here("results", "img", "value_by_mortality.pdf"),
       width = 8,
       height = 6)

ggsave(plot = c_dif_time,
       filename = here("results", "img", "c_dif_time.pdf"),
       width = 8,
       height = 4)