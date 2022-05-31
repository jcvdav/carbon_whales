######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(tidyverse)


bau <- readRDS(here("data", "output", "value_by_mortality_source.rds")) %>% 
  filter(type == "Whaling")


C_bau <- bau %>% 
  select(species, time, C_b_bau, C_p_bau, C_s_bau) %>% 
  distinct() %>% 
  pivot_longer(cols = c("C_b_bau", "C_p_bau", "C_s_bau"), names_to = "C_source", values_to = "C") %>% 
  mutate(C_source = case_when(C_source == "C_b_bau" ~ "In-body",
                              C_source == "C_p_bau" ~ "Productivity",
                              C_source == "C_s_bau" ~ "Sequestered"))

C_t_bau <- ggplot(data = C_bau, aes(x = time, y = C, color = species)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~C_source, scales = "free", ncol = 2) +
  labs(x = "Time",
       y = "Carbon (tons)",
       color = "Species") +
  theme(strip.background = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(1, 0))

ggsave(plot = C_t_bau,
       filename = here("results", "img", "C_t_bau.pdf"),
       width = 8,
       height = 4)


# C today
# C_bau %>% filter(time == 0) %>% group_by(C_source) %>% summarize(C = sum(C))

# C by 2025
# C_bau %>% filter(time == 28) %>% group_by(C_source) %>% summarize(C = sum(C))

# Sequestered by 2050
# C_bau %>% filter(C_source == "Sequestered") %>% pull(C) %>% sum()