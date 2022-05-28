
library(here)
library(cowplot)
library(tidyverse)

spp <- "Gray"

value_N0 <- readRDS(here("data", "output", "value_by_N0.rds")) %>% 
  filter(species == spp)

params <- readRDS(here("data", "processed", "primers.rds")) %>% 
  filter(species == spp)

max_at_age <- value_N0 %>% 
  group_by(species, K_fact, age_touched) %>%
  summarize(V_disc_dif = sum(V_disc_dif)) %>% 
  group_by(K_fact) %>%
  slice_min(V_disc_dif)

npv <- value_N0 %>% 
  group_by(species, K_fact, age_touched) %>%
  summarize(V_disc_dif = sum(V_disc_dif)) %>%
  ggplot(aes(x = age_touched, y = -V_disc_dif/1e3, color = K_fact, group = K_fact)) +
  geom_vline(data = params, aes(xintercept = mature_age), linetype = "dotted") +
  geom_vline(data = params, aes(xintercept = (log(0.5) / -k) + a0), linetype = "dashed") +
  geom_vline(data = params, aes(xintercept = max_age)) +
  geom_line(size = 1) +
  geom_point(data = max_at_age, aes(x = age_touched, y = -V_disc_dif/1e3), color = "black", size = 4, shape = 21) +
  scale_color_viridis_c() +
  theme_bw() +
  guides(color = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "Age harvested",
       y = "Implied carbon cost (Thousand USD)",
       color = bquote(N[0]/K)) +
  theme(legend.justification = c(0, 0),
        legend.position = c(0.3, 0),
        legend.background = element_blank())


# min_at_age %>% 
#   select(K_fact, age_touched) %>% 
#   left_join(value_N0, by = c("K_fact", "age_touched")) %>% 
#   ggplot(aes(x = time, y = C_t - C_t_bau, group = K_fact)) +
#   geom_line()
# 
# value_N0 %>% 
#   filter(age_touched == 1) %>% 
#   ggplot(aes(x = time, y = C_t - C_t_bau, group = K_fact)) +
#   geom_line()
# 
# 
#   group_by(species, K_fact, age_touched) %>%
#   summarize(V_disc_dif = sum(V_disc_dif)) %>% 
#   ggplot(aes(x = age_touched, y = K_fact, fill = -V_disc_dif / 1e3, z = -V_disc_dif / 1e3)) +
#   geom_raster() +
#   geom_contour(color = "black") +
#   scale_fill_viridis_c()

ggsave(plot = npv, filename = here("results", "img", "value_by_N0.pdf"),
       width = 8,
       height = 4.5)
