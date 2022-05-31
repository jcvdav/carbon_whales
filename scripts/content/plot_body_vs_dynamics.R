######################################################
#title#
######################################################
# 
# Source
#
######################################################

library(here)
library(cowplot)
library(tidyverse)

spp <- "Gray"

mort_src <- readRDS(here("data", "output", "value_by_mortality_source.rds")) %>% 
  filter(species == spp,
         type == "Whaling")

body_c_at_age <- mort_src %>% 
  filter(time == 0) %>% 
  mutate(sink = ifelse(type == "Strikes", 0.5, 1),
         V_disc_b_dif = sink * scc_t * C_b_dif * 3.67) %>% 
  select(species, type, age_touched, V_disc_b_dif)

df <- mort_src %>% 
  mutate() %>% 
  group_by(species, type, age_touched) %>%
  summarize(V_disc_dif = sum(V_disc_dif)) %>%
  left_join(body_c_at_age, by = c("species", "type", "age_touched")) %>% 
  mutate(pct_b = V_disc_b_dif / V_disc_dif,
         V_disc_dif = V_disc_dif - V_disc_b_dif) 

VvV <- ggplot(data = df,
       mapping = aes(x = - V_disc_b_dif / 1e3, y = - V_disc_dif / 1e3,size = age_touched)) +
  geom_point(shape = 21, fill = "steelblue", alpha = 0.5) +
  theme_bw() +
  labs(x = "Implied cost from in-body C (Thousand USD)",
       y = "Implied carbon cost (Thousand USD)",
       size = "Age harvested") +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank(),
        strip.background = element_blank())


pct_age <- ggplot(data = df,
       mapping = aes(x = age_touched, y = pct_b)) +
  geom_line(color = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "Age harvested",
       y = "% cost from in-body C",
       color = "Source of mortality")


body_vs_dynamics <- plot_grid(VvV, pct_age, ncol = 2, labels = "AUTO")

ggsave(plot = body_vs_dynamics,
       filename = here("results", "img", "body_vs_dynamics.pdf"),
       width = 8,
       height = 4)

# Ranges for text

# Value of carbon in biomass
# body_c_at_age$V_disc_b_dif %>% range()

# Value of carbon in biomass as % of total value
# df$pct_b %>% range() * 100
