######################################################
#title#
######################################################
# 
# Plot total whale catches
# Data from: https://iwc.int/total-catches
######################################################

library(here)
library(janitor)
library(cowplot)
library(tidyverse)


iwc <- read_csv(here("data", "raw", "iwc_total_catches_timeseries.csv")) %>% 
  clean_names() %>% 
  select(year, fin, sperm, humpback, sei, brydes, minke, gray, bowhead) %>% 
  pivot_longer(cols = c(fin, sperm, humpback, sei, brydes, minke, gray, bowhead),
               names_to = "species",
               values_to = "catch") %>% 
  mutate(catch = abs(catch))



ts <- iwc %>%
  group_by(year) %>% 
  summarize(catch = sum(catch, na.rm = T)) %>% 
  ggplot(mapping = aes(x = year, y = catch)) +
  geom_line() +
  geom_point(shape = 21, fill = "steelblue", size = 2) + 
  theme_bw() +
  labs(x = "Year",
       y = "Catch\n(# of whales / yr)")

last <- iwc %>% 
  filter(year == max(year)) %>% 
  group_by(species) %>% 
  summarize(catch = sum(catch, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(species = fct_reorder(str_to_sentence(species), -catch),
         included = species %in% c("Fin", "Right", "Humpback", "Gray", "Minke")) %>% 
  ggplot(aes(x = species, y = catch, fill = included)) +
  geom_col(color = "black") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 800)) +
  scale_fill_manual(values = c("gray", "steelblue")) +
  labs(y = "Catch in 2020 (# of whales)",
       x = "Species",
       fill = "C estimate available") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank()) +
  coord_flip()


p <- plot_grid(ts, last,
               ncol = 1,
               rel_heights = c(1, 1.5),
               labels = c("AUTO"))

ggsave(plot = p,
       filename = here("results", "img", "iwc_data.pdf"),
       width = 8,
       height = 4)

# Cost fo 2020 whaling
# cost <- readRDS(here("data", "output", "removed_at_random.rds")) %>% 
#   group_by(species) %>% 
#   summarize(cost = mean(V_disc_dif))
# 
# iwc %>% 
#   filter(year == max(year)) %>% 
#   group_by(species) %>% 
#   summarize(catch = sum(catch, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(species = str_to_sentence(species)) %>% 
#   left_join(cost, by = "species") %>% 
#   mutate(tc = catch * cost) %>% 
#   pull(tc) %>% 
#   sum(na.rm = T) * 1e3
