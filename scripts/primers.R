######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(furrr)
library(tidyverse)

source(here("scripts", "_functions.R"))


# Read parameters
params <- read_csv(here("data", "raw", "pershing_parameters.csv")) %>% 
  filter(!species %in% c("blue", "sei", "bowhead", "bryde")) %>% 
  mutate_at(.vars = c("mature_age", "max_age"), round) %>% 
  mutate(species = str_to_sentence(species),
         N = map2(max_age, N_tot, distribute_N),
         m = 1 / calving_interval,
         s_juvs = s_juvs + 0.1,
         s_adul = s_adul + 0.02
  ) %>% 
  mutate(M = pmap(.l = list(max_age, mature_age, m, s_juvs, s_adul),
                  .f = make_matrix),
         e = map(M, eigen),
         lambda = map_dbl(e, ~Re(.x$values[1])),
         w = map(e, ~Re(.x$vectors[,1])),
         w = map2(w, KN, ~.x / sum(.x)),
         N_stable = map2(N_tot, w, get_N_stable),
         N_equil = map2(w, KN, ~.x*.y),
         KN = map(KN, as.numeric),
         KM = map(KM, as.numeric)) %>% 
  select(-c(e, lambda))

# Export
saveRDS(object = params,
        file = here("data", "processed", "primers.rds"))
