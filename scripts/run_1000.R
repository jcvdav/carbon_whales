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

plan(multisession, workers = 14)

runs <- read_csv(here("data", "raw", "pershing_parameters.csv")) %>% 
  filter(!species %in% c("blue", "sei", "bowhead", "bryde")) %>% 
  mutate_at(.vars = c("mature_age", "max_age"), round) %>% 
  mutate(species = str_to_sentence(species),
         N = map2(max_age, N_tot, distribute_N),
         m = 1 / calving_interval,
         s_juvs = s_juvs + 0.1,
         s_adul = s_adul + 0.02
  ) %>% 
  mutate(first_run = future_pmap(.l = list(max_age = max_age,
                                           mature_age = mature_age,
                                           m = m,
                                           s_juvs = s_juvs,
                                           s_adul = s_adul,
                                           K = KN,
                                           N = N,
                                           d_type = "KN",
                                           m_inf = m_inf,
                                           a0 = a0,
                                           k = k),
                                 .f = leslie,
                                 nsteps = 1000))


plan(sequential)

saveRDS(object = runs,
        file = here("data", "output", "runs_to_1000.rds"))