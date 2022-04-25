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


plan(multisession, workers = 14)

# Read parameters
params <- read_csv(here("data", "raw_data", "pershing_parameters.csv")) %>% 
  filter(!species %in% c("blue", "sei", "bowhead", "bryde")) %>% 
  mutate_at(.vars = c("mature_age", "max_age"), round) %>% 
  mutate(species = str_to_sentence(species),
         N_tot = N_tot / 2,
         M_tot = M_tot / 2,
         KN = KN / 2,
         KM = KM / 2,
         N = map2(max_age, N_tot, distribute_N),
         m = 2 / calving_interval,
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
                          nsteps = 1000)) %>% 
  mutate(N_stable = map2(N_tot, first_run, get_N_stable),
         KNi = map2(first_run, KN, get_Ki),
         KN = map(KN, as.numeric),
         KM = map(KM, as.numeric))

plan(sequential)

# Export
saveRDS(object = params,
        file = here("data", "processed_data", "primers.rds"))
