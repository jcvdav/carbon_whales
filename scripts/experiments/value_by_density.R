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

source(here("scripts", "_functions.r"))

params <- readRDS(here("data", "processed", "primers.rds"))



# SIMULATIONS ##################################################################
plan(multisession, workers = 14)
# Establish BAU scenarios
bau <- params %>% 
  pivot_longer(cols = c("KM", "KN", "KNi"),
               names_to = "d_type",
               values_to = "K") %>% 
    mutate(
    sim = future_pmap(
      .l = list(d_type = d_type,
                max_age = max_age,
                mature_age = mature_age,
                m = m,
                s_juvs = s_juvs,
                s_adul = s_adul,
                K = K,
                N = N_stable,
                m_inf = m_inf,
                a0 = a0,
                k = k,
                nsteps = max_age * 2),
      .f = leslie_wraper,
      touch_at_a = 0)) %>%
  select(species, d_type, sim) %>% 
  unnest(sim) %>%
  select(species, d_type, time, V_disc_bau = V_disc, C_b_bau = C_b, C_p_bau = C_p , C_s_bau = C_s, N_bau = N, D_bau = D)


# Run harvesting scenarios
pols <- params %>% 
  pivot_longer(cols = c("KM", "KN", "KNi"),
               names_to = "d_type",
               values_to = "K") %>% 
  mutate(age_touched = map(.x = max_age, .f = ~1:.x)) %>% 
  unnest(age_touched) %>% 
  mutate(M = pmap(.l = list(max_age = max_age, touch_at_a = age_touched), .f = make_wd)) %>% 
  mutate(
    sim = future_pmap(
      .l = list(touch_at_a = age_touched,
                d_type = d_type,
                max_age = max_age,
                mature_age = mature_age,
                m = m,
                s_juvs = s_juvs,
                s_adul = s_adul,
                K = K,
                N = N_stable,
                m_inf = m_inf,
                a0 = a0,
                k = k,
                nsteps = max_age * 2,
                H = M),
      .f = leslie_wraper))

plan(sequential)

#### Organize simulations


benchmarked <- pols %>%
  select(species, d_type, sim, age_touched) %>% 
  unnest(sim) %>% 
  ungroup() %>% 
  left_join(bau, by = c("species", "time", "d_type")) %>%
  mutate(V_disc_dif = V_disc - V_disc_bau,
         C_b_dif = C_b - C_b_bau,
         C_p_dif = C_p - C_p_bau,
         C_s_dif = C_s - C_s_bau,
         d_type = case_when(d_type == "KM" ~ "Biomass",
                            d_type == "KN" ~ "Abundance",
                            d_type == "KNi" ~ "Age-specific")) %>% 
  ungroup()

# EXPORT DATA ##################################################################
saveRDS(object = benchmarked,
        file = here("data", "output", "value_by_density.rds"))
