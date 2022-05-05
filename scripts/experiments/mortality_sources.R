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

params <- readRDS(here("data", "processed", "primers.rds")) %>% 
  select(-c(KN, KNi, N_tot)) %>% 
  mutate(d_type = "KM")



# SIMULATIONS ##################################################################
plan(multisession, workers = 14)
# Establish BAU scenarios
bau <- params %>% 
  mutate(
    sim = future_pmap(
      .l = list(d_type = d_type,
                max_age = max_age,
                mature_age = mature_age,
                m = m,
                s_juvs = s_juvs,
                s_adul = s_adul,
                K = KM,
                N = N_stable,
                m_inf = m_inf,
                a0 = a0,
                k = k,
                nsteps = max_age * 2),
      .f = leslie_wraper,
      touch_at_a = 0)) %>%
  select(species, d_type, sim) %>% 
  unnest(sim) %>%
  select(species, time, V_disc_bau = V_disc, C_t_bau = C_t, C_b_bau = C_b, C_p_bau = C_p , C_s_bau = C_s, N_bau = N, D_bau = D)


# Mortality scenarios
harvest <- params %>% 
  mutate(age_touched = map(.x = max_age, .f = ~1:.x)) %>% 
  unnest(age_touched) %>% 
  mutate(M = pmap(.l = list(max_age = max_age, touch_at_a = age_touched), .f = make_wd),
         type = "Whaling") %>% 
  mutate(
    sim = future_pmap(
      .l = list(touch_at_a = age_touched,
                d_type = d_type,
                max_age = max_age,
                mature_age = mature_age,
                m = m,
                s_juvs = s_juvs,
                s_adul = s_adul,
                K = KM,
                N = N_stable,
                m_inf = m_inf,
                a0 = a0,
                k = k,
                nsteps = max_age * 2,
                H = M),
      .f = leslie_wraper))

# Simulate ship strikes (whales sink)
sink <- params %>% 
  mutate(age_touched = map(.x = max_age, .f = ~1:.x)) %>% 
  unnest(age_touched) %>% 
  mutate(M = pmap(.l = list(max_age = max_age, touch_at_a = age_touched), .f = make_wd),
         type = "Strikes") %>% 
  mutate(
    sim = future_pmap(
      .l = list(touch_at_a = age_touched,
                d_type = d_type,
                max_age = max_age,
                mature_age = mature_age,
                m = m,
                s_juvs = s_juvs,
                s_adul = s_adul,
                K = KM,
                N = N_stable,
                m_inf = m_inf,
                a0 = a0,
                k = k,
                nsteps = max_age * 2,
                S = M),
      .f = leslie_wraper))

# Simulate whale harvesting (whales don't sink)
mort <- rbind(harvest, sink) %>% 
  select(species, type, sim, age_touched) %>% 
  unnest(sim) %>% 
  ungroup() %>% 
  left_join(bau, by = c("species", "time")) %>%
  mutate(V_disc_dif = V_disc - V_disc_bau,
         C_b_dif = C_b - C_b_bau,
         C_p_dif = C_p - C_p_bau,
         C_s_dif = C_s - C_s_bau,
         C_t_dif = C_t - C_t_bau) %>% 
  ungroup()


saveRDS(object = mort,
        file = here("data", "output", "value_by_mortality_source.rds"))
