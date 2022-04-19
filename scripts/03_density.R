

library(here)
library(cowplot)
library(furrr)
library(tidyverse)




# Read parameters

params <- read_csv(here("data", "raw_data", "pershing_parameters.csv")) %>% 
  filter(!species %in% c("blue", "sei", "bowhead", "bryde")) %>% 
  # filter(species == "gray") %>% 
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
  mutate(first_run = pmap(.l = list(max_age = max_age,
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
         KM = map(KM, as.numeric)) %>% 
  select(-first_run)


# SIMULATIONS ##################################################################
plan(multisession, workers = 12)
# Establish BAU scenarios
bau <- params %>% 
  pivot_longer(cols = c("KM", "KN", "KNi"),
               names_to = "d_type",
               values_to = "K") %>% 
  # filter(d_type == "KN") %>%
  # filter(species == "Fin") %>% 
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
  select(species, d_type, time, V_disc_bau = V_disc, C_b_bau = C_b, C_p_bau = C_p , C_s_bau = C_s, N_bau = N)


# Run harvesting scenarios

pols <- params %>% 
  pivot_longer(cols = c("KM", "KN", "KNi"),
               names_to = "d_type",
               values_to = "K") %>% 
  # filter(d_type == "KNi") %>% 
  mutate(age_touched = map(.x = max_age, .f = ~1:.x)) %>% 
  unnest(age_touched) %>% 
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
                nsteps = max_age * 2),
      .f = leslie_wraper))

plan(sequential)

#### Organize simulations


benchmarked <- pols %>%
  select(species, d_type, sim, age_touched, mature_age) %>% 
  unnest(sim) %>% 
  left_join(bau, by = c("species", "time", "d_type")) %>%
  mutate(V_disc_dif = V_disc - V_disc_bau,
         C_b_dif = C_b - C_b_bau,
         C_p_dif = C_p - C_p_bau,
         C_s_dif = C_s - C_s_bau,
         d_type = case_when(d_type == "KM" ~ "Biomass",
                            d_type == "KN" ~ "Abundance",
                            d_type == "KNi" ~ "Age-specific"))


# Make figures #################################################################
# BAU TIMESERIES
whale_in_time <- ggplot(data = bau %>% 
                          filter(d_type == "KN"), aes(x = time, y = N_bau / 1e3, color = species)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Time (years)",
       y = "Whale abundance (thousands)",
       color = "Species")

ggsave(plot = whale_in_time,
       filename = here("results", "img", "whale_bau_timeline.pdf"),
       width = 6,
       height = 4)

# NPV
npv <- benchmarked %>%
  group_by(species, d_type, age_touched) %>%
  summarize(V_disc_rat = V_disc_dif / sum(V_disc_dif),
            V_disc_dif = sum(V_disc_dif)) %>%
  ungroup() %>% 
  ggplot(aes(x = age_touched, y = - V_disc_dif / 1e3, color = d_type)) +
  # geom_vline(xintercept = params$mature_age, linetype = "dashed") +
  # geom_vline(xintercept = params$m_inf) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  facet_wrap(~species, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Age harvested",
       y = "Implied carbon cost (Thousand USD)",
       color = "Density-dependence") +
  theme(legend.justification = c(1, 0),
        legend.position = c(0.9, 0),
        legend.background = element_blank())

ggsave(plot = npv,
       filename = here("results", "img", "npv.pdf"),
       width = 8,
       height = 5.3)

# C_sources
c_source <- benchmarked %>% 
  group_by(species, d_type, age_touched) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  select(species, age_touched, d_type, C_b_dif, C_p_dif, C_s_dif) %>% 
  pivot_longer(cols = contains("C_"), names_to = "C_source", values_to = "C") %>% 
  mutate(C_source = case_when(C_source == "C_b_dif" ~ "In-body carbon",
                              C_source == "C_p_dif" ~ "Productivity stimulation",
                              C_source == "C_s_dif" ~ "Sequestered")) %>% 
  ggplot(aes(x = age_touched, y = C, color = d_type)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~C_source, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "Age harvested",
       y = bquote(C[con] - C[bau]),
       color = "Density-dependence") +
  theme(legend.position = "bottom",
        strip.background = element_blank())

benchmarked %>% 
  select(time, age_touched, d_type, C_b_dif, C_p_dif, C_s_dif) %>% 
  filter(age_touched %in% seq(1, 97, by = 10)) %>% 
  pivot_longer(cols = contains("C_"), names_to = "C_source", values_to = "C") %>% 
  mutate(C_source = case_when(C_source == "C_b_dif" ~ "In-body carbon",
                              C_source == "C_p_dif" ~ "Productivity stimulation",
                              C_source == "C_s_dif" ~ "Sequestered")) %>% 
  ggplot(aes(x = time, y = C, color = age_touched, group = paste(d_type, age_touched))) +
  geom_line() +
  facet_wrap(~C_source, scale = "free_y") +
  scale_color_viridis_c() +
  theme_bw()

ggsave(plot = c_source, 
       filename = here("results", "img", "blue_c_npv_source.pdf"),
       width = 6,
       height = 2)
