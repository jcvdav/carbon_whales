

library(here)
library(cowplot)
library(tidyverse)

leslie_wraper <- function(touch_at_a = NULL, d_type, a, mature_age, m, s_juvs, s_adul, K, N, nsteps){
  if(touch_at_a == 0){touch_at_a <- NULL}
  
  leslie(a = a,
         mature_age = mature_age,
         m = m,
         s_juvs = s_juvs,
         s_adul = s_adul,
         K = K,
         N = N,
         nsteps = n_steps,
         d_type = d_type,
         touch_at_a = touch_at_a) %>%
    left_join(mass_at_age, by = "age") %>%
    group_by(time) %>%
    summarize(C_b = sum(N * mass),
              C_s = sum(D * mass),
              N = sum(N)) %>%
    ungroup() %>%
    mutate(C_t = C_b + C_s,
           V = 3 * C_t,
           V_disc = V / (1 + 0.03) ^ time)
}


# von Bertalanfy
m_inf <- 117
k <- 0.2
a0 <- 0

mass_at_age <- tibble(age = 1:200) %>%
  mutate(mass = vbl(
    a = age,
    m_inf = m_inf,
    a0 = a0,
    k = k
  ))

# Parameters
a <- round(129.8)       # Maximum age
mature_age <- round(17.5)  # Age at maturity
m <- 1 / 8.4              # Fecundity
s_juvs <- 0.751          # Survival of juveniles
s_adul <- 0.955          # Survival of adults


K <- 340280                                                  # Carrying capacity
KM <- 35730693
N_tot <- 4727                                                    # Current popsize
N <- rep((N_tot / a), a)
n_steps <- 100                                                # In time

first_run <- leslie(
  a = a,
  mature_age = mature_age,
  m = m,
  s_juvs = s_juvs,
  s_adul = s_adul,
  K = K,
  N = N,
  d_type = "N",
  nsteps = n_steps
)

KNi <- first_run %>%
  filter(time == max(time)) %>%
  pull(N)

# KM <- first_run %>%
#   filter(time == max(time)) %>%
#   left_join(mass_at_age, by = "age") %>%
#   mutate(M = mass * N) %>%
#   pull(M) %>% 
#   sum()
# 
# second_run <- leslie(
#   a = a,
#   mature_age = mature_age,
#   m = m,
#   s_juvs = s_juvs,
#   s_adul = s_adul,
#   K = KM,
#   N = N,
#   d_type = "M",
#   nsteps = n_steps
# )

N_stable <- N_tot * (KNi / sum(KNi))

# Start simulations

base <- tibble(d_type = c("N","M"),
               K = list(K, KM))

bau <- base %>% 
  mutate(
    sim = pmap(
      .l = list(d_type = d_type, K = K),
      .f = leslie_wraper,
      touch_at_a = 0,
      a = a,
      mature_age = mature_age,
      m = m,
      s_juvs = s_juvs,
      s_adul = s_adul,
      N = N_stable,
      nsteps = n_steps
    )
  ) %>%
  unnest(sim) %>%
  select(d_type, time, V_disc_bau = V_disc, C_s_bau = C_s, C_b_bau = C_b, N_bau = N)


test <- base %>% 
  expand_grid(age_touched = 1:a) %>%
  mutate(
    npv = pmap(
      list(
        d_type = d_type,
        K = K,
        touch_at_a = age_touched
      ),
      .f = leslie_wraper,
      a = a,
      mature_age = mature_age,
      m = m,
      s_juvs = s_juvs,
      s_adul = s_adul,
      N = N_stable,
      nsteps = n_steps
    )
  ) %>%
  unnest(npv)

benchmarked <- test %>%
  left_join(bau, by = c("time", "d_type")) %>%
  mutate(V_disc_dif = V_disc - V_disc_bau,
         C_b_dif = C_b - C_b_bau,
         C_s_dif = C_s - C_s_bau)

benchmarked %>% 
  select(-K) %>% 
  group_by(d_type, age_touched) %>% 
  summarize_all(sum) %>% 
  select(age_touched, d_type, C_b_dif, C_s_dif) %>% 
  pivot_longer(cols = contains("C_"), names_to = "C_source", values_to = "C") %>% 
  ggplot(aes(x = age_touched, y = C, color = d_type)) +
  geom_line() +
  geom_vline(xintercept = mature_age, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~C_source, scales = "free_y", ncol = 2) +
  theme_bw()

ggplot(data = benchmarked,
       aes(x = time, y = V_disc_dif, color = age_touched, group = age_touched)) +
  geom_line() +
  facet_wrap(~d_type) +
  theme_bw()

benchmarked %>%
  group_by(d_type, age_touched) %>%
  summarize(V_disc_rat = V_disc_dif / sum(V_disc_dif),
            V_disc_dif = sum(V_disc_dif)) %>%
  ggplot(aes(x = age_touched, y = V_disc_dif, color = d_type)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mature_age) +
  theme_bw()



# Just testing
base %>% 
  expand_grid(age_touched = 1:a) %>%
  filter(str_detect(d_type, "i")) %>% 
  mutate(
    npv = pmap(
      list(
        d_type = d_type,
        K = K,
        touch_at_a = age_touched
      ),
      .f = leslie_wraper,
      a = a,
      mature_age = mature_age,
      m = m,
      s_juvs = s_juvs,
      s_adul = s_adul,
      N = N,
      nsteps = n_steps
    )
  ) %>%
  unnest(npv)


leslie(
  a = a,
  mature_age = mature_age,
  m = m,
  s_juvs = s_juvs,
  s_adul = s_adul,
  K = K,
  N = N_stable,
  d_type = "N",
  nsteps = n_steps
) %>% 
  ggplot(aes(x = time, y = N, color = age, group = age)) + 
  geom_line() +
  scale_y_log10()
