######################################################
#title#
######################################################
# 
# Whales
#
######################################################

library(here)
library(cowplot)
library(tidyverse)


# von Bertalanfy
m_inf <- 117
k <- 0.2
a0 <- 1

mass_at_age <- tibble(age = 1:200) %>% 
  mutate(mass = vbl(a = age, m_inf = m_inf, a0 = a0, k = k))

# Parameter sampling
set.seed(42)
n_runs <- 1                                               # Number of random simulations
a <- round(rnorm(n = n_runs, mean = 129.8, sd = 11.5))       # Maximum age
mature_age <- round(rnorm(n = n_runs, mean = 7.5, sd = 1.5))  # Age at maturity
m <- 1 / rnorm(n = n_runs, mean = 8.4, sd = 2.9)              # Fecundity
s_juvs <- rnorm(n = n_runs, mean = 0.751, sd = 0.092)         # Survival of juveniles
s_adul <- rnorm(n = n_runs, mean = 0.955, sd = 0.02)          # Survival of adults

K <- 340280                                                  # Carrying capacity
N <- 4727                                                    # Current popsize
n_steps <- 10                                               # In time

if(n_runs == 1){
  a <- round(129.8)       # Maximum age
  mature_age <- round(7.5)  # Age at maturity
  m <- 1 / 8.4              # Fecundity
  s_juvs <- 0.751          # Survival of juveniles
  s_adul <- 0.955          # Survival of adults
}

bau <- tibble(run = 1:n_runs, a = a, mature_age = mature_age, m = m, s_juvs = s_juvs, s_adul = s_adul) %>% 
  mutate(sim = pmap(.l = list(a = a, mature_age = mature_age, m = m, s_juvs = s_juvs, s_adul = s_adul),
                    .f = leslie,
                    K = K,
                    N = N,
                    nsteps = n_steps)) %>% 
  select(run, sim) %>% 
  unnest(sim) %>% 
  left_join(mass_at_age, by = "age") %>% 
  group_by(run, time) %>% 
  summarize(C_b = sum(N * mass),
            C_s = sum(D * mass),
            N = sum(N)) %>% 
  ungroup() %>% 
  mutate(scenario = "BAU")

new_sjuvs <- s_juvs + ((1 - s_juvs) * 0.9)
con1 <- tibble(run = 1:n_runs, a = a, mature_age = mature_age, m = m, s_juvs =  new_sjuvs, s_adul = s_adul) %>% 
  mutate(sim = pmap(.l = list(a = a, mature_age = mature_age, m = m, s_juvs = s_juvs, s_adul = s_adul),
                    .f = leslie,
                    K = K,
                    N = N + 10,
                    nsteps = n_steps)) %>% 
  select(run, sim) %>% 
  unnest(sim) %>% 
  left_join(mass_at_age, by = "age") %>% 
  group_by(run, time) %>% 
  summarize(C_b = sum(N * mass),
            C_s = sum(D * mass),
            N = sum(N)) %>% 
  ungroup() %>% 
  mutate(scenario = "Pol1")


new_sadul <- s_adul + ((1 - s_adul) * 0.9)
con2 <- tibble(run = 1:n_runs, a = a, mature_age = mature_age, m = m, s_juvs =  s_juvs, s_adul = new_sadul) %>% 
  mutate(sim = pmap(.l = list(a = a, mature_age = mature_age, m = m, s_juvs = s_juvs, s_adul = s_adul),
                    .f = leslie,
                    K = K,
                    N = N + 10,
                    nsteps = n_steps)) %>% 
  select(run, sim) %>% 
  unnest(sim) %>% 
  left_join(mass_at_age, by = "age") %>% 
  group_by(run, time) %>% 
  summarize(C_b = sum(N * mass),
            C_s = sum(D * mass),
            N = sum(N)) %>% 
  ungroup() %>% 
  mutate(scenario = "Pol2")

n_plot <- rbind(bau, con1, con2) %>%
  ggplot(aes(x = time, y = N / 1e3, linetype = scenario)) + 
  geom_line() +
  theme_bw() +
  labs(x = "Time",
       y = "Number of whales",
       linetype = "Scenario") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank())

m_plot <- rbind(bau, con1, con2) %>% 
  mutate(C_t = C_b + C_s) %>% 
  ggplot(aes(x = time, y = C_t / 1e6, linetype = scenario)) + 
  geom_line() +
  theme_bw() +
  labs(x = "Time",
       y = "Whale carbon") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "None")


dift <- rbind(bau, con1, con2) %>% 
  mutate(C_t = C_b + C_s) %>% 
  select(run, time, C_t, scenario) %>% 
  spread(scenario, C_t) %>% 
  mutate(Pol2 = Pol2 - BAU,
         Pol1 = Pol1 - BAU) %>% 
  select(time, Pol2, Pol1) %>% 
  pivot_longer(cols = c("Pol2", "Pol1"), names_to = "scenario", values_to = "C_t") %>% 
  ggplot(aes(x = time, y = C_t / 1e6, linetype = scenario)) + 
  geom_line() +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  theme_bw() +
  labs(x = "Time",
       y = "Carbon relative to BAU") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "None")

dif <- rbind(bau, con1, con2) %>% 
  mutate(C_t = C_b + C_s) %>% 
  mutate(V = 35 * C_t,
         V_disc = V / (1 + 0.05) ^ time) %>% 
  select(run, time, V_disc, scenario) %>% 
  spread(scenario, V_disc) %>% 
  mutate(Pol2 = Pol2 - BAU,
         Pol1 = Pol1 - BAU) %>% 
  select(time, Pol2, Pol1) %>% 
  pivot_longer(cols = c("Pol2", "Pol1"), names_to = "scenario", values_to = "C_t") %>% 
  group_by(scenario) %>% 
  summarize(C = sum(C_t)) %>% 
  ggplot(aes(x = scenario, y = C / 1e6, linetype = scenario)) +
  geom_col(color = "black", size = 1, fill = "transparent") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Policy",
       y = "NPV of policy") +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  scale_y_continuous(breaks = 0) +
  theme_bw() +
  theme(legend.position = "None")

p <- cowplot::plot_grid(n_plot, m_plot, dift, dif, ncol = 2, labels = "AUTO")

ggsave(plot = p,
       filename = here("results", "img", "panel_figure.tiff"),
       width = 6,
       height = 4)


cols <- rbind(bau, con1, con2) %>% 
  mutate(C_t = C_b + C_s) %>% 
  mutate(V = 35 * C_t,
         V_disc = V / (1 + 0.05) ^ time) %>% 
  group_by(scenario) %>% 
  summarize(V = sum(V_disc) / 1e6) %>% 
  ungroup() %>% 
  mutate(scenario = fct_reorder(scenario, V, .desc = T)) %>% 
  ggplot(aes(x = scenario, y = V)) +
  geom_col(fill = "steelblue", color = "black") +
  scale_y_continuous(labels = "", breaks = NULL) +
  theme_bw() +
  labs(x = "Scenario", y = "Present value of policy (M$)")

cowplot::plot_grid(cowplot::plot_grid(n_plot, leg, m_plot, ncol = 3, rel_widths = c(1, 0.3, 1)), cols, ncol = 1)



