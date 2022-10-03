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
n_steps <- 500                                               # In time

if(n_runs == 1){
  a <- round(129.8)       # Maximum age
  mature_age <- round(7.5)  # Age at maturity
  m <- 1 / 4              # Fecundity
  s_juvs <- 0.851          # Survival of juveniles
  s_adul <- 0.955          # Survival of adults
}

bau <- leslie_wraper(max_age = a,
                     mature_age = mature_age,
                     m = m,
                     s_juvs = s_juvs,
                     s_adul = s_adul,
                     K = K,
                     N = distribute_N(max_age = a, N_tot = N),
                     nsteps = n_steps,
                     m_inf = m_inf,
                     k = k,
                     a0 = a0,
                     d_type = "KN",
                     touch_at_a = 0) %>% 
  mutate(scenario = "BAU")

new_sjuvs <- s_juvs - ((1 - s_juvs) * 0.1)
con1 <- leslie_wraper(max_age = a,
                      mature_age = mature_age,
                      m = m,
                      s_juvs = new_sjuvs,
                      s_adul = s_adul,
                      K = K,
                      N = distribute_N(max_age = a, N_tot = N),
                      nsteps = n_steps,
                      m_inf = m_inf,
                      k = k,
                      a0 = a0,
                      d_type = "KN",
                      touch_at_a = 0) %>% 
  mutate(scenario = "Pol1")


new_sadul <- s_adul + ((1 - s_adul) * 0.9)
con2 <- leslie_wraper(max_age = a,
                      mature_age = mature_age,
                      m = m,
                      s_juvs = s_juvs,
                      s_adul = new_sadul,
                      K = K,
                      N = distribute_N(max_age = a, N_tot = N),
                      nsteps = n_steps,
                      m_inf = m_inf,
                      k = k,
                      a0 = a0,
                      d_type = "KN",
                      touch_at_a = 0) %>% 
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
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank())

c_plot <- rbind(bau, con1, con2) %>% 
  ggplot(aes(x = time, y = C_t / 1e6, linetype = scenario)) + 
  geom_line() +
  theme_bw() +
  labs(x = "Time",
       y = "Carbon") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme(legend.position = "None")

v_disc_t <- rbind(bau, con1, con2) %>%
  ggplot(aes(x = time, y = V_disc, linetype = scenario)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line() +
  theme_bw() +
  labs(x = "Time",
       y = "Discounted value") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = 0) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme(legend.position = "None")


dift <- rbind(bau, con1, con2) %>% 
  select(time, V_disc, scenario) %>% 
  spread(scenario, V_disc) %>% 
  mutate(Pol2 = Pol2 - BAU,
         Pol1 = Pol1 - BAU) %>% 
  select(time, Pol2, Pol1) %>% 
  pivot_longer(cols = c("Pol2", "Pol1"), names_to = "scenario", values_to = "C_t") %>% 
  ggplot(aes(x = time, y = C_t / 1e6, linetype = scenario)) + 
  geom_hline(yintercept = 0, color = "gray") +
  geom_line() +
  theme_bw() +
  labs(x = "Time",
       y = "Value relative to BAU") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = 0) +
  scale_linetype_manual(values = c("dotted", "longdash")) +
  theme(legend.position = "None")

dif <- rbind(bau, con1, con2) %>% 
  select(time, V_disc, scenario) %>% 
  spread(scenario, V_disc) %>% 
  mutate(Pol2 = Pol2 - BAU,
         Pol1 = Pol1 - BAU) %>% 
  select(time, Pol2, Pol1) %>% 
  pivot_longer(cols = c("Pol2", "Pol1"), names_to = "scenario", values_to = "V_t") %>% 
  group_by(scenario) %>% 
  summarize(NPV = sum(V_t, na.rm = T)) %>% 
  ggplot(aes(x = scenario, y = NPV / 1e6, linetype = scenario)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_col(color = "black", size = 1, fill = "transparent") +
  labs(x = "Policy",
       y = "NPV of policy") +
  scale_linetype_manual(values = c("dotted", "longdash")) +
  scale_y_continuous(breaks = 0) +
  theme_bw() +
  theme(legend.position = "None")

p <- cowplot::plot_grid(n_plot, c_plot, v_disc_t, dift, ncol = 2, labels = "AUTO")

pp <- cowplot::plot_grid(p, dif, ncol = 2, rel_widths = c(3, 1), labels = c("", "E"))


ggsave(plot = pp,
       filename = here("results", "img", "panel_figure.tiff"),
       width = 7,
       height = 4.5)


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



