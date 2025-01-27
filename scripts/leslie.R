######################################################
#title#
######################################################
# 
# Deer
#
######################################################

library(here)
library(cowplot)
library(tidyverse)


# BAU
a <- 7
f <- c(1.426, 1.290, 1.296, 1.120, 1.126, 1.554, 0)
s <- c(0.713, 0.645, 0.648, 0.560, 0.563, 0.777)


K <- 2220
N <- numeric(length = a)
N[1] <- 4
nsteps <- 20

# von Bertalanfy
m_inf <- 117
k <- 0.2
a0 <- 0


mass_at_age <- tibble(age = 1:a) %>% 
  mutate(mass = vbl(a = age, m_inf = m_inf, a0 = a0, k = k))

plot(mass_at_age)

bau <- leslie(a = a, f = f, s = s, K = K, N = N, nsteps = nsteps) %>% 
  left_join(mass_at_age, by = "age") %>% 
  group_by(time) %>% 
  summarize(M = sum(N * mass),
            N = sum(N)) %>% 
  ungroup() %>% 
  mutate(scenario = "BAU")



s2 <- 1.1 * s
f2 <- 1.1 * f

con <- leslie(a = a, f = f, s = s2, K = K, N = N, nsteps = nsteps) %>% 
  left_join(mass_at_age, by = "age") %>% 
  group_by(time) %>% 
  summarize(M = sum(N * mass),
            N = sum(N)) %>% 
  ungroup() %>% 
  mutate(scenario = "10% more\nsurvival")

con2 <- leslie(a = a, f = f2, s = s, K = K, N = N, nsteps = nsteps) %>% 
  left_join(mass_at_age, by = "age") %>% 
  group_by(time) %>% 
  summarize(M = sum(N * mass),
            N = sum(N)) %>% 
  ungroup() %>% 
  mutate(scenario = "10% more\nfecundity")


n_plot <- rbind(bau, con, con2) %>% 
  ggplot(aes(x = time, y = N, linetype = scenario)) + 
  geom_line() +
  scale_y_continuous(labels = "", breaks = NULL) +
  theme_bw() +
  labs(x = "Time (years)",
       y = "Total population size") +
  theme(legend.position = "None")

m_plot <- rbind(bau, con, con2) %>% 
  ggplot(aes(x = time, y = M, linetype = scenario)) + 
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = "", breaks = NULL) +
  labs(x = "Time (years)",
       y = "Total whale carbon")

leg <- cowplot::get_legend(m_plot)

m_plot <- m_plot +
  theme(legend.position = "None")

cols <- rbind(bau, con, con2) %>% 
  mutate(V = 35 * M,
         V_disc = V / (1 + 0.05) ^ time-1) %>% 
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

####################

N <- rep((0.25 * K / a), a) |> as.matrix()


# browser()
# Build M
M <- matrix(numeric(a^2), nrow = a)
M[1,] <- f

for(i in 1:(a - 1)){
  M[i + 1, i] <- s[i]
}


K <- 110
N <- numeric(7)
N[2] <- 4 
NN <- matrix(nrow = 7, ncol = 11)
NN[,1] <- N
I <- diag(1, 7)
for (t in 1:10){
  D <- (K - sum(N)) / K
  # N <- N + (D * (M - I) %*% N)
  N <- N + ((D *(M %*% N)) - (I %*% N))
  N2 <- D * (M %*% N)
  NN[,t+1] <- N
}
round(NN)

N <- N + ((D *(M %*% N)) - (I %*% N))

# a <- function() {
  c_par <- get_c(N_stable = NN[,11], K = K)
  N <- numeric(7)
  N[2] <- 4
  NN2 <- matrix(nrow = 7, ncol = 11)
  NN2[,1] <- N
  for (t in 1:10){
    # browser()
    D <- pmax(((c_par * K) - N), 0) / (c_par * K)
    D[is.nan(D)] <- 0
    # D <- diag(D, 7)
    N <- N + (D * (M - I) %*% N)
    NN2[,t+1] <- N
  }
# }

round(NN2)

