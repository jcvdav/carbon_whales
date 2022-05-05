######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

distribute_N <- function(max_age, N_tot){
  rep((N_tot / max_age), max_age)
}

distribute_N0 <- function(max_age, N_tot){
  N <- numeric(max_age)
  N[1] <- N_tot
  # rep((N_tot / max_age), max_age)
  return(N)
}

get_N_stable <- function(N_tot, data){
  KNi <- data %>% 
    filter(time == max(time)) %>%
    pull(N)
  
  N_tot * (KNi / sum(KNi))
}

get_N_equil <- function(N_tot, data){
  KNi <- data %>% 
    filter(time == max(time)) %>%
    pull(N)
  
  KNi
}

get_Ki <- function(data, K){
  N_stable <- data %>% 
    filter(time == max(time)) %>% 
    pull(N)
  K * (N_stable / sum(N_stable))
}

make_wd <- function(max_age, touch_at_a, killed = 1){
  # browser()
  v <- numeric(max_age)
  v[touch_at_a] <- killed
  
  return(v)
}

# Von-bertalanfy weight-at-age #################################################
vbl <- function(a, m_inf, a0, k){
  # Calculate mass
  m <- m_inf * (1 - exp(-k * (a + a0)))
  
  # Return mass
  return(m)
}


make_matrix <- function(max_age, mature_age, m, s_juvs, s_adul) {
  # Build components of M from point estimates
  # Fecundity
  m_vec <- numeric(length = max_age)
  m_vec[mature_age:max_age] <- m
  
  # Survival
  s_vec <- numeric(length = max_age)
  s_vec[1:(mature_age-1)] <- s_juvs
  s_vec[mature_age:(max_age-1)] <- s_adul
  
  # Recruitment
  f_vec <- c(m_vec[2:max_age], 0) * s_vec
  # f_vec <- m_vec * s_vec
  
  # Initialize matrix
  M <- matrix(numeric(max_age^2), nrow = max_age)
  
  # Add the recruitment vector
  M[1,] <- f_vec
  
  # Add the survival vector to the lower diagonal
  for(i in 1:(max_age - 1)){
    M[i + 1, i] <- s_vec[i]
  }
  
  # Return matrix
  return(M)
}


# Population model #############################################################
leslie <- function(max_age, mature_age, m, s_juvs, s_adul, K, N, nsteps, d_type = NULL, touch_at_a = NULL, m_inf, a0, k, just_first = T, H = 0, S = 0){

  # Build M
  M <- make_matrix(max_age = max_age,
                   mature_age = mature_age,
                   m = m,
                   s_juvs = s_juvs,
                   s_adul = s_adul)
  
  s_vec <- c(M[lower.tri(M) & M != 0], 0)
  
  # Define other things
  I <- diag(max_age)
  # I <- 0
  
  mass_at_age <- tibble(age = 1:max_age) %>%
    mutate(mass = vbl(
      a = age,
      m_inf = m_inf,
      a0 = a0,
      k = k
    ))
  
  results <- data.frame(time = 0, age = 1:max_age, N = N, D = 0, H = H)
  
  # browser()

  for(i in 1:nsteps){
    
    # Check density-dependence
    if(is.null(d_type)){
      D <- 1
    } else if(d_type == "KN"){
      N_tot <- sum(N)
      D <- (K - N_tot) / K
    } else if (d_type == "KNi"){
      # D <- diag(max_age)
      # c_par <- get_c(N_stable = N, K = KN)
      D <- pmax((K - N), 0) / K
      D[is.nan(D)] <- 0
    } else if (d_type == "KM"){
      M_tot <- sum(N * mass_at_age$mass)
      D <- (K - M_tot) / K
    } else if(d_type == "KMi") {
      M_i <- N * mass_at_age$mass
      D <- (K - M_i) / K
    }
    

    # browser()
    E <- N - H - S                                                  # Escapement
    dead <- ((rep(1, max_age) - s_vec) * E) + S                     # Deaths
    N <- E + (D * ((M - I) %*% E))                                  # Reproduction
    
    if(just_first){
      H <- 0
      S <- 0
    }
    
    res <- data.frame(time = i, age = 1:max_age, N = N, D = dead, H = H)
    results <- rbind(results, res)
  }
  
  
  c_b <- (0.4) * ((0.2 * 0.54) + (0.2 * 0.77))
  
  #https://github.com/mssavoca/prey_consumption_paper/blob/5f2f2af7af25499e31536f8653ae55116e337ef3/Savoca%20et%20al._Prey%20consumption%20paper%20analysis.R#L1924
  c_p <- (0.175 *          # whales eat 5-30% of their body weight daily
            0.25*          # 25 percent of it is dry mass, 75 is just water
            90 *           # They feed 90 - 120 days per year
            0.000146 *     # There are 0.000146 Kg of Fe per Kg of whale poop (Mean = 0.000146, SD = 0.000135))
            0.8 *          #80% of iron consumed is excreted
            0.25 ) *       # 50 % stays within ithe photic zone. and 50% of that is incorporated
    (0.018 *               # There are 0.018 mol Fe per f of FE,
       1e6) *              # convret to micromols
    ((1 / 3) *             # There are 3 micro-mols of FE per 1 mol of C
       12.01) *              # There are 12.01 g of C in 1 mol of C
    1e-6
  
  # browser()
  results <- results %>% 
    left_join(mass_at_age, by = "age") %>% 
    mutate(biomass = N * mass,
           C_b = biomass * c_b,               # Carbon stored in whale bodies
           C_p = biomass * c_p,               # How much carbon does each whale stimulate?
           C_s = D * mass * c_b * 0.5,   # Percent of C in body ultimately sequestered
           E = H * mass * c_b) %>%
    select(-mass)
  
  return(results)
}


leslie_wraper <- function(touch_at_a = NULL, d_type, max_age, mature_age, m, s_juvs, s_adul, K, N, nsteps, m_inf, a0, k, just_first = T, H = 0, S = 0){
  if(touch_at_a == 0){touch_at_a <- NULL}
  # browser()
  res <- leslie(max_age = max_age,
         mature_age = mature_age,
         m = m,
         s_juvs = s_juvs,
         s_adul = s_adul,
         K = K,
         N = N,
         nsteps = nsteps,
         d_type = d_type,
         touch_at_a = touch_at_a,
         m_inf = m_inf,
         a0 = a0,
         k = k,
         just_first = just_first,
         H = H,
         S = S) %>%
    group_by(time) %>%
    summarize(C_b = sum(C_b),
              C_p = sum(C_p),
              C_s = sum(C_s),
              N = sum(N),
              D = sum(D),
              E = sum(E)) %>%
    mutate_at(.vars = c("C_b", "C_p", "C_s", "N"), .funs = ~.x * 2) %>% 
    ungroup() %>%
    mutate(C_t = C_b + C_p + C_s - E,
           V = 36.6 * C_t,
           V_disc = V / (1 + 0.02) ^ time,
           scc = 36.6 / (1 + 0.02) ^ time)
  
  return(res)
}



# Time -series plot ############################################################
ts_plot <- function(data){
  data %>% 
    ggplot(aes(x = time, y = N)) + 
    stat_summary(geom = "line", fun = "sum") +
    scale_y_continuous(labels = "", breaks = NULL) +
    theme_bw() +
    labs(x = "Time (years)",
         y = "Total population size") +
    theme(legend.position = "None")
}


# mean_ci

mean_ci <- function(data){
  m <- mean(data)
  se <- sd(data) / sqrt(length(data))
  tibble(y = m,
         ymin = m -(1.96 * se),
         ymax = m +(1.96 * se))
}


# # pers <- function(max_age, mature_age, m, s_juvs, s_adul, nsteps, N) {
#   # Build components of M from point estimates
#   # Fecundity
#   m_vec <- numeric(length = 130)
#   m_vec[8:130] <- 1/8
#   
#   # Survival
#   s_vec <- numeric(length = 130)
#   s_vec[1:(8-1)] <- 0.843
#   s_vec[8:130] <- 0.975
#   
#   # Recruitment
#   f_vec <- c(m_vec[1:129], 0) * s_vec
#   
#   N <- params %>% 
#     filter(species == "Blue") %>% 
#     pull(N_stable)
#   N <- N[[1]]
#   
#   N_new <- numeric()
#   NN <- matrix(nrow = 130, ncol = nsteps )
#   
#   for(i in 1:nsteps){
#     D <- (K - sum(N)) / K
#     N_new[1] <- D * sum(f_vec * N)
#     N_new[2:length(f_vec)] <- N[1:129] * s_vec[1:129]
#     
#     N <- N_new
#     
#     NN[, i] <- N
#   }
#   
#   plot(colSums(NN))
# # }