######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

# Von-bertalanfy weight-at-age #################################################
vbl <- function(a, m_inf, a0, k){
  m <- m_inf * (1 - exp(-k * (a - a0)))
  return(m)
}



# Population model #############################################################
leslie <- function(a, mature_age, m, s_juvs, s_adul, K, N, nsteps, d_type = NULL, touch_at_a = NULL){
  # browser()
  # Build components of M from point estimates
  m_vec <- numeric(length = a)
  m_vec[mature_age:a] <- m
  
  s_vec <- numeric(length = a)
  s_vec[1:(mature_age-1)] <- s_juvs
  s_vec[mature_age:a] <- s_adul
  f_vec <- m_vec * s_vec
  
  
  
    # Build M
  M <- matrix(numeric(a^2), nrow = a)
  M[1,] <- f_vec
  
  for(i in 1:(a - 1)){
    M[i + 1, i] <- s_vec[i]
  }
  
  # Define other things
  I <- diag(a)
  
  # browser()
  
  results <- data.frame(time = 0, age = 1:a, N = N, D = 0, t_equil = F)
  i_equil <- n_steps
  j <- 1
  
  N[touch_at_a] <- N[touch_at_a] - 1
  for(i in 1:nsteps){
    
    # Were we asked to touch whales, and when?
    # if((!is.null(touch_at_a)) & (i == (i_equil + 2))){
      # print(paste0("touching age ", touch_at_a, "at ", i_equil))
      # N[touch_at_a] <- N[touch_at_a] - 1
    # }
    # browser()
    
    if(is.null(d_type)){
      D <- 1
    } else if(d_type == "N"){
      N_tot <- sum(N)
      D <- (K - N_tot) / K
    } else if (d_type == "Ni"){
      # D <- diag(a)
      D <- (K - N) / K
    } else if (d_type == "M"){
      masses <- vbl(a = 1:a, m_inf = m_inf, a0 = a0, k = k)
      M_tot <- sum(N * masses)
      D <- (K - M_tot) / K
    } else if(d_type == "Mi") {
      masses <- vbl(a = 1:a, m_inf = m_inf, a0 = a0, k = k)
      M_i <- N * masses
      D <- (K - M_i) / K
    }
    
    dead <- (rep(1, a) - s_vec) * N
    N_new <- N + (D * M %*% N)
    
    equil <- sum(N) == sum(N_new)
    if(equil & j == 1){
      i_equil <- i
      j <- j + 1
      }
    
    N <- N_new
    
    res <- data.frame(time = i-1, age = 1:a, N = N, D = dead, t_equil = equil)
    results <- rbind(results, res)
  }
  
  return(results)
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

