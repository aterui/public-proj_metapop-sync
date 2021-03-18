model {

  # constant ----------------------------------------------------------------
  
  ninfo <- 1.0E-3
  u <- 10
  df <- 1
  s <- rep(1, N_river)
  
  # prior -------------------------------------------------------------------
  
  ## sampling area
  mu_log_area ~ dnorm(0, ninfo)
  tau_log_area ~ dscaled.gamma(u, df)
  sigma_log_area <- 1 / sqrt(tau_log_area)
  
  ## site-level random effect
  tau_eps_site ~ dscaled.gamma(u, df)
  sigma_eps_site <- 1 / sqrt(tau_eps_site)
  
  for (i in 1:N_river) {
    ## mean growth rate r
    mean_log_r[i] ~ dnorm(0, ninfo)

    ## initial density
    log_mean_d[1, i] ~ dnorm(0, ninfo)
  }
  
  ## variance-covariance in r
  m_tau_r[1:N_river, 1:N_river] ~ dwish(w[,], N_river + 1)
  m_sigma_r[1:N_river, 1:N_river] <- inverse(m_tau_r[,])
  
  
  # observation -------------------------------------------------------------
  
  for (n in 1:N_sample) {
    Y[n] ~ dpois(n_obs[Year[n], River[n], Site[n]])
    log(n_obs[Year[n], River[n], Site[n]]) <- log_d_obs[Year[n], River[n], Site[n]] +
                                              log_Area[n]
    
    log_Area[n] ~ dnorm(mu_log_area, tau_log_area)
  }

  
  # state -------------------------------------------------------------------
  
  ## metapopulation-level dynamics
  for (t in 1:(N_year - 1)) {
    for (i in 1:N_river) {
      log_mean_d[t + 1, i] <- log_r[t, i] + log_mean_d[t, i]
    } 
    log_r[t, 1:N_river] ~ dmnorm(mean_log_r[], m_tau_r[,])
  }
  
  
  ## site-level random effect
  for (i in 1:N_river) {
    for (j in 1:N_site[i]) {
      for (t in 1:N_year) {
          log_d_obs[t, i, j] <- log_mean_d[t, i] + eps_site[i, j]
      }
      eps_site[i, j] ~ dnorm(0, tau_eps_site)
    }
  }
    
}



