
# setup -------------------------------------------------------------------
  
  rm(list = ls())
  pacman::p_load(tidyverse, foreach)
  d0 <- read_csv(here::here("data_est/data_ssm_est.csv")) %>% 
    rename(low = "2.5%",
           median = "50%",
           high = "97.5%")
  
  unique_species <- unique(d0$species)

    
# define function ---------------------------------------------------------
  
  ## Yamane's diversity deficit metric
  fun_dd <- function(x) {
    
    if(any(x < 0)) {
      m_x <- x + abs(min(x))
    } else {
      m_x <- x
    }
    
    sum_mu <- sum(colMeans(m_x))
    m_cov <- var(m_x)
    cv_current <- sum(m_cov) / sum_mu
    cv_null <- sum(diag(m_cov)) / sum_mu
    
    return(cv_current - cv_null)
  }
  

# jackknife dd ------------------------------------------------------------

  dd_contr <- foreach(i = 1:2, .combine = bind_rows) %do% {
    m_density <- d0 %>% 
      filter(species == unique_species[i],
             param_id == "log_d_mean") %>% 
      pivot_wider(id_cols = year_id,
                  names_from = river,
                  values_from = median) %>% 
      select(-year_id)
    
    dd_obs <- fun_dd(m_density)
    dd_jackknife <- sapply(1:ncol(m_density),
                           FUN = function(i) fun_dd(m_density[, -i]))
    
    tibble(river = colnames(m_density),
           dd_jackknife = dd_jackknife,
           dd_obs = dd_obs,
           dd_c = dd_obs - dd_jackknife,
           species = unique_species[i])
  }

  
  