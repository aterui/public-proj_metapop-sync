
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, runjags)
  source(here::here("data_fmt_watershed.R"))
  
  n_ad <- 100
  n_iter <- 1E+4
  n_thin <- max(3, ceiling(n_iter/500))
  n_burn <- ceiling(max(10, n_iter/2))
  n_sample <- ceiling(n_iter/n_thin)


# prepare data ------------------------------------------------------------

  dat <- dat_list[[2]]
  
  dat <- dat %>% 
    mutate(river_id = as.numeric(as.factor(river)))
  
  # data
  ## data
  Y <- dat$sample1 + dat$sample2
  log_Area <- log(dat$area)
  
  ## year, river, site indicators
  Year <- dat$year + 1 - min(dat$year, na.rm = T)
  River <- dat$river_id
  Site <- dat$site
  
  ## n iterators
  N_sample <- nrow(dat)
  N_year <- n_distinct(dat$year)
  N_river <- n_distinct(dat$river)
  N_site <- dat %>% 
    group_by(river_id) %>% 
    arrange(river_id) %>% 
    summarize(n_site = n_distinct(site)) %>% 
    pull(n_site)
  

# jags setup --------------------------------------------------------------

  inits <- replicate(3,
                     list(
                       mean_log_r = rep(0, N_river),
                       tau_eps_site = 1,
                       .RNG.name = "base::Mersenne-Twister",
                       .RNG.seed = NA
                       ),
                     simplify = FALSE)
  
  for (i in 1:3) inits[[i]]$.RNG.seed <- i + 100
  
  d_jags <- list(Y = Y,
                 log_Area = log_Area,
                 Year = Year,
                 River = River,
                 Site = Site,
                 N_sample = N_sample,
                 N_year = N_year,
                 N_river = N_river,
                 N_site = N_site,
                 w = diag(N_river))
  
  para <- c("mu_log_area",
            "sigma_log_area",
            "mean_log_r",
            "m_sigma_r",
            "sigma_eps_site")
  
  m <- read.jagsfile("bayes/model_ssm.R")

  
# run jags ----------------------------------------------------------------
  
  post <- run.jags(m$model,
                   monitor = para,
                   data = d_jags,
                   n.chains = 3,
                   inits = inits,
                   method = "parallel",
                   burnin = n_burn,
                   sample = n_sample,
                   adapt = n_ad,
                   thin = n_thin,
                   n.sims = 3,
                   module = "glm")
  
  MCMCvis::MCMCsummary(post$mcmc)
  