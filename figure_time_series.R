
# setup -------------------------------------------------------------------

  rm(list = ls())
  pacman::p_load(tidyverse, runjags, foreach)
  d0 <- read_csv(here::here("data_est/data_ssm_est.csv")) %>% 
    rename(low = "2.5%",
           median = "50%",
           high = "97.5%")


# plot --------------------------------------------------------------------

  g <- d0 %>% 
    filter(param_id == "log_d_mean") %>% 
    ggplot() +
    geom_line(aes(x = year_id, y = median + log(100), color = species)) +
    facet_wrap(facets = ~ river) +
    ylab("Fish density (ind./100 sq-m)") +
    xlab("Year")
  
  print(g)
