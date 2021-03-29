
# setup -------------------------------------------------------------------

  rm(list = ls())
  pacman::p_load(tidyverse, foreach, patchwork)
  source("analysis_jackknife.R")
  source("figure_set_theme.R")
  

# merge_data --------------------------------------------------------------
  
  files <- list("data_fmt/data_env_fmt.csv", "data_fmt/data_ocean_fmt.csv")
  data_list <- lapply(files, read_csv)
  
  dat <- dd_contr %>% 
    left_join(data_list[[1]], by = "river") %>% 
    left_join(data_list[[2]], by = "river") %>% 
    mutate(logit_forest = log(frac_forest / (1 - frac_forest)))
  

