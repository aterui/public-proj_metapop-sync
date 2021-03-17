
# setup -------------------------------------------------------------------
  
  rm(list = ls())
  pacman::p_load(tidyverse, foreach)
  d0 <- read_csv(here::here("data_fmt/data_hkd_prtwsd_fmt.csv"))

  
# select by occurrence frequency ------------------------------------------

  df_freq <- d0 %>% 
    group_by(year, river, LatinName) %>% 
    summarize(summed_n = sum(sample1 + sample2, na.rm = T)) %>% 
    filter(summed_n > 0) %>% 
    group_by(river, LatinName) %>% 
    summarize(freq = n())
  
  species <- df_freq %>% 
    filter(freq > 2) %>% 
    ungroup() %>% 
    distinct(LatinName) %>% 
    pull()
  
  dat_list <- foreach(i = seq_len(length(species))) %do% {
    river_f3 <- df_freq %>% 
      filter(freq > 2 & LatinName == species[i]) %>% 
      pull(river)
    
    dat <- d0 %>% 
      filter(river %in% river_f3 & LatinName == species[i]) %>% 
      mutate(n_river = length(river_f3))
    
    return(dat)
  }
    