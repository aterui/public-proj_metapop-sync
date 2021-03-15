
# setup -------------------------------------------------------------------

  pacman::p_load("tidyverse")
  d0 <- read_csv(here::here("data_raw/data_hkd_prtwsd.csv"))
  

# species name ------------------------------------------------------------
  
  d0 <- d0 %>% 
    mutate(StCode = paste0(river, site)) %>%
    filter(species != "Gasterosteidae") %>% 
    mutate(LatinName = str_replace(species, "Cottus amblystom opsis", "Cottus amblystomopsis")) %>%
    mutate(LatinName = str_replace(LatinName, "Pungitius sinensis", "Pungitius spp.")) %>%
    mutate(LatinName = str_replace(LatinName, "Pungitius pungitius", "Pungitius spp.")) %>%
    mutate(LatinName = str_replace(LatinName, "Barbatula toni", "Noemacheilus barbatulus")) %>%
    mutate(LatinName = str_replace(LatinName, "Lethenteron reissneri", "Lethenteron sp.")) %>%
    mutate(LatinName = str_replace(LatinName, "Rhinogobius sp. CO", "Rhinogobius mizunoi")) %>%
    mutate(LatinName = str_remove_all(LatinName, "\\.")) %>% 
    mutate(LatinName = str_replace_all(LatinName, "\\s", "_"))
  
  ## genus with unidentified species
  ui_genus <- d0 %>% 
    filter(str_detect(LatinName, pattern = "spp")) %>% 
    distinct(LatinName) %>% 
    pull() %>% 
    str_remove(pattern = "_spp")
  
  d0 <- d0 %>% 
    mutate(LatinName = case_when(genus %in% ui_genus ~ paste0(genus, "_spp"),
                                 TRUE ~ as.character(LatinName)))
  

# summarize ---------------------------------------------------------------

  d1 <- d0 %>% 
    group_by(year, river, site, LatinName) %>% 
    summarize(sample1 = sum(sample1, na.rm = TRUE),
              sample2 = sum(sample2, na.rm = TRUE),
              area = unique(area)) %>% 
    mutate(sample1 = ifelse(is.na(area), NA, sample1),
           sample2 = ifelse(is.na(area), NA, sample2))
    
  write_csv(d1, file = here::here("data_fmt/data_hkd_prtwsd_fmt.csv"))