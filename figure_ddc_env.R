#' ---
#' title: prelim analysis, diversity deficit
#' author: ""
#' output: html_document
#' ---
#' 
#' In the following figure, each data point represents a watershed (metapopulation).
#' First, I estimated diversity deficit for all the metapopulations (31 or 29, varies by species).
#' Then, I removed one watershed from the calculation of diversity deficit to see the contribution of the watershed to the overall diversity deficit.
#' The 'one-minus' diversity deficit was calculated from the overall diversity deficit (`dd_c` in the following figure).
#' 
#' - dd_c: diversity deficit difference between all and one-minus set of watersheds. Higher values indicate a particular watershed contributes to increasing diversity deficit\
#' - area: watershed area\
#' - mean_temp: mean air temperature\
#' - mean_ppt: mean cumulative precipitation\
#' - logit_forest: logit-transformed fraction of forest\
#' - chr_a: chrolophyll a concetration in the ocean surface, 30 km buffer\
#' - sst: sea surface water temperature, 30 km buffer\
#+ echo = FALSE, message = FALSE

# setup -------------------------------------------------------------------

  rm(list = ls())
  pacman::p_load(tidyverse, foreach, patchwork)
  source("data_merge.R")


# plot diversity deficit --------------------------------------------------

  theme_set(theme_bw())
  
  g1 <- dat %>% 
    ggplot() +
    geom_point(aes(y = dd_c, x = log(area), color = species)) + 
    geom_smooth(aes(y = dd_c, x = log(area), color = species),
                method = "lm", size = 0.5, se = FALSE) +
    theme(legend.position = "none")
  
  g2 <- dat %>% 
    ggplot() +
    geom_point(aes(y = dd_c, x = mean_temp, color = species)) + 
    geom_smooth(aes(y = dd_c, x = mean_temp, color = species),
                method = "lm", size = 0.5, se = FALSE) +
    theme(legend.position = "none")
  
  g3 <- dat %>% 
    ggplot() +
    geom_point(aes(y = dd_c, x = mean_ppt, color = species)) + 
    geom_smooth(aes(y = dd_c, x = mean_ppt, color = species),
                method = "lm", size = 0.5, se = FALSE) +
    theme(legend.position = "none")
  
  g4 <- dat %>% 
    ggplot() +
    geom_point(aes(y = dd_c, x = logit_forest, color = species)) + 
    geom_smooth(aes(y = dd_c, x = logit_forest, color = species),
                method = "lm", size = 0.5, se = FALSE) +
    theme(legend.position = "none")
  
  g5 <- dat %>% 
    ggplot() +
    geom_point(aes(y = dd_c, x = chr_a, color = species)) + 
    geom_smooth(aes(y = dd_c, x = chr_a, color = species),
                method = "lm", size = 0.5, se = FALSE) +
    theme(legend.position = "none")
  
  g6 <- dat %>% 
    ggplot() +
    geom_point(aes(y = dd_c, x = sst, color = species)) +
    geom_smooth(aes(y = dd_c, x = sst, color = species),
                method = "lm", size = 0.5, se = FALSE)
  
  (g1 | g2) / (g3 | g4) / (g5 | g6)

#' Time series data used for the calculation of diversity deficit.
#+ echo = FALSE, message = FALSE, fig.height = 8, fig.width = 8
  
# plot time series --------------------------------------------------------

  source("figure_time_series.R")
  