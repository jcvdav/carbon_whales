######################################################
#title#
######################################################
# 
# Purpose
# https://www.whitehouse.gov/omb/information-regulatory-affairs/regulatory-matters/#scghgs
#
######################################################


library(here)
library(janitor)
library(tidyverse)

source(here("scripts", "_functions.R"))

scc <- read_csv(here("data", "raw", "tsd_2021_annual_unrounded.csv")) %>% 
  clean_names() %>% 
  mutate(time = year - 2022) %>% 
  select(year, time, scc_t = x2_5_percent_co2)

# mod <- lm(scc_t ~ time, data = scc)

# scc_2070 <- tibble(time = 0:nsteps) %>% 
  # mutate(scc_pred_t = predict(mod, newdata = .))

saveRDS(object = scc,
        file = here("data", "processed", "scc_pred.rds"))
