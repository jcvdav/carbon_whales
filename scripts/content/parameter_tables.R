######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(knitr)
library(kableExtra)
library(tidyverse)

source(here("scripts", "_functions.R"))

species_params <- read_csv(here("data", "raw", "pershing_parameters.csv")) %>% 
  filter(!species %in% c("blue", "sei", "bowhead", "bryde")) %>% 
  mutate_at(.vars = c("mature_age", "max_age"), round) %>% 
  mutate_at(.vars = c("KN", "KM", "N_tot", "M_tot"), ~round(.x / 1e3), 2) %>% 
  mutate(species = str_to_sentence(species),
         N = map2(max_age, N_tot, distribute_N),
         m = round(1 / calving_interval, 2),
         s_juvs = s_juvs + 0.1,
         s_adul = s_adul + 0.02,
         a0 = -1 * a0) %>% 
  select(species, KN, KM, N_tot, M_tot, mature_age, max_age, m, s_juvs, s_adul, m_inf, k, a0)


kable(species_params,
      format = "latex",
      booktabs = T,
      escape = F,
      col.names = c("Species",
                    "$K_N$",
                    "$K_M$",
                    "$N_0$",
                    "$M_0$",
                    "$\\alpha_m$",
                    "$\\alpha$",
                    "$\\mu$",
                    "$\\sigma_{juv}$",
                    "$\\sigma_{adt}$",
                    "$m_\\infty$",
                    "$k$",
                    "$a_0$"),
      format.args = list(big.mark = ","),
      label = "species_params",
      caption = "Demographic and mass-at-age parameters for five baleen whale species. $K_N$ and $K_M$ represent the pre-whaling abundance (in thousands) and biomass (in thousand tonnes) estimates used as carrying capacity. $N_0$ and $M_0$ are the present day (2011) estimates of abundance and biomass. $\\alpha_m$ is the age at maturity, $\\alpha$ is the maximum age attained, $\\mu$ is the fecundity, $\\sigma_{juv}$ and $\\sigma_{adt}$ are the juvenile and adult survival rates, and $m_\\infty$, $k$, and $a_0$ are the von Bertalanfy parameters for mass-at-age conversions. All parameters come from Pershing et al., 2010.") %>% 
  cat(file = here("results", "tab", "species_params.tex"))


global_params <- read_csv(here("data", "raw", "global_parameters.csv"))

kable(global_params,
      format = "latex",
      digits = 2,
      booktabs = T,
      escape = F,
      col.names = c("Parameter", "Symbol", "Value", "Source"),
      label = "global_params",
      caption = "Other model parameters.") %>% 
  cat(file = here("results", "tab", "global_params.tex"))


scc <- read_csv(here("data", "raw", "tsd_2021_annual_unrounded.csv")) %>% 
  janitor::clean_names() %>% 
  select(year, contains("percent_co"))
  
knitr::kable(scc,
      format = "latex",
      digits = 2,
      booktabs = T,
      label = "global_params",
      caption = "Annual Social Cost of Carbon, 2020-2050 (in 2020 dollars per metric ton of CO2) for three different discount rates. Model output by the Interagency Working Group reports values in five-year increments, they then use linear interpolation to fill-in missing year (Interagency Working Group, 2021),",
      col.names = c("Year", "5% Average", "3% Average", "2.5% Average")) %>% 
  cat(file = here("results", "tab", "scc.tex"))
