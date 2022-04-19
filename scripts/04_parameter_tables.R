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

species_params <- read_csv(here("data", "raw_data", "pershing_parameters.csv")) %>% 
  mutate_at(.vars = c("mature_age", "max_age"), round) %>% 
  mutate(m = round(1 / calving_interval, 2),
         a0 = -1 * a0) %>% 
  select(species, KN, KM, N_tot, M_tot, mature_age, max_age, m, s_juvs, s_adul, m_inf, k, a0)


kable(species_params,
      format = "latex",
      escape = F,
      col.names = c("Species",
                    "$K_N$",
                    "$K_M$",
                    "$N_0$",
                    "$M0$",
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
      caption = "Demographic and mass-at-age parameters for nie baleen whale species. $K_N$ and $K_M$ represent the pre-whaling abundance and biomass estimates used as carrying capacity. $N_0$ and $M_0$ are the present day (2011) estimates of abundance and biomass. $\\alpha_m$ is the age at maturity, $\\alpha$ is the maximum age attained, $\\mu$ is the fecundity, $\\sigma_{juv}$ and $\\sigma_{adt}$ are the juvenile and adult survival rates, and $\\m_inf$, $k$, and $a_0$ are the von Bertalanfy parameters for mass-at-age conversions. All parameters come from \\citet{pershing2010impact}.") %>% 
  cat(file = here("results", "tab", "species_params.tex"))


global_params <- read_csv(here("data", "raw_data", "global_parameters.csv"))

kable(global_params,
      format = "latex",
      escape = F,
      col.names = c("Parameter", "Symbol", "Value", "Source"),
      label = "global_params",
      caption = "Other model parameters.") %>% 
  cat(file = here("results", "tab", "global_params.tex"))
