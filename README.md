# Carbon whales 


## Dependency graph 

![](makefile-dag.png) 

## Repository structure 

```
-- carbon_whales.Rproj
-- data
   |__output
      |__removed_at_random.rds
      |__runs_to_1000.rds
      |__value_by_density.rds
      |__value_by_mortality_source.rds
      |__value_by_N0.rds
   |__processed
      |__primers.rds
   |__raw
      |__global_parameters.csv
      |__pershing_parameters.csv
-- docs
-- Makefile
-- makefile-dag.png
-- README.md
-- renv
-- renv.lock
   |__activate.R
   |__library
      |__R-3.6
   |__settings.dcf
-- results
   |__img
      |__c_dif_time.pdf
      |__panel_figure.tiff
      |__stable_age_distributions.pdf
      |__unstable_age_distributions.pdf
      |__value_by_density.pdf
      |__value_by_mortality.pdf
      |__value_by_N0.pdf
      |__value_by_species.pdf
      |__whale_bau_timeline.pdf
   |__tab
      |__global_params.tex
      |__species_params.tex
-- scripts
   |___functions.R
   |__blue_whales.R
   |__content
      |__parameter_tables.R
      |__plot_mortality_sources.R
      |__plot_removed_at_random.R
      |__plot_stable_age_distributions.R
      |__plot_unstable_age_distributions.R
      |__plot_value_by_density.R
      |__plot_value_by_N0.R
      |__plot_whale_bau_timeline.R
   |__experiments
      |__initial_pop_size.R
      |__mortality_sources.R
      |__removed_at_random.R
      |__value_by_density.R
   |__leslie.R
   |__make_README.R
   |__primers.R
   |__renato.R
   |__run_1000.R
```

---------
