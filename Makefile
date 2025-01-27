all: main_figures tables sup_figures README.md
main_figures: results/img/value_by_density.pdf results/img/value_by_mortality.pdf results/img/value_by_N0.pdf results/img/value_by_species.pdf results/img/body_vs_dynamics.pdf results/img/iwc_data.pdf
sup_figures: results/img/unstable_age_distributions.pdf results/img/rep_value.pdf results/img/stable_age_distributions.pdf results/img/whale_bau_timeline.pdf results/img/C_t_bau.pdf
tables: results/tab/species_params.tex results/tab/global_params.tex results/tab/scc.tex
dag: makefile-dag.png

# draw makefile dag
makefile-dag.png: Makefile
				make -Bnd | make2graph -b | dot -Tpng -Gdpi=300 -o makefile-dag.png

README.md: scripts/make_README.r makefile-dag.png
				cd $(<D); Rscript $(<F)

# TABLES 
# Species parameter tables
results/tab/species_params.tex: scripts/content/parameter_tables.R data/raw/pershing_parameters.csv
				cd $(<D); Rscript $(<F)

results/tab/global_params.tex: scripts/content/parameter_tables.R data/raw/global_parameters.csv
				cd $(<D); Rscript $(<F)

results/tab/scc.tex: scripts/content/parameter_tables.R data/raw/tsd_2021_annual_unrounded.csv
				cd $(<D); Rscript $(<F)
				
# FIGURES ######################################################################
# Plot of value by age
results/img/value_by_density.pdf: scripts/content/plot_value_by_density.R data/output/value_by_density.rds
				cd $(<D); Rscript $(<F)

# Plot of value by mortality type
results/img/value_by_mortality.pdf: scripts/content/plot_mortality_sources.R data/output/value_by_mortality_source.rds
				cd $(<D); Rscript $(<F)

# Plot carbon difference in time
results/img/c_dif_time.pdf: scripts/content/plot_mortality_sources.R data/output/value_by_mortality_source.rds
				cd $(<D); Rscript $(<F)

# Plot of value by starting population size
results/img/value_by_N0.pdf: scripts/content/plot_value_by_N0.R data/output/value_by_N0.rds
				cd $(<D); Rscript $(<F)

# Plot of randomly removing a whale
results/img/value_by_species.pdf: scripts/content/plot_removed_at_random.R data/output/removed_at_random.rds
				cd $(<D); Rscript $(<F)

# Plor of body vs dynamics
results/img/body_vs_dynamics.pdf: scripts/content/plot_body_vs_dynamics.R data/output/value_by_mortality_source.rds
				cd $(<D); Rscript $(<F)
				
# Plot of IWC data
results/img/iwc_data.pdf: scripts/content/plot_iwc_data.R data/raw/iwc_total_catches_timeseries.csv
				cd $(<D); Rscript $(<F)

# SUPPLEMENTARY FIGURES ########################################################
results/img/stable_age_distributions.pdf: scripts/content/plot_stable_age_distributions.R data/processed/primers.rds
				cd $(<D); Rscript $(<F)
				
results/img/rep_value.pdf: scripts/content/plot_reproductive_value.R data/processed/primers.rds
				cd $(<D); Rscript $(<F)
				
results/img/unstable_age_distributions.pdf: scripts/content/plot_unstable_age_distributions.R data/output/runs_to_1000.rds
				cd $(<D); Rscript $(<F)

results/img/whale_bau_timeline.pdf: scripts/content/plot_whale_bau_timeline.r data/output/value_by_density.rds
				cd $(<D); Rscript $(<F)

results/img/C_t_bau.pdf: scripts/content/plot_C_t_bau.R data/output/value_by_mortality_source.rds
				cd $(<D); Rscript $(<F)

# EXPERIMENTAL DATA SETS #######################################################
# Data of mortality by age under different density-dependences
data/output/value_by_density.rds: scripts/experiments/value_by_density.R data/processed/primers.rds
				cd $(<D); Rscript $(<F)

# Data of mortality sources
data/output/value_by_mortality_source.rds: scripts/experiments/mortality_sources.R data/processed/primers.rds
				cd $(<D); Rscript $(<F)

# Data on carrying capcity experiments
data/output/value_by_N0.rds: scripts/experiments/initial_pop_size.R data/processed/primers.rds
				cd $(<D); Rscript $(<F)
				
# Data on species-value experiments
data/output/removed_at_random.rds: scripts/experiments/removed_at_random.R data/processed/primers.rds
				cd $(<D); Rscript $(<F)

# PRIMER #######################################################################
data/processed/primers.rds: scripts/primers.R data/raw/pershing_parameters.csv data/processed/scc.rds scripts/_functions.R
				cd $(<D); Rscript $(<F)

# Randomly killing whales
data/output/runs_to_1000.rds: scripts/run_1000.R data/raw/pershing_parameters.csv data/processed/scc.rds scripts/_functions.R
				cd $(<D); Rscript $(<F)

# SCC
data/processed/scc.rds: scripts/scc_t.R data/raw/tsd_2021_annual_unrounded.csv
				cd $(<D); Rscript $(<F)