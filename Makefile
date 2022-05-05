all: figures tables
figures: results/img/value_by_density.pdf results/img/value_by_mortality.pdf results/img/value_by_N0.pdf
tables: results/tab/species_params.tex results/tab/global_params.tex
dag: makefile-dag.png

# draw makefile dag
makefile-dag.png: Makefile
	make -Bnd | make2graph | dot -Tpng -Gdpi=300 -o makefile-dag.png

# TABLES 
# Species parameter tables
results/tab/species_params.tex: scripts/content/parameter_tables.R data/raw/pershing_parameters.csv
				cd $(<D); Rscript $(<F)

results/tab/global_params.tex: scripts/content/parameter_tables.R data/raw/global_parameters.csv
				cd $(<D); Rscript $(<F)
				
# FIGURES ######################################################################
# Plot of value by age
results/img/value_by_density.pdf: scripts/content/plot_value_by_density.R data/output/value_by_density.rds
				cd $(<D); Rscript $(<F)

# Plot of value by mortality type
results/img/value_by_mortality.pdf: scripts/content/plot_mortality_sources.R data/output/value_by_mortality_source.rds
				cd $(<D); Rscript $(<F)

# Plot of value by starting population size
results/img/value_by_N0.pdf: scripts/content/plot_value_by_N0.R data/output/value_by_N0.rds
				cd $(<D); Rscript $(<F)

# EXPERIMENTAL DATA SETS #######################################################
# Data of mortality by age under different density-dependences
data/output/value_by_density.rds: scripts/experiments/value_by_density.R data/processed/primer.rds
				cd $(<D); Rscript $(<F)

# Data of mortality sources
data/output/value_by_mortality_source.rds: scripts/experiments/mortality_sources.R data/processed/primer.rds
				cd $(<D); Rscript $(<F)

# Data on carrying capcity experiments
data/output/value_by_N0.rds: scripts/experiments/initial_pop_size.R data/processed/primer.rds
				cd $(<D); Rscript $(<F)

# PRIMER #######################################################################
data/processed/primer.rds: scripts/primers.R data/raw/pershing_parameters.csv
				cd $(<D); Rscript $(<F)

