# FIGURES
results/img/value_by_age.pdf: scripts/content/plot_value_by_age.R data/output/value_by_age.rds
				Rscript scripts/content/plot_value_by_age.R

# EXPERIMENTAL DATA SETS
data/output/value_by_age.rds: scripts/experiments/value_by_age.R data/processed/primer.rds
				Rscript scripts/experiments/value_by_age.R

# PRIMER
data/processed/primer.rds: scripts/primers.R
				Rscript scripts/primers.R
				
				
## NEXT STEPS
# - I have re-rcreated the value-at-age figure for different density-dependences
# - I have everyting I need to get hwo that changes dependong on fate of mortality (shipping vs harvesting)
# - I also have the code to calculate the value of a whale at different population sizes