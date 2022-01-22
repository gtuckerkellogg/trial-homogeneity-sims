##AZ
## N Engl J Med 2021; 385:2348-2360
## https://www.nejm.org/doi/full/10.1056/NEJMoa2105290
## DOI: 10.1056/NEJMoa2105290


library(tidyverse)
library(here)
source(here('R/trial_sim_funcs.R'))

set.seed(1) #for reproducibility

n_simulations <- 1000


# Minimum and maximum per-group efficacy that we would publish.

eff.min <- 0.7213

eff.max <- 0.8260

treatment_allocation = 0.66


## data from the paper

az_groups <- import_patient_groups(here('data/az.tsv'))
az_groups

az_sim <- simTrials(az_groups,n=n_simulations,treatment_prob = treatment_allocation)

table(summariseTrials(az_sim,eff.min,eff.max)$win)

trials_to_win <- 1/(sum(summariseTrials(az_sim,eff.min,eff.max)$win)/n_simulations)
trials_to_win

pdf(here('results/AZ.pdf'))
plot_results(az_sim,eff.min,eff.max)
dev.off()

