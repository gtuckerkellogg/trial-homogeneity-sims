##PFIZER
## data from
## N Engl J Med 2020; 383:2603-2615
## DOI: 10.1056/NEJMoa2034577

library(tidyverse)
library(here)
source(here('R/trial_sim_funcs.R'))


set.seed(1) #for reproducibility

n_simulations <- 1000


# Minimum and maximum per-group efficacy that we would publish.

eff.min <- 0.9284 

eff.max <- 1

treatment_allocation = 0.5

pfizer_groups <- import_patient_groups(here('data/pfizer.tsv'))
pfizer_groups

## data from the paper

infection_rates <- overall_infection_rates(pfizer_groups)
infection_rates


pfizer_sim <- simTrials(pfizer_groups,n=n_simulations,treatment_prob=treatment_allocation)

trials_to_win <- 1/(sum(summariseTrials(pfizer_sim,eff.min,eff.max)$win)/n_simulations)
trials_to_win

pdf(here('results/pfizer.pdf'))
plot_results(pfizer_sim,eff.min,eff.max)
dev.off()

