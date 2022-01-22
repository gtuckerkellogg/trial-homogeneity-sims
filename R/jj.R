## J&J Janssen trial simulation
## data from N Engl J Med 2021; 384:2187-2201
## DOI: 10.1056/NEJMoa2101544

library(tidyverse)
library(here)
source(here('R/trial_sim_funcs.R'))


set.seed(1) #for reproducibility

n_simulations <- 1000


# Minimum and maximum per-group efficacy that we would publish.

eff.min <- 0.6392

eff.max <- 0.7632

treatment_allocation = 0.5

janssen_groups <- import_patient_groups(here('data/janssen.tsv'))
janssen_groups

## data from the paper

infection_rates <- overall_infection_rates(janssen_groups)

janssen_sim <- simTrials(janssen_groups,n=n_simulations,treatment_prob=treatment_allocation)

trials_to_win <- 1/(sum(summariseTrials(janssen_sim,eff.min,eff.max)$win)/n_simulations)
trials_to_win

pdf(here('results/janssen.pdf'))
plot_results(janssen_sim,eff.min,eff.max)
dev.off()
