library(tidyverse)
library(here)
source(here('R/trial_sim_funcs.R'))


set.seed(1)


#Sputnik-V trial simulations

n_simulations <- 1000
    
eff.min <- 0.9003

eff.max <- 0.9271

treatment_allocation = 0.75


sputnik_groups <- import_patient_groups(here('data/sputnik.tsv'))
sputnik_groups

infection_rate <- overall_infection_rates(sputnik_groups)
infection_rate

#sputnik_sim <- parallel_simTrials(sputnik_groups,n=n_simulations,treatment_prob=treatment_allocation)
sputnik_sim <- simTrials(sputnik_groups,n=n_simulations,treatment_prob=treatment_allocation)

trials_to_win <- 1/(sum(summariseTrials(sputnik_sim,eff.min,eff.max)$win)/n_simulations)
trials_to_win

pdf(here('results/Sputnik.pdf'))
plot_results(sputnik_sim,eff.min,eff.max)
dev.off()

