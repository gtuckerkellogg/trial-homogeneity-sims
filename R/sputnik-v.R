library(tidyverse)
library(here)
source(here('R/trial_sim_funcs.R'))

set.seed(1)

#Sputnik-V trial simulations

n_simulations <- 10000

eff.min <- 0.899 

eff.max <- 0.931

sputnik_groups <- import_patient_groups(here('data/sputnik.tsv'))

equal_efficacy <- simTrials(sputnik_groups,n=n_simulations)

unequal_efficacy <- simTrials(sputnik_groups,n=n_simulations,as_equal = FALSE)

table(summariseTrials(equal_efficacy,eff.min,eff.max)$win)
table(summariseTrials(unequal_efficacy,eff.min,eff.max)$win)


plot_results(unequal_efficacy,eff.min,eff.max)
