
##moderna
## Data from
## N Engl J Med 2021; 384:403-416
# DOI: 10.1056/NEJMoa2035389https://www.nejm.org/doi/full/10.1056/nejmoa2035389

library(tidyverse)


set.seed(1) #for reproducibility

n_simulations <- 1000


# Minimum and maximum per-group efficacy that we would publish.

eff.min <- 0.8642
eff.max <- 0.9559

treatment_allocation = 0.5


moderna_groups <- import_patient_groups(here('data/moderna.tsv'))
moderna_groups

## data from the paper

infection_rates <- overall_infection_rates(moderna_groups)

infection_rate

moderna_sim <- simTrials(moderna_groups,n=n_simulations,treatment_prob=treatment_allocation)

trials_to_win <- 1/(sum(summariseTrials(moderna_sim,eff.min,eff.max)$win)/n_simulations)
trials_to_win

pdf(here('results/moderna.pdf'))
plot_results(moderna_sim,eff.min,eff.max)
dev.off()

