
##AZ
## This is Kyle Sheldrick's simulations, but I've refactored it so I understand it. -- GTK
## from https://doi.org/10.1016/S0140-6736(21)00234-8

#install.packages('epitools')
#install.packages('cowplot')

library(tidyverse)


set.seed(1) #for reproducibility

n_simulations <- 1000


# Minimum and maximum per-group efficacy that we would publish.

eff.min <- 0.7213

eff.max <- 0.8260


## data from the paper
patient_groups <- read_tsv('age-groups - copy - copy.tsv',show_col_types=FALSE) %>%
  mutate(n_patients=n_treatment+n_control)

control_infections = 130
treatment_infections = 73


infection_rate = c(control=control_infections/sum(patient_groups$n_control),
                   treatment = treatment_infections/sum(patient_groups$n_treatment))



simGroup <- function(n_patients,infection_rates=infection_rate,treatment_prob = 0.66) { 
  # simulate a group of n_patients with given infection rates, randomly assigned into treatment or control groups
  # according to the given probability
  # return efficacy if calculable, NA if not
  treatment_group <- factor(rbinom(n_patients, 1, treatment_prob)) %>%
    fct_recode("control"="0","treatment"="1")
  infection_prob <- infection_rates[treatment_group]
  infection <- rbinom(n_patients,1,infection_prob)
  cont <- table(infection,treatment_group)
  
  if (all(cont >0) && length(cont)>3){ #this checks there are no 0 cells (any 0 cell obviously means efficacy outside our range of 0%, 100% or incalculable)
    suppressWarnings(oops <- epitools::oddsratio(cont)) #calculates OR for this age group in this simulation run
    efficacy <- 1-oops$measure[2] #calculates VE as "1-OR" as defined in the paper
  } else { 
    efficacy <- 1
  }
  efficacy
}


simTrial <- function(patient_groups) { 
  # simulate a trial of patient groups passed in as a data frame
  mutate(rowwise(patient_groups),efficacy=simGroup(n_patients)) %>% 
    select(age_group,efficacy)
}


results <- map_df(1:n_simulations,function(n) simTrial(patient_groups) %>% mutate(sim_num=n))

summarised_results <- group_by(results,sim_num) %>% 
  summarise(max_efficacy=max(efficacy,na.rm=TRUE),
            min_efficacy=min(efficacy,na.rm=TRUE),
            win=all(efficacy >= eff.min) && all(efficacy <= eff.max),
            win=replace_na(win,FALSE)
  )

table(summarised_results$win)


#sure, why not plot it. 

summarised_results %>% 
  arrange(min_efficacy,win,max_efficacy) %>% 
  ggplot(aes(x=1:nrow(summarised_results),ymin=min_efficacy,ymax=max_efficacy,color=win)) + 
  geom_linerange() + 
  xlab("Trial simulations in rank order of minimum effect size") + 
  ylab("effect size range in patient age groups per simulated trial") + 
  geom_hline(yintercept=eff.min) + geom_hline(yintercept=eff.max) + 
  coord_flip() + 
  cowplot::theme_cowplot()