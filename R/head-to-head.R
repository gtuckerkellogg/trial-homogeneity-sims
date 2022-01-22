## head to head comparisons,
## parallel computation of simulations
## # of repetitions calculated and plotted from the data
## plots for individual simulations available and saved.


library(tidyverse)
library(here)
library(furrr)
set.seed(1)
source(here('R/trial_sim_funcs.R'))


get_treatment_allocation <- function(AR) {
    AR <- as.numeric(str_split(AR,":",simplify=TRUE))
    AR[1]/sum(AR)
}


n_simulations <- 50000

results <- read_tsv(here('data/studies.tsv'),show_col_types=FALSE) %>%
    rowwise() %>% 
    mutate(data_file=here('data',data_file),
           treatment_allocation = get_treatment_allocation(allocation_ratio)
           ) %>%
    group_by(study,data_file,treatment_allocation,min_eff,max_eff) %>%
    nest() %>%
    mutate(patient_groups=map(data_file,import_patient_groups),
           n_simulations=n_simulations) %>%
    rowwise() %>%
    mutate(sim=list(parallel_simTrials(patient_groups,n_simulations,treatment_allocation)),
           summary=list(summariseTrials(sim,min_eff,max_eff)),
           trials_to_win=n_simulations/sum(summary$win))

fig2 <- ggplot(results,aes(x=study,y=trials_to_win,label=round(trials_to_win,1))) + 
  geom_col(width=0.5) + geom_text(position=position_dodge(width=0.9), vjust=-0.25) + 
  ylab("trial repetitions") + xlab("") + 
  ggtitle("Number of trial repetitions needed on average for age group\nefficacies to fall within bounds reported in trial") + 
  cowplot::theme_cowplot() 

#pdf(here('results','fig2.pdf'),width=8,height=6)
fig2
#dev.off()

results <- results %>% mutate(plt=list(plot_results(sim,min_eff,max_eff)))

save.image()
                
