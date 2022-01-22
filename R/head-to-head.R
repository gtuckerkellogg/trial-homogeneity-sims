## head to head comparisons,
## parallel computation of simulations
## # of repetitions calculated and plotted from the data
## plots for individual simulations available and saved.


library(tidyverse)
library(here)
library(furrr)
set.seed(1)
library(patchwork)
source(here('R/trial_sim_funcs.R'))


get_treatment_allocation <- function(AR) {
    AR <- as.numeric(str_split(AR,":",simplify=TRUE))
    AR[1]/sum(AR)
}


## For Figure 1

n_simulations <- 1000

fig1_results <- read_tsv(here('data/studies.tsv'),show_col_types=FALSE) %>%
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
           trials_to_win=n_simulations/sum(summary$win)) %>%
    mutate(plt=list(plot_results(sim,min_eff,max_eff)+
                    ggtitle(sprintf("%s: %d wins",study,sum(summary$win))) +
                    ylim(0,1) + theme_bw() + 
                    theme(legend.position='none')))



test_fig <- fig1_results %>%
    mutate(plt=list(plot_results(sim,min_eff,max_eff)+
                    ggtitle(sprintf("%s: %d wins",study,sum(summary$win))) +
                    ylim(0,1) + theme_bw()))

fig1_legend  <-     cowplot::get_legend(test_fig$plt[[1]] +
                                        theme(legend.key.size = unit(1.5, "cm"),
                                              legend.key.width = unit(1.5,"in")) +
                                        scale_color_discrete(name="Simulation efficacy range within\ntrial reported efficacy range")
                                        )


pdf(here('results/fig1.pdf'),width=210/25.4,,height=297/25.4)
(test_fig$plt[[1]] +
    test_fig$plt[[2]] +
    test_fig$plt[[3]] +
    test_fig$plt[[4]] +
    test_fig$plt[[5]]) * theme(legend.position='none') +
    fig1_legend + 
    plot_layout(ncol=2) +  plot_annotation(tag_levels = list(c('A','B','C','D','E')))
dev.off()

           
           
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


results <- results %>% 
    mutate(plt=list(plot_results(sim,min_eff,max_eff)+
                    ggtitle(sprintf("%s: %d wins",study,sum(summary$win))) +
                    ylim(0,1) + theme_bw() + 
                    theme(legend.position='none')))


fig2a <- results$plt[[5]]
    
fig2b <- ggplot(results,aes(x=study,y=trials_to_win,label=round(trials_to_win,1))) + 
  geom_col(width=0.5) + geom_text(position=position_dodge(width=0.9), vjust=-0.25) + 
  ylab("trial repetitions") + xlab("") + 
    cowplot::theme_cowplot()  +
    scale_x_discrete(guide = guide_axis(angle = 45),labels=function(x){sub("/", "/\n", x)}) +
    theme(axis.text.x = element_text(size = rel(0.8)))

pdf(here('results/fig2.pdf'),width=210/25.4,,height=(297/2.5)/25.4)
fig2a + fig2b +  plot_annotation(tag_levels = 'A')
dev.off()

save.image()
                
