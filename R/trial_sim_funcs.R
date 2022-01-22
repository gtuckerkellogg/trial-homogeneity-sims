## Read tab-separated patient group file
## required columns: 
## - group (age group)
## - n_control (patients in control group) 
## - n_treatment (patients in treatment group
## optional columns: 
## - i_control (infections in control group) 
## - i_treatment (infections in treatment group)

library(furrr)


import_patient_groups <- function(fn) {
  data <- readr::read_tsv(fn,show_col_types=FALSE)
  if (all(c('group','n_control','n_treatment') %in% names(data))) {
    if (all(c('i_control','i_treatment') %in% names(data))) {
      data <- 
        dplyr::mutate(data,
               n_patients=n_treatment+n_control,
               control_ir=i_control/n_control,
               treatment_ir=i_treatment/n_treatment)
    }
    data  
  } else { 
    warning('data not imported correctly')}
  
}


## calculate overall infection rates from per-group infections
## in the data frame
## return named vector of rates
overall_infection_rates <- function(patient_group_df) { 
  with(patient_group_df,
    c(control=sum(i_control)/sum(n_control),
      treatment=sum(i_treatment)/sum(n_treatment)))
  }


simGroup <- function(n_patients,control_ir,treatment_ir,treatment_prob) { 
  # simulate a group of n_patients with given infection rates, randomly assigned into treatment or control groups
  # according to the given probability
  # return efficacy if calculable, NA if not
  infection_rates <- c(control=control_ir,treatment=treatment_ir)
  treatment_group <- factor(rbinom(n_patients, 1, treatment_prob)) %>%
    forcats::fct_recode("control"="0","treatment"="1")
  infection_prob <- infection_rates[treatment_group]
  infection <- rbinom(n_patients,1,infection_prob)
  cont <- table(infection,treatment_group)
  
  if (all(cont >0) && length(cont)>3){ #this checks there are no 0 cells (any 0 cell obviously means efficacy outside our range of 0%, 100% or incalculable)
    suppressWarnings(oops <- epitools::oddsratio(cont)) #calculates OR for this age group in this simulation run
    efficacy <- 1 - oops$measure[2] #calculates VE as "1-OR" as defined in the paper
  } else { 
    efficacy <- 1 # Kyle replaced with 1
  }
  efficacy
}


simTrial <- function(patient_groups,treatment_prob,as_equal=TRUE) { 
  # simulate a trial of patient groups passed in as a data frame
  if (as_equal) { 
    ir <- overall_infection_rates(patient_groups)
    patient_groups <- dplyr::mutate(patient_groups,
                                    control_ir=ir[['control']],
                                    treatment_ir=ir[['treatment']])
  }
 dplyr:: mutate(rowwise(patient_groups),efficacy=simGroup(n_patients,control_ir,treatment_ir,treatment_prob))
}

simTrials <- function(patient_groups,n_simulations,treatment_prob,as_equal=TRUE) {
   pb <- progress::progress_bar$new(total=n_simulations)
   pb$tick(0)
   map_dfr(1:n_simulations,
                function(.n) {
                 pb$tick()
                    simTrial(patient_groups,treatment_prob,as_equal) %>% mutate(sim_num=.n)})
  
  
}


parallel_simTrials <- function(patient_groups,n_simulations,treatment_prob,as_equal=TRUE) {
    future::plan(multicore)
    print("\n")
    furrr::future_map_dfr(1:n_simulations,
                          function(.n) {
                              simTrial(patient_groups,treatment_prob,as_equal) %>% mutate(sim_num=.n)},
                          .options= furrr::furrr_options(seed=TRUE),
                          .progress=TRUE)
}


summariseTrials <- function(results,eff.min,eff.max) { 
  group_by(results,sim_num) %>% 
    summarise(max_efficacy=max(efficacy,na.rm=TRUE),
              min_efficacy=min(efficacy,na.rm=TRUE),
              win=all(efficacy >= eff.min) && all(efficacy <= eff.max),
              win=replace_na(win,FALSE)
    )
  }


plot_results <- function(results,eff.min,eff.max) {
  summarised_results <- summariseTrials(results,eff.min,eff.max)
  summarised_results %>%
    arrange(min_efficacy,win,max_efficacy) %>% 
    ggplot(aes(x=1:nrow(summarised_results),ymin=min_efficacy,ymax=max_efficacy,color=win)) + 
      geom_linerange() + 
      geom_hline(yintercept=eff.min) + geom_hline(yintercept=eff.max) +
      xlab("Simulation") +
      ylab("efficacy range across age groups") + 
      coord_flip()
}

