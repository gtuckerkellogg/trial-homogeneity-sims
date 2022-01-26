# Plausibility of claimed Covid-19 Vaccine Efficacies by Age: A Simulation Study

## Kyle A Sheldrick, Gideon Meyerowitz-Katz, Greg Tucker-Kellogg

The central question: “how likely is this range of results if the point estimate of efficacy is the true underlying efficacy?". Point estimates of efficacy are obtained from the entire cohort and from individual patient age groups. The question here is whether a simulation model with the reported overall efficacy is able to reproduce the ranges of reported per patient-group efficacies. If it is difficult to produce ranges as precise as reported, the interpretation is that it is difficult to achieve that level of consistency in the ideal case, and unlikely to be generated from experimental data without other supporting evidence.


## Required R packages
- `epitools`
- `tidyverse`
- `patchwork`
- `furrr`
- `here`

## How to reproduce results

- All code is in the `R` directory
- Study data is in the `data` directory
- Results are in the `results` directory

- Figure 1 and Figure 2 can be reproduced by running `R/head-to-head.R`
- Individual trial simulations can be examined and modified by looking for the appropriately named file in the `R` directory.

