library(tidyverse)
library(here)

trial_comparisons <- read_csv(here("data/trial-comparison.csv"))

trial_comparisons

fig2 <- ggplot(trial_comparisons,aes(x=Trial,y=repetitions,label=repetitions)) + 
  geom_col(width=0.5) + geom_text(position=position_dodge(width=0.9), vjust=-0.25) + 
  ylab("trial repetitions") + xlab("") + 
  ggtitle("Number of trial repetitions needed on average for age group\nefficacies to fall within bounds reported in trial") + 
  cowplot::theme_cowplot() 


pdf(here('results','fig2.pdf'),width=8,height=6)
fig2
dev.off()
