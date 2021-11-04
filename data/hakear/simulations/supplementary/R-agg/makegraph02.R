library(here)
library(readr)
library(tidyverse)
# run for  norm MMPD files aggregation
#all_data <- read_rds("simulations/result_report/all_data_ norm.rds")
# run for  norm max pairwise distances files aggregation


all_data <- read_rds("simulations/supplementary/data-agg/wpd_N01/all_data.rds")


# raw categories
draw_design_by_categories <- all_data %>% 
  filter(ntimes == 500, w == 1) %>% 
  ggplot()+
  ggridges::geom_density_ridges(aes(x = value, y = design)) + 
  #geom_boxplot(aes(fill = design), alpha = 0.5) +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd")
  
  
  +
  scale_x_continuous(breaks = scales::breaks_extended(3))
