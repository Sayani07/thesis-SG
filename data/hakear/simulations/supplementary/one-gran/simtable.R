### This code creates a table of data for different simulations scenarios ###
library(tidyverse)
# nperm and nsim can run parallelly
sim_table <- expand.grid(
   nfacet = 1,
   nx  = c(2, 3, 5, 7, 9, 14, 17, 20, 24, 31, 42, 50)
   #nx = c(2, 3, 5, 7, 14, 20, 31, 50) 
) %>% tibble()

#Export as .csv

write_csv(sim_table,'simulations/supplementary/one-gran/sim_table.csv')
