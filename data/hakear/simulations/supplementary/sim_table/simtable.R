### This code creates a table of data for different simulations scenarios ###
library(tidyverse)
# nperm and nsim can run parallelly
sim_table <- expand.grid(#data = c(99, 9999), # different data with same specifications
  nperm = c(10, 30, 50, 100, 300),
  #nperm = c(10, 30, 500),
  ntimes = c(5, 30, 100, 500),
  #ntimes = c(5, 500),
  # different permutations to compute mean and sd
  #nobs_each_comb  = c(10, 40, 500), # different number of observations for each combinations for each panel
  #nobs_diff_comb  = c("comb1", "comb2"), # different number of observations for different combinations for each panel
  #nsim = c(100, 500), # number of simulations of the entire exercise to compute distribution and CI of MMPD 
  #dist = c("N01", "N05", "N51", "N55", "G01", "G21"), #
  dist = c("N01"), #distributions from where data is generated
   nfacet = c(5, 7, 14, 20, 31), # range of facet levels 
  # 50 removed for now
   #nfacet = c(2, 3),
   #nx = c(2, 3)
   nx = c(5, 7, 14, 20, 31), # range of x levels
  design = c("null", "vary_x", "vary_f", "vary_all"),
  #seed = seq(1500, 7500, 1000),
  seed = c(1500),
  #w = c(seq(1, 20, 7), 50),
  w = c(1, 10, 50),
  type = c("norm-nperm")
             
) %>% tibble()


#Export as .csv

write_csv(sim_table, here::here('simulations/supplementary/sim_table/sim_table.csv'))
