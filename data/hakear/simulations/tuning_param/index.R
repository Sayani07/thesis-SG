library(hakear)
library(tidyverse)
library(parallel)
library(distributional)
source("simulations/functions_simulate_designs/vary_mean.R")

mean = 0
sd = 1
range_w =  seq(1, 10, 1)
range_lambda = seq(0.1, 0.9, 0.05)
simtable<-read_csv(here::here('simulations/null/sim_table.csv'))


### changing mean with constant sd

set.seed(9999)

data_varx <- mclapply(seq_len(nrow(simtable)),
         function(j){
           scen<- j 
           simj<-simtable[scen,] #Extract row of table
           nfacetj<-simj$nfacet # Which nfacet level
           nxj<-simj$nx #Which nx level

data_varx <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    #data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = nxj,
                        nfacet = nfacetj,
                        ntimes = 500,
                        sim_dist = 
                          sim_varx_normal_varymean(nx = nxj, nfacet = nfacetj, mean = mean, sd = sd, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data, lambda = x)
      
    bind_cols(omega = w, wpd = wpd)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>% bind_rows() 


data_varf <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    #data_y <- mclapply(range_mean, function(y){   
    # generate the data
    data <- sim_panel(nx = nxj,
                      nfacet = nfacetj,
                      ntimes = 500,
                      sim_dist = 
                        sim_varf_normal_varymean(nx = nxj, nfacet = nfacetj, mean = mean, sd = sd, w = w)) %>%
      unnest(c(data))
    
    # compute wpd
    wpd <- compute_pairwise_max(data, 
                                gran_x = "id_x",
                                gran_facet = "id_facet",
                                response = sim_data, lambda = x)
    
    bind_cols(omega = w, wpd = wpd)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>% bind_rows() 


data_all <- bind_rows(data_varx, data_varf, .id = "design") %>% 
  mutate(design = if_else(design == "1", "vary_x", "vary_facet"))

saveRDS(data_all, paste0('simulations/results/raw/tuning_param/',
                         nxj,'_',
                         nfacetj,'_tuning_param.rds'))
})



