# This script computes size of the test in our paper and shows it is close to 95%

library(readr)
library(dplyr)
library(distributional)
library(gravitas)
library(here)
library(purrr)
library(tidyverse)
library(purrr)
library(hakear)

# make choices for x levels and seed for which size needs to be computed
simtable <-  expand.grid(x_levels = c(3, 7, 14),
                         facet_levels = c(2, 9, 10)) %>%
  mutate(design = c("null", "varf","varx", "varall", "null", "varf", "varx", "varall", "null"))

sample_seed = seq(1000, 10999, by = 10)
npermj <- 200
ntimesj <- 500
nsampj <- 100
wj <- 2

# different designs

sim_null <- function(nxj, nfacetj, mean, sd, w = 0){
  rep(distributional::dist_normal(mu = mean, sigma = sd),
      times = nxj*nfacetj)}

sim_varx <- function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nx-1, by  = 1)*w), sd), nfacet)
}

sim_varf <- function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nfacet-1, by  = 1)*w), sd), each = nx)
}

sim_varall <- function(nx, nfacet, mean, sd, w)
{
  dist_normal((mean + seq(0,
                          (nx*
                             nfacet - 1), by  = 1)*w), sd)
}



sim_harmony_sim <-   map(seq_len(length(sample_seed)),
                         function(seedj){
                           set.seed(seedj) 
  ## code for one harmony table starts
  sim_data_all <- map(seq_len(nrow(simtable)),
                      function(x){
                        y = simtable %>% magrittr::extract(x,)
                        
                        # assigning different rows for different design
                        
                        # setting value of parameters
                        designj = y$design
                        nfacetj<- y$facet_levels # Which nfacet level
                        nxj<- y$x_levels
                    
                        
                        if(designj=="null")
                          sim_function = sim_null
                        else if(designj=="varx")
                          sim_function =  sim_varx
                        else if(designj=="varf")
                          sim_function =  sim_varf
                        else
                          sim_function =  sim_varall
                        
                        # making data for each row using customized sim_function
                     
                        sim_panel_data =
                          hakear::sim_panel(nx = nxj,
                                            nfacet = nfacetj,
                                            ntimes = ntimesj,
                                            sim_dist = sim_function(nx = nxj,
                                                                    nfacet = nfacetj,
                                                                    mean = 0,
                                                                    sd = 1,
                                                                    w = wj)) %>%
                          unnest(c(data)) %>%
                          ungroup()
                        
                        # graphs to check if data is appropriately generated
                        
                        # sim_panel_data %>% unnest(c(data)) %>%
                        #   ggplot() +
                        #   geom_boxplot(aes(x=as.character(id_x), y = sim_data)) + facet_wrap(~id_facet)
                        
                        # finding wpd_norm for each row
                        ## need to use wpd
                        
                        
                      })
  
  names(simtable) = c("x_levels", "facet_levels", "design")
  
  wpd_threshold(sim_data_all, harmony_tbl = simtable, create_harmony_data = FALSE, response = sim_data, nsamp = nsampj, nperm = npermj)
  
  ## code for one harmony table ends
  
}) %>% bind_rows(.id = "seed_id")


write_csv(sim_harmony_sim, "simulations/supplementary/test-hpc/sim_harmony_sim.csv")

