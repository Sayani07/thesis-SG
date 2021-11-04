#This script calculates raw mmpd for each simulation scenario
##Read Simulation Table

.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(drake)
library(tidyverse)
library(hakear)

set.seed(9999)

nsim = 200
# change path while running it on HPC
simtable<-read_csv(here::here('simulations/supplementary/one-gran/sim_table.csv'))
#simtable<-read_csv('../../../sim_table/sim_table.csv')

### Extract flags from simulation scenario

#scen<- 2 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
nfacetj<-simj$nfacet # Which nfacet level
nxj<-simj$nx #Which nx level

#create data for each row for null normal

sim_null_normal = function(nxj, nfacetj){
  rep(distributional::dist_gamma(2, 1), 
      times = nxj*nfacetj)
}

sim_panel_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_null_normal(nxj, nfacetj)) %>% 
  unnest(c(data)) %>% ungroup()

set.seed(1111)

raw_dist <- map(seq_len(nsim), function(i)
{
  new_sim_data = sample(sim_panel_data$sim_data, 
                        size = nrow(sim_panel_data))
  new_data = sim_panel_data %>% 
    select(-sim_data) %>% 
    mutate(sim_data = new_sim_data)
  
  # for creating one raw mmpd
  raw_mmpd = compute_pairwise_max(new_data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data) %>% 
    as_tibble() %>% mutate(perm_id = i)
  
}) %>% bind_rows()

saveRDS(raw_dist,
        paste0('simulations/supplementary/one-gran/raw/null_design_quantrans/data-ind/wpd_Gamma21/',
               nxj,'_',
               nfacetj,'_wpd.rds')) 
