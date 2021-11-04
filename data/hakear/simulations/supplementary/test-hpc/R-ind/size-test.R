library(readr)
library(dplyr)
library(hakear)
library(distributional)
library(gravitas)
library(here)
sample_seed = seq(1000,2999, by = 1000)
nperm = 2

#scen <-as.numeric(commandArgs()[[6]])
sample_seedj <- sample_seed[scen]
#sample_seedj = 1000

simtable<-read_csv(here::here('simulations/sim_table/sim_table.csv'))

set.seed(sample_seedj)

library(distributional)
sim_dist <- distributional::dist_normal(0,1)
sim_orig <-  distributional::generate(sim_dist, 50000)
sim_orig <- sim_orig[[1]]
lensim <- length(sim_orig)

# for each row of the sim table
each_seed <- lapply(seq_len(nrow(simtable)), 
                    function(scen){
                      
                      simj<-simtable[scen,] 
                      nfacetj<-simj$nfacet
                      nxj<-simj$nx
                      
                      id_x <- rep_len(seq_len(nxj), length.out = lensim) 
                      id_facet <- rep_len(rep(seq_len(nfacetj), each = nxj), length.out = lensim) 
                      
                      sim_panel_orig <- bind_cols(id_x = id_x, id_facet = id_facet, sim_data = sim_orig)
                      
                      wpd_orig = compute_pairwise_norm_scalar(sim_panel_orig, 
                                                              gran_x = "id_x",
                                                              gran_facet = "id_facet",
                                                              response = sim_data)
                      
                      wpd_sample <- lapply(seq_len(nperm), function(x){
                        
                        sample_data <- sample(sim_orig, size =  lensim)
                        
                        sim_panel_sample <- bind_cols(id_x = id_x, 
                                                      id_facet = id_facet, 
                                                      sim_data = sample_data)
                        
                        wpd =  compute_pairwise_norm_scalar(sim_panel_sample, 
                                                            gran_x = "id_x",
                                                            gran_facet = "id_facet",
                                                            response = sim_data) %>% 
                          as_tibble() %>% mutate(perm_id = x)
                        
                      }) %>% bind_rows()
                      
                      wpd_all <- wpd_orig %>% as_tibble() %>% 
                        mutate(perm_id = 0) %>% 
                        bind_rows(wpd_sample) %>% 
                        bind_cols(nx = nxj, nfacet = nfacetj)
                      
                      wpd_all
                    }) %>%
  bind_rows()

# find threshold

threshold <- each_seed %>% mutate(
  threshold99 = quantile(each_seed$value, probs = 0.99),
  threshold95 = quantile(each_seed$value, probs = 0.95),
  threshold90 = quantile(each_seed$value, probs = 0.90))

# conduct test

threshold_data <- threshold %>% 
  dplyr::filter(perm_id == 0) %>% 
  mutate(select99 = if_else(value>threshold99, "yes", "no"),
         select95 = if_else(value>threshold95, "yes", "no"),
         select90 = if_else(value>threshold90, "yes", "no")) %>% 
  mutate(seed_id = sample_seedj)


saveRDS(threshold_data,
        paste0('simulations/supplementary/test-hpc/data-ind/wpd_N01/',
               #"type-", 
               sample_seedj, "-",
               'seed_id.rds')) # seed and dist not included yet


