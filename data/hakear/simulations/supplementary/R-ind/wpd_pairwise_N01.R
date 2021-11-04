#This script calculates raw mmpd for each simulation scenario
##Read Simulation Table

.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(drake)
library(tidyverse)
library(hakear)

# seed has been set later in the code when sampling part comes - to be included as a parameter later at least for two more seeds results hold true

nsim = 200
# change path while running it on HPC
simtable<-read_csv(here::here('simulations/supplementary/sim_table/sim_table.csv'))

### Extract flags from simulation scenario

#scen<- 28800 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
nfacetj<-simj$nfacet # Which nfacet level
nxj<-simj$nx #Which nx level
wj <- simj$w
designj <- simj$design
npermj <- simj$nperm
ntimesj <- simj$ntimes
distj <- simj$dist
seedj <- simj$seed
typej <- simj$type


if(designj=="null")
{
  sim_function <-  function(nxj, nfacetj, mean, sd, w = 0){
    rep(distributional::dist_normal(mu = mean, sigma = sd), 
        times = nxj*nfacetj)
  }
}
if(designj=="vary_x")
{
  sim_function <- function(nx, nfacet, mean, sd, w)
  {
    rep(dist_normal((mean + seq(0, nx-1, by  = 1)*w), sd), nfacet)
  }
  
}
if(designj=="vary_f")
{
  sim_function <- function(nx, nfacet, mean, sd, w)
  {
    rep(dist_normal((mean + seq(0, nfacet-1, by  = 1)*w), sd), each = nx)
  }
  
}
if(designj=="vary_all"){
  
  sim_function <- function(nx, nfacet, mean, sd, w)
  {
    dist_normal((mean + seq(0, (nx*nfacet - 1), by  = 1)*w), sd)
  }
  
  
}

#create data for each row for null normal

set.seed(seedj)


sim_panel_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = ntimesj, 
                    sim_dist = sim_function(nx = nxj, nfacet = nfacetj,
                                            mean = 0, sd = 1, w = wj)) %>% 
  unnest(c(data)) %>% ungroup() %>% 
  bind_cols(w = wj)

# permuting the data keeping other columns constant  


if(typej == "raw")
{
dist_data <- map(seq_len(nsim), function(i)
{
  new_sim_data = sample(sim_panel_data$sim_data, 
                        size = nrow(sim_panel_data))
  
  new_data = sim_panel_data %>% 
    select(-sim_data) %>% 
    mutate(sim_data = new_sim_data)

  # for creating one raw mmpd
  wpd = compute_pairwise_max(new_data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data) %>% 
    as_tibble() %>% mutate(perm_id = i)
}) %>% bind_rows()
}


if(typej == "norm-nperm")
{
  dist_data <- map(seq_len(nsim), function(i)
  {
    new_sim_data = sample(sim_panel_data$sim_data, 
                          size = nrow(sim_panel_data))
    
    new_data = sim_panel_data %>% 
      select(-sim_data) %>% 
      mutate(sim_data = new_sim_data)
    
    # for creating one raw mmpd
    wpd = compute_pairwise_norm(new_data, 
                                     gran_x = "id_x",
                                     gran_facet = "id_facet",
                                     response = sim_data,
                                     nperm = npermj, 
                                seed = seedj) %>% 
      as_tibble() %>% mutate(perm_id = i)
    
  }) %>% bind_rows()
}

if(!dir.exists("simulations/supplementary/data-ind/wpd_N01"))
{
  dir.create("simulations/supplementary/data-ind/wpd_N01")
}


if(!dir.exists("simulations/supplementary/data-ind/wpd_N01"))
{
  dir.create("simulations/supplementary/data-ind/wpd_N01")
}

saveRDS(dist_data,
        paste0('simulations/supplementary/data-ind/wpd_N01/',
               #"type-", 
               typej, "-",
               #"design-",
               designj, "-",
               #"nxj-", 
               nxj,'-',
               #"nfacetj-",
               nfacetj,"-",
               #"w-",
                wj,"-",
               #"nperm-",
               npermj,"-",
               #"ntimes-",
               ntimesj,
               '-wpd.rds')) # seed and dist not included yet

