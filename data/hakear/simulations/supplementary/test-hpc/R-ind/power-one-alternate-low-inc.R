# This script computes power of the test in our paper 
# low increment : 0.5 , medium increment : 2, high increment: 5
# initial dist : N(0, 1)
# only one alternate considered 
# since all the harmonies need to be run at once, you can only run the seeds parallely, not all the harmonies.
# sim_table should only contain the seeds in that case

library(readr)
library(dplyr)
library(distributional)
library(gravitas)
library(here)
library(purrr)
library(tidyverse)
library(purrr)
library(hakear)

# harmony_table<-read_csv(here::here('simulations/sim_table/sim_table.csv'))

sample_seed <- seq(1000, 10999, by = 10) %>% as_tibble()
simtable <- sample_seed
scen <- as.numeric(commandArgs()[[6]])
#scen = 2 # testing if works for one seed
simj <- simtable[scen, ] # Extract row of table
seedj <- simj$value # which seed with which simulations should be run


harmony_table <- expand.grid(
  x_levels = c(3, 7, 14),
  facet_levels = c(2, 9, 10)
) %>%
  mutate(design = c("null", "null", "varall", "null", "null", "null", "null", "null", "null"))

# parameters and design used for this simulation
npermj <- 200
ntimesj <- 500
nsampj <- 100
wj <- 0.5 # change this to change increment

sim_null <- function(nxj, nfacetj, mean, sd, w = 0) {
  rep(distributional::dist_normal(mu = mean, sigma = sd),
      times = nxj * nfacetj
  )
}
sim_varall <- function(nx, nfacet, mean, sd, w)
{
  dist_normal((mean + seq(0,
                          (nx*
                             nfacet - 1), by  = 1)*w), sd)
}

set.seed(seedj)
## code for one harmony table starts
sim_data_all <- map(
  seq_len(nrow(harmony_table)),
  function(x) {
    y <- harmony_table %>% magrittr::extract(x, )
    
    # assigning different rows for different design
    
    # setting value of parameters
    designj <- y$design
    nfacetj <- y$facet_levels # Which nfacet level
    nxj <- y$x_levels
    
    
    if (designj == "null") {
      sim_function <- sim_null
    } else if (designj == "varx") {
      sim_function <- sim_varx
    } else if (designj == "varf") {
      sim_function <- sim_varf
    } else {
      sim_function <- sim_varall
    }
    
    # making data for each row using customized sim_function
    
    sim_panel_data <-
      hakear::sim_panel(
        nx = nxj,
        nfacet = nfacetj,
        ntimes = ntimesj,
        sim_dist = sim_function(
          nx = nxj,
          nfacet = nfacetj,
          mean = 0,
          sd = 1,
          w = wj
        )
      ) %>%
      unnest(c(data)) %>%
      ungroup()
  }
)

names(harmony_table) <- c("x_levels", "facet_levels", "design")

null_harmony_oneseed <- wpd_threshold(sim_data_all,
                                      harmony_tbl = harmony_table,
                                      create_harmony_data = FALSE,
                                      response = sim_data,
                                      nsamp = nsampj,
                                      nperm = npermj
)

## code for one harmony table ends

write_csv(
  null_harmony_oneseed,
  paste0("../data-ind-power-one-alternate-low-inc/", "seed-id-", scen, ".csv")
)


# locally
# write_csv(
#   null_harmony_oneseed,
#   paste0("simulations/supplementary/test-hpc/data-ind-power-one-alternate-low-inc/", "seed-id-", scen, ".csv")
# )
# 
