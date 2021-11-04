
# contains function to aggregate data each for from N(0,1), N(0,5), N(5,1), N(5,5), Gamma(0.5, 1), Gamma(2, 1)
#  aggregated data from the file simulations/results/norm/folder_name and tidy them up
library(tidyverse)
library(here)
library(readr)
library(rlang)

  all_files = list.files(path = paste0("simulations/supplementary/data-ind/wpd_N01"), 
                         pattern = ".rds")

  names_levels <- map_dfr(all_files, 
                          function(x){
                            z = str_split(str_remove(x, "-wpd.rds"), "-") %>% 
                              unlist()
                            bind_cols(type = z[1],
                                      design = z[2],
                                      nx = as.numeric(z[3]),
                                      nfacet = as.numeric(z[4]),
                                      w = as.numeric(z[5]),
                                      nperm = as.numeric(z[6]),
                                      ntimes = as.numeric(z[7])
                                      )
                          })

  
  all_files_path <- paste0("simulations/supplementary/data-ind/wpd_N01/",
                           all_files)  
  
  
  all_data <- lapply(1:length(all_files_path), function(x){
    
    data = all_files_path %>% magrittr::extract2(x) %>% 
      readRDS()
    
    names = names_levels %>% magrittr::extract(x,)
    
    names_rep =   names %>% slice(rep(1:n(), each = nrow(data)))
    bind_cols(names_rep, data)
    
  }) %>% bind_rows()
  
  write_rds(all_data, paste0("simulations/supplementary/data-agg/wpd_N01/all_data.rds"))

