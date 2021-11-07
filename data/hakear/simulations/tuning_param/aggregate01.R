# contains aggregated data from the file simulations/results/raw/ and tidy them up

library(tidyverse)
library(here)
library(readr)
library(rlang)

all_files = list.files(path = "simulations/results/raw/tuning_param", 
                       pattern = ".rds")


names_levels <- map_dfr(all_files, 
                        function(x){
                          z = str_split(str_remove(x, "_dist.rds"), "_") %>% 
                            unlist()
                          bind_cols(nx = as.numeric(z[1]),
                                    nfacet = as.numeric(z[2]))
                        })

len_file = read_rds(("simulations/results/raw/tuning_param/2_2_tuning_param.rds"))

names_rep <- names_levels %>% slice(rep(1:n(), each = nrow(len_file)))

  
all_files_path <- paste0("simulations/results/raw/tuning_param/",
         all_files)  
  

all_data <- lapply(1:length(all_files_path), function(x){
    
  data = all_files_path %>% magrittr::extract2(x) %>% 
    readRDS()
  
  names = names_levels %>% magrittr::extract(x,)
  names_rep =   names %>% slice(rep(1:n(), each = nrow(data)))
  bind_cols(names_rep, data)
  }) %>% bind_rows() %>% 
    arrange(nfacet, nx)
    
    

# run for raw MMPD files aggregation
#write_rds(all_data, "simulations/result_report/all_data_raw.rds")

# run for raw max pairwise distances files aggregation
write_rds(all_data, "simulations/tuning_param/all_data.rds")
# 
# write_rds(all_data, "simulations/result_report/all_data_norm_mmpd.rds")
