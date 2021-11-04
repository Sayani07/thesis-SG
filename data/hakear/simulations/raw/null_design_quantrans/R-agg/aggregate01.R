# contains function to aggregate data each for from N(0,1), Gamma(2, 1)
#  aggregated data from the file ../data-ind 
library(tidyverse)
library(here)
library(readr)
library(rlang)

aggregate01 <-  function(folder_name){
  
all_files = list.files(path = paste0("simulations/raw/null_design_quantrans/data-ind/", folder_name), 
                       pattern = ".rds")

names_levels <- map_dfr(all_files, 
                        function(x){
                          z = str_split(str_remove(x, "_dist.rds"), "_") %>% 
                            unlist()
                          bind_cols(nx = as.numeric(z[1]),
                                    nfacet = as.numeric(z[2]))
                        })

  
all_files_path <- paste0("simulations/raw/null_design_quantrans/data-ind/",folder_name,"/",
         all_files)  
  

all_data <- lapply(1:length(all_files_path), function(x){
    
  data = all_files_path %>% magrittr::extract2(x) %>% 
    readRDS()
  
  names = names_levels %>% magrittr::extract(x,)
  names_rep =   names %>% slice(rep(1:n(), each = nrow(data)))
  bind_cols(names_rep, data)
  }) %>% bind_rows() %>% 
    arrange(nfacet, nx)
    
write_rds(all_data, paste0("simulations/raw/null_design_quantrans/data-agg/all_data_", folder_name, ".rds"))
}


# use functions for all folders

aggregate01(folder_name = "wpd_N01")
aggregate01(folder_name = "wpd_N05")
aggregate01(folder_name = "wpd_N51")
aggregate01(folder_name = "wpd_N55")


aggregate01(folder_name = "wpd_Gamma01")
aggregate01(folder_name = "wpd_Gamma21")


