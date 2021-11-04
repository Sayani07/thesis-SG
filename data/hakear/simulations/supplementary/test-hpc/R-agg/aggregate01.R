#  aggregated data from the file simulations/results/norm/folder_name and tidy them up
library(tidyverse)
library(here)
library(readr)
library(rlang)
  
all_files = list.files(path = paste0("simulations/supplementary/test-hpc/data-ind/"), 
                       pattern = ".csv")

names_levels <- map(all_files, 
                        function(x){
                          z = str_remove(str_remove(x, "null-oneseed-id-"), ".csv") 
                        }) %>% unlist()

  
all_files_path <- paste0("simulations/supplementary/test-hpc/data-ind/",
         all_files)  
  

all_data <- lapply(1:length(all_files_path), function(x){
    
  data = all_files_path %>% magrittr::extract2(x) %>% 
    read_csv()
  
  names = rep(names_levels[x], each = nrow(data))
  data = data %>% mutate(seed_id = names, 
                         select_harmony = as.character(select_harmony)) %>% mutate(significance = if_else(!is.na(str_extract(select_harmony, "\\*\\*\\*")), 99, if_else(!is.na(str_extract(select_harmony, "\\*\\*")), 95, if_else(!is.na(str_extract(select_harmony, "\\*")), 90, 0))))
  }) %>% bind_rows() 



 write_rds(all_data, paste0("simulations/supplementary/test-hpc/data-agg/all_data.rds"))



