# size computation
# 
all_data <- read_rds("simulations/supplementary/test-hpc/data-agg/all_data.rds")


# computes size for 99 percentile threshold
data_summary <- all_data %>% 
  mutate(sig = if_else(significance!=99, 0, 99)) %>% 
  group_by(seed_id) %>% 
  summarize(sumsig = sum(sig)) %>% 
  count(sumsig==0) %>% 
  mutate(p_value = n/sum(n)) %>% 
  slice(1)


# plots
ggplot() + 
  geom_histogram(data = all_data, aes(x = wpd))  + 
  geom_vline(data = data_summary, aes(xintercept = p_value),colour = "red", size= 1.5) 


# computes size for 95 percentile threshold
data_summary <- all_data %>%
  mutate(sig = if_else(!(significance %in%  c(99, 95)), 0, 95)) %>% 
  group_by(seed_id) %>% 
  summarize(sumsig = sum(sig))

data_summary %>% 
  count(sumsig==0)

size = 0.3



# computes size for 95 percentile threshold
data_summary <- all_data %>%
  mutate(sig = if_else(!(significance %in%  c(99)), 0, 99))

data_summary %>% 
  count(sig==0)


# plot
  
# p_data <- all_data %>% 
#     mutate(sig = if_else(significance!=99, 0, 99)) %>% 
#     group_by(x_levels, facet_levels) %>% 
#     summarise(p_value = sum(if_else(sig == 99,1,0))/n(),
#               .groups = 'drop') %>%
#     summarise(p_value = sum(p_value))
#     

+
  geom_text(data = round(compute_p_value, 3), size = 3,  
            aes(x = -Inf,
                y =  Inf,
                label = paste("p-value:",p_value),
                hjust   = 0,
                vjust   = 1)) + 
  facet_grid(nx ~ nfacet)


all_data %>% ggplot() + geom_density(aes(x = wpd)) + 
  facet_grid(x_levels~facet_levels)
