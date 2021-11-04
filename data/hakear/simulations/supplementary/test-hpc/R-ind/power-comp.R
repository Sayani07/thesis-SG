all_data <- read_rds("simulations/supplementary/test-hpc/data-agg/all_data-power-one-alternate-low-inc.rds.rds")


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
  geom_vline(data = data_summary, aes(xintercept = p_value),colour = "red")

