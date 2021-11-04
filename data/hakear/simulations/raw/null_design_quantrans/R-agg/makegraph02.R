library(here)
library(readr)
library(tidyverse)
# run for  norm MMPD files aggregation
#all_data <- read_rds("simulations/result_report/all_data_ norm.rds")
# run for  norm max pairwise distances files aggregation


makegraph02 <- function(folder_name){
  
all_data <- read_rds(paste0("simulations/raw/null_design_quantrans/data-agg/all_data_wpd_", folder_name, ".rds"))
  
summary_data <- all_data %>% 
  group_by(nx, nfacet) %>% 
  summarise(mean = mean(value))

nxbyfacet_density <- all_data %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "#999999") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("raw wpd") +
  geom_vline(data = summary_data, aes(xintercept  = mean), color = "#0072B2") +
  geom_rug(sides = "b", colour = "#D55E00") + 
 scale_x_continuous(breaks = scales::breaks_extended(3.5)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) 

ggsave(nxbyfacet_density, filename = paste0("simulations/raw/null_design_quantrans/figs/", "nxbyfacet_density_wpd_", folder_name,".png"))


nxbyfacet_ridge <- all_data %>% 
  ggplot(aes(x = value, y = as.factor(nx))) +
  ggridges::geom_density_ridges() +
  facet_wrap(~nfacet, labeller = "label_both", nrow = 2) + 
  xlab("raw wpd") +
  ylab("nx") +
  scale_x_continuous(breaks = scales::breaks_extended(4)) +
  theme_bw()

ggsave(nxbyfacet_ridge, filename = paste0("simulations/raw/null_design_quantrans/figs/", "nxbyfacet_ridge_wpd_", folder_name,".png"))


nfacetbynx_ridge <- all_data %>% 
  ggplot(aes(x = value, y = as.factor(nfacet))) +
  ggridges::geom_density_ridges() +
  facet_wrap(~nx, labeller = "label_both", nrow = 2) + 
  xlab("raw wpd") +
  ylab("nfacet") +
  scale_x_continuous(breaks = scales::breaks_extended(4)) +
  theme_bw()


ggsave(nfacetbynx_ridge, filename = paste0("simulations/raw/null_design_quantrans/figs/", "nfacetbynx_ridge_wpd_", folder_name,".png"))
}

makegraph02(folder_name = "wpd_N01")
makegraph02(folder_name = "wpd_N05")
makegraph02(folder_name = "wpd_N51")
makegraph02(folder_name = "wpd_N55")




# N(0,1) and N(5,1) together as they will go in the paper

normal_0_1 <- aggregate01(folder_name = "wpd_N01")
normal_5_1 <- aggregate01(folder_name = "wpd_N51")

all_data <- bind_rows(normal_0_1, normal_5_1, .id = "distribution") %>% mutate(distribution = if_else(distribution == 1, "normal_0_1", "normal_5_1"))

normal_ridge_nxbynfacet <- all_data %>% 
  ggplot(aes(x = value, y = distribution)) +
  ggridges::geom_density_ridges(aes(fill = distribution)) +
  facet_grid(nx~nfacet, labeller = "label_both") + 
  xlab("wpd") +
  scale_fill_manual(values =
                      c("#999999", "#D55E00")) + 
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::breaks_extended(4))

ggsave(normal_ridge_nxbynfacet, filename = paste0("simulations/null_design/figs/normalised", "diff_mean_normal.png"))



# Extra

# N(0,1), N(5,1), N(0,5) together as they will go in the paper

normal_0_5 <- aggregate01(folder_name = "wpd_N05") %>% mutate(distribution = "normal_0_5")

all_data3 <- bind_rows(all_data, normal_0_5)

nxbyfacet <- all_data3 %>% 
  ggplot(aes(x = value, y = distribution)) +
  ggridges::geom_density_ridges(aes(fill = distribution)) +
  facet_grid(nx~nfacet, labeller = "label_both") + 
  xlab("wpd") +
  scale_fill_manual(values = c("#999999", "#D55E00", "#0072B2")) +
  theme(strip.text = 
          element_text(size = 10, margin = margin(b = 0, t = 0)), 
                                  legend.position = "bottom"
  )  +
  scale_x_continuous(breaks = scales::breaks_extended(4))

ggsave(nxbyfacet, filename = paste0("simulations/raw/null_design_quantrans/figs/", "diff_mean3_normal.png"))


## for gamma might go in the paper

makegraph02(folder_name = "wpd_Gamma01")
makegraph02(folder_name = "wpd_Gamma21")

# N(0,1) and N(5,1) together as they will go in the paper

gamma_0.5_1 <- aggregate01(folder_name = "wpd_Gamma01")
gamma_2_1 <- aggregate01(folder_name = "wpd_Gamma21")

all_data <- bind_rows(gamma_0.5_1, gamma_2_1, .id = "distribution") %>% mutate(distribution = if_else(distribution == 1, "gamma_0.5_1", "gamma_2_1"))

gamma_ridge_nxbynfacet <- all_data %>% 
  ggplot(aes(x = value, y = distribution)) +
  ggridges::geom_density_ridges(aes(fill = distribution)) +
  facet_grid(nx~nfacet, labeller = "label_both") + 
  xlab("wpd") +
  scale_fill_manual(values = c("#999999", "#D55E00"))+
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)), legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = scales::breaks_extended(4))

ggsave(gamma_ridge_nxbynfacet, 
filename = paste0("simulations/raw/null_design_quantrans/figs/", 
                  "diff_mean3_gamma.png"))


## How mean and sd changes with nx and nfacet individually

data_N01 <- aggregate01(folder_name = "wpd_N01")

data_x <- data_N01 %>% 
  group_by(nx) %>% 
  summarise(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE)) %>% pivot_longer(-1, names_to = "statistic", 
                                                           values_to = "value") %>% 
  rename("axis" = "nx")

data_facet <- data_N01 %>% 
  group_by(nfacet) %>% 
  summarise(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE)) %>% pivot_longer(-1, names_to = "statistic", 
                                                           values_to = "value")%>% 
  rename("axis" = "nfacet")

data_all <- bind_rows(data_x, data_facet, .id = "axis_name") %>% 
  mutate(axis_name = if_else(axis_name==1, "nx", "nfacet")) %>% 
  rename("level" = "axis_name",
         "nlevel" = "axis")

mean_sd_nx_nfacet <- ggplot(data_all, aes(x = nlevel,
                               y = log(value), colour = level)) +
  facet_wrap(~statistic,
             scales = "free_y")  + ylab("wpd") + geom_line() + 
  scale_colour_manual(values =
                                                                                 c("#0072B2", "#D55E00")) + geom_point() 

ggsave(mean_sd_nx_nfacet, 
       filename = paste0("simulations/raw/null_design_quantrans/figs/", 
                         "mean_sd_nx_nfacet.png"))

