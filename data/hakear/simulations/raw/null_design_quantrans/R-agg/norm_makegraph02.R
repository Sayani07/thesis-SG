library(here)
library(readr)
library(tidyverse)
# run for  norm MMPD files aggregation
#all_data <- read_rds("simulations/result_report/all_data_ norm.rds")
# run for  norm max pairwise distances files aggregation


makegraph02 <- function(folder_name){
all_data <- read_rds(paste0("simulations/null_design/data/norm_all_data_", folder_name, ".rds"))
  

nxbyfacet <- all_data %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab(" norm wpd")

ggsave(nxbyfacet, filename = paste0("simulations/null_design/figs/normalised/", "density_nx_by_nfacet_", folder_name,".png"))


 norm_nxbyfacet <- all_data %>% 
  ggplot(aes(x = value, y = as.factor(nx))) +
  ggridges::geom_density_ridges() +
  facet_wrap(~nfacet, labeller = "label_both", nrow = 2) + 
  xlab(" norm mmpd") +
  ylab("nx")

ggsave( norm_nxbyfacet, filename = paste0("simulations/null_design/figs/normalised/", "ridge_by_nfacet_", folder_name,".png"))


 norm_nfacetbynx <- all_data %>% 
  ggplot(aes(x = value, y = as.factor(nfacet))) +
  ggridges::geom_density_ridges() +
  facet_wrap(~nx, labeller = "label_both", nrow = 2) + 
  xlab(" norm mmpd") +
  ylab("nfacet")

ggsave( norm_nxbyfacet, filename = paste0("simulations/null_design/figs/normalised/", "ridge_by_nx_", folder_name,".png"))
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
  xlab(" norm wpd") +
  scale_fill_manual(values =
                      c("#999999", "#D55E00")) + 
  theme(legend.position = "bottom")

ggsave(normal_ridge_nxbynfacet, filename = paste0("simulations/null_design/figs/normalised", "normal_ridge_nxbynfacet.png"))



# Extra

# N(0,1), N(5,1), N(0,5) together as they will go in the paper

normal_0_5 <- aggregate01(folder_name = "wpd_N05") %>% mutate(distribution = "normal_0_5")

all_data3 <- bind_rows(all_data, normal_0_5)

all_data3 %>% 
  ggplot(aes(x = value, y = distribution)) +
  ggridges::geom_density_ridges(aes(fill = distribution)) +
  facet_grid(nx~nfacet, labeller = "label_both") + 
  xlab(" norm wpd") +
  scale_fill_manual(values = c("#999999", "#D55E00", "#0072B2"))

ggsave( norm_nxbyfacet, filename = paste0("simulations/null_design/figs/normalised", "normal3_ridge_nxbynfacet_", folder_name,".png"))


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
  xlab(" norm wpd") +
  scale_fill_manual(values = c("#999999", "#D55E00"))+
  theme(legend.position = "bottom")

ggsave(gamma_ridge_nxbynfacet, filename = paste0("simulations/null_design/figs/normalised/", "gamma_ridge_nxbynfacet.png"))
