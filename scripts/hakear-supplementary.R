## ---- load
library(knitr)
library(tidyverse)
library(lubridate)
library(lvplot)
#library(ggridges)
library(tsibble)
library(gravitas)
library(ggpubr)
library(readr)
library(kableExtra)
library(distributional)
library(ggplot2)
library(sugrrants)
library(here)
library(ggplot2)
library(patchwork)
library(scales)
library(GGally)
library(viridis)
#remotes::install_github("Sayani07/hakear", force = TRUE)
library(hakear)

##----explain-design
sim_varx_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nx-1, by  = 1)*w), sd), nfacet)
}

sim_varf_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nfacet-1, by  = 1)*w), sd), each = nx)
}
sim_varall_normal = function(nx, nfacet, mean, sd, w)
{
  dist_normal((mean + seq(0, (nx*nfacet - 1), by  = 1)*w), sd)
}

mean = 0
sd = 1
w = 3
nx = 2
nfacet = 3


raw_levels = expand.grid(facet = c("$b_1$", "$b_2$", "$b_3$"),
                         x = c("$a_1$", "$a_2$"))

facet = rep(c("$b_1$", "$b_2$", "$b_3$"), each = nx)
x =  rep(c("$a_1$", "$a_2$"), nfacet)

null <-  sim_varall_normal(nx, nfacet, mean, sd, w=0) %>% tibble()
vary_f <-  sim_varf_normal(nx, nfacet, mean, sd, w) %>% tibble()
vary_x <-  sim_varx_normal(nx, nfacet, mean, sd, w) %>% tibble()
vary_all <-  sim_varall_normal(nx, nfacet, mean, sd, w) %>% tibble()


raw_table <- bind_cols(x = x, 
                       facet = facet, 
                       null = null,
                       vary_f = vary_f, 
                       vary_x = vary_x, 
                       vary_all = vary_all) %>% 
  tibble() 
names(raw_table) = c("x levels", "facet levels", "$D_{null}$",  "$D_{var_f}$", "$D_{var_x}$", "$D_{var_{all}}$" )
raw_table %>% 
  kable(format = "latex",
        booktabs = TRUE, 
        escape = FALSE,
        caption = "Simulation setup for a panel with 3 facet levels and 2 x-axis levels for different designs starting from an initial distribution N(0, 1) for the combination $(a_1, b_1)$ and $\\omega=3$.") %>% kable_styling()
