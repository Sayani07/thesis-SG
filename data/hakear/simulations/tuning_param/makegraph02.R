library(here)
library(readr)
library(tidyverse)
library(latex2exp)
# run for raw MMPD files aggregation
#all_data <- read_rds("simulations/result_report/all_data_raw.rds")
# run for raw max pairwise distances files aggregation
all_data <- read_rds(here::here("simulations/tuning_param/all_data.rds") )%>% 
  mutate(design = case_when(
    design == "vary_x" ~ "var_x",
    design == "vary_facet" ~ "var_f"
  ))

  
# for omega 8

all_data_filtered <- all_data %>% 
  filter(nx<=7, nfacet<=7)

nxbyfacet8 <- all_data_filtered %>% 
  filter(omega == 8) %>% 
  ggplot(aes(x = lambda, y = wpd)) +
  geom_line(aes(colour = design), size = 1) +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  theme(legend.position = "none")  +
  scale_color_manual(values = c("#E69F00",
                                "#56B4E9")) +
  theme(legend.position = "bottom") +
  ggtitle("(b)") + theme_bw() + xlab(TeX("$\\lambda$")) +
  ylab("raw wpd")


# ggsave(nxbyfacet, filename = here("simulations/tuning_param/figs/", "nxbyfacet_omega8.png"))

# for omega 1
nxbyfacet1 <- all_data_filtered %>% 
  filter(omega == 1) %>% 
  ggplot(aes(x = lambda, y = wpd)) +
  geom_line(aes(colour = design), size = 1) +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  theme(legend.position = "none")  +
  scale_color_manual(values = c("#E69F00", 
                                "#56B4E9")) +
  theme(legend.position = "bottom")  +
  ggtitle("(a)") + theme_bw() +  xlab(TeX("$\\lambda$"))  +
  ylab("raw wpd")
# ggsave(nxbyfacet, filename = here("simulations/tuning_param/figs/", "nxbyfacet_omega1.png"))

# 
# get_legend<-function(myggplot){
#   tmp <- ggplot_gtable(ggplot_build(myggplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)
# }
# 
# legend_here <- get_legend(nxbyfacet1)
# 
# library(gridExtra)
# gridExtra::grid.arrange(
#   arrangeGrob(nxbyfacet1, top = c("omega:1")),
#   arrangeGrob(nxbyfacet8, top = c("omega:8")),
#   nrow = 2, legend_here) + 
#   #font.label = list(face = "plain", size = 10)) + 
#   theme(legend.position = "bottom") +
#   xlab("raw wpd") 

# fixed_omega <- ggpubr::ggarrange(
#   nxbyfacet1, nxbyfacet8, nrow = 2, common.legend = TRUE) +
#   theme(legend.position = "bottom") +
#   xlab("raw wpd") + xlab("tuning parameter")


fixed_omega <- nxbyfacet1 / nxbyfacet8 

ggsave(fixed_omega, filename = here("simulations/tuning_param/figs/", "fixed_omega.png")) 



# graph of intersection

intersection_data <- all_data %>% 
  pivot_wider(id_cols = -c(design), names_from = design, values_from = wpd) %>% 
  group_by(nx, nfacet, omega) %>% 
  mutate(intersection_point = abs(var_x - var_f)) %>% filter(intersection_point == min(intersection_point)) %>% 
  mutate(factor = as.factor(omega))

intersection_plot <- ggplot(intersection_data %>% ungroup() %>% filter(nx<=14), aes(x = omega, y = lambda)) +
  geom_line() +
  geom_point() + 
  facet_grid(nx~nfacet, labeller = "label_both") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 10, 3)) +
  theme_bw() + xlab(TeX("$\\omega$")) + ylab(TeX("$\\lambda$")) 
  

ggsave(intersection_plot, filename = here("simulations/tuning_param/figs/", "intersection_plot.png"))


