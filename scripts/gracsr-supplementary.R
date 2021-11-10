# scripts to see how the time series for the four designs look like

##----load-lib
library(hakear)
library(tidyr)
library(distributional)
library(ggplot2)
library(patchwork)
library(purrr)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(gravitas)
library(magrittr)
## ----designs

sim_null_normal <- function(nxj, nfacetj, mean, sd, w1 = 0, w2=0) {
  rep(distributional::dist_normal(mu = mean, sigma = sd),
      times = nxj * nfacetj
  )
}

sim_varf_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w1),
                  (sd + seq(0, nfacet - 1, by = 1) * w2)), each = nx)
}

sim_varx_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w1),
                  (sd + seq(0, nx - 1, by = 1) * w2)), nfacet)
}

sim_varall_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  dist_normal((mean + seq(0, (nx *nfacet - 1), by = 1) * w1),
              (sd + seq(0,  (nx *  nfacet - 1), by = 1) * w2))
}


## ----data


sim_panel_varall <- sim_panel(
  nx = nx_val, nfacet =  nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varall_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varall_normal(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
) %>% unnest(data)

sim_panel_varx <- sim_panel(
  nx = nx_val, nfacet =  nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varx_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varx_normal(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
) %>% unnest(data)

sim_panel_varf <- sim_panel(
  nx = nx_val, nfacet =  nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varf_normal(2, 3, 5, 10, 5, 5)
  sim_dist = sim_varf_normal(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
) %>% unnest(data)

sim_panel_null <- sim_panel(
  nx = nx_val,
  nfacet =  nfacet_val,
  ntimes = ntimes_val,
  sim_dist = distributional
  ::dist_normal(mean_val, sd_val)
) %>% unnest(c(data))


## ----category-plots

p_null <- sim_panel_null %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~id_facet) +
  geom_boxplot() +
  ylab("") +
  #ggtitle(paste("(a)", round(null, 2))) +
  xlab("x level") +
  theme_bw()

p_varf <- sim_panel_varf %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~id_facet) +
  geom_boxplot() +
  ylab("") +
  #ggtitle(paste("(b)", round(varf, 2))) +
  xlab("x level")+
  theme_bw()

p_varx <- sim_panel_varx %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~id_facet) +
  geom_boxplot()+
  ylab("")  +
  #ggtitle(paste("(c)", round(varx, 2))) +
  xlab("x level")+
  theme_bw()

p_varall <- sim_panel_varall %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~id_facet) +
  geom_boxplot() +
  ylab("") +
  #ggtitle(paste("(d)", round(varall, 2))) +
  xlab("x level")+
  theme_bw()


## ----line-graph


change_index <- function(data){
  
  index_new <- map(seq_len(ntimes_val), function(i){
    map((seq_len(nx_val*nfacet_val)), function(j)
    {
      value = i + (j-1)*ntimes_val
    })
  }) %>% unlist()
  
  data_new = data %>%
    ungroup() %>%
    mutate(index_old = row_number(),
           index_new = index_new)
  
  y = data_new[match(index_new, data_new$index_old),]
  
  y <- y %>%
    mutate(time = row_number())
  
  return(y)
}

endbreaks<- nrow(sim_panel_null)

p1 <- change_index(sim_panel_null) %>%
  ggplot(aes(x = time,
             y = sim_data)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, nfacet_val*nx_val))+
  theme_bw() +
  #geom_point(alpha = 0.5, color = "blue")
  #theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #    panel.grid.minor.x =  element_blank())+
  ylab("simulated response")+
  xlab("index")


p2 <- change_index(sim_panel_varf) %>%
  ggplot(aes(x = time,
             y = sim_data)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, nfacet_val*nx_val))+
  theme_bw() +
  #geom_point(alpha = 0.5, color = "blue") +
  #theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #   panel.grid.minor.x =  element_blank())+
  ylab("simulated response")+
  xlab("index")

p3 <- change_index(sim_panel_varx) %>%
  ggplot(aes(x = time,
             y = sim_data)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, nfacet_val*nx_val))+
  theme_bw() +
  #geom_point(alpha = 0.5, color = "blue")+
  #theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #     panel.grid.minor.x =  element_blank())+
  ylab("simulated response")+
  xlab("index")


p4 <- change_index(sim_panel_varall) %>%
  ggplot(aes(x = time,
             y = sim_data)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, nfacet_val*nx_val))+
  theme_bw() +
  #geom_point(alpha = 0.5, color = "blue") +
  theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
        panel.grid.minor.x =  element_blank())+
  ylab("simulated response")+
  xlab("index")






# p1 <- change_index(sim_panel_varx) %>%
#   filter(id_facet==1) %>%
#   ggplot(aes(x = index_new,
#              y = sim_data)) +
#   geom_line() +
#   geom_point(alpha = 0.5, color = "blue")
#
#
# p2 <- change_index(sim_panel_varx) %>%
#   filter(id_facet==2) %>%
#   ggplot(aes(x = index_new,
#              y = sim_data)) +
#   geom_line()  +
#   geom_point(alpha = 0.5, color = "blue")
#
# p4 <- change_index(sim_panel_varx) %>%
#   filter(id_facet==3) %>%
#   ggplot(aes(x = index_new,
#              y = sim_data)) +
#   geom_line()  +
#   geom_point(alpha = 0.5, color = "blue")
#
#
# p3 <- change_index(sim_panel_varx) %>%
#   ggplot(aes(x = index_new,
#              y = sim_data)) +
#   geom_line() +
#   scale_x_continuous(breaks = seq(1, 300, nfacet_val*nx_val))+
#   theme_bw() +
#   theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
#         panel.grid.minor.x =  element_blank()) +
#   geom_point(alpha = 0.5, color = "blue")
#
#
# p1/p2/p4/p3

# plot

##---plot-together

(p1 + p_null)/( p2 + p_varf)/(p3 + p_varx)/(p4 + p_varall)

##----JS

JS <- function(prob, q, p) {
  # Compute approximate densities
  x <- seq(min(q, p), max(q, p), l = 201)
  qpmf <- pmf(x, prob, q)
  ppmf <- pmf(x, prob, p)
  m <- 0.5 * (ppmf + qpmf)
  JS <- suppressWarnings(0.5 * (sum(stats::na.omit(ppmf * log(ppmf / m, base = 2))) +
                                  sum(stats::na.omit(qpmf * log(qpmf / m, base = 2)))))
  return(JS)
}

# Compute approximate discretized density (like a probability mass function)
# at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q) {
  qcdf <- stats::approx(q, p, xout = x, yleft = 0, yright = 1, ties = max, na.rm = TRUE)$y
  qpmf <- c(0, diff(qcdf) / (x[2] - x[1]))
  return(qpmf / sum(qpmf))
}

