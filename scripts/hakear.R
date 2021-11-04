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

#library(tidyquant)

## ---- calendar-elec
elec <- read_rds(here("data/hakear/elec.rds")) %>%
  filter(date >= ymd("20180101"), date < ymd("20180701"))
rdbl <- c("Weekday" = "#d7191c", "Weekend" = "#2c7bb6")

elec <- elec %>%
  mutate(
    wday = wday(date, label = TRUE, week_start = 1),
    weekday = if_else(wday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  )
# p_cal_elec <- elec %>%
#   filter(id %in% c(2, 4)) %>%
#   frame_calendar(x = time, y = kwh, date = date, nrow = 1) %>%
#   ggplot(aes(x = .time, y = .kwh, group = date)) +
#   geom_line(aes(colour = as.factor(id)), size = 0.5) +
#   scale_colour_brewer(name = "", palette = "Dark2", direction = 1) +
#   facet_grid(id ~ ., labeller = label_both) +
#   theme(legend.position = "bottom")
# prettify(p_cal_elec, size = 2.5, label.padding = unit(0.1, "lines"))
#
# simtable_new <- simtable %>% filter(nfacet==50 & nx == 20 | nfacet==50 & nx == 31 | nfacet==50 & nx == 50 | nfacet==31 & nx == 50)


## ---- intro_all
id2_tsibble <- elec %>%
  filter(id == 2) %>%
  as_tsibble(index = date_time)

id4_tsibble <- elec %>%
  filter(id == 4) %>%
  as_tsibble(index = date_time)

# hour-of-day and month-of-year (important pair) id2's behavior across different hours of the day very different across months, but for id4 behavior across different hours of the day is not likely a function of month.
p1 <- id2_tsibble %>%
  prob_plot("month_year",
            "hour_day",
            response = "kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9)) +
  ggtitle("(a)") + theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  scale_colour_brewer(name = "", palette = "PiYG") +
  ylab ("energy consumption (in kwh)")


p2 <- id4_tsibble %>%
  prob_plot("month_year",
            "hour_day",
            response = "kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9)) +
  ggtitle("(b)") + theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))+
  ylab ("energy consumption (in kwh)")



p3 <- id2_tsibble %>%
  create_gran("week_month") %>%
  filter(week_month != 5) %>%
  prob_plot("wknd_wday",
            "week_month",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75)) +
  ggtitle("") +

  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))

p4 <- id4_tsibble %>%
  create_gran("week_month") %>%
  filter(week_month != 5) %>%
  prob_plot("wknd_wday",
            "week_month",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75)) +
  ggtitle("b") +  #+ scale_x_discrete(breaks = seq(0, 23, 4)) +
  theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))

## ---- id2-new2
ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE)

## ---- intro-all-wpd

sm <- id2_tsibble

gran_x <- "hour_day"
gran_facet <- "month_year"
v1_id2 <- hakear::compute_pairwise_norm_scalar(sm, gran_x, gran_facet,
  response = kwh, lambda = 0.67
)

gran_x <- "month_year"
gran_facet <- "hour_day"
v2_id2 <- hakear::compute_pairwise_norm_scalar(sm, gran_x, gran_facet,
                                          response = kwh, lambda = 0.67
)


sm <- id4_tsibble

gran_x <- "hour_day"
gran_facet <- "month_year"
v1_id4 <- hakear::compute_pairwise_norm_scalar(sm, gran_x, gran_facet,
                                               response = kwh, lambda = 0.67
)

gran_x <- "month_year"
gran_facet <- "hour_day"
v2_id4 <- hakear::compute_pairwise_norm_scalar(sm, gran_x, gran_facet,
                                               response = kwh, lambda = 0.67
)



## ----onegran-new
id2_tsibble_hd <- elec %>%
  filter(id == 2) %>%
  as_tsibble(index = date_time) %>%
  create_gran("hour_day") %>%
  ggplot(aes(x = hour_day, y = kwh)) +
  geom_boxplot(width = 0.6, outlier.colour = "black", outlier.alpha = 0.5, fill = "#CC79A7", colour =  "#0072B2") + scale_y_log10() +
  geom_jitter(alpha = 0.04, colour = "#E69F00") +
  ggtitle("(a)")+
  ylab ("energy consumption (in kwh)")



id2_tsibble_dw <- elec %>%
  filter(id == 2) %>%
  as_tsibble(index = date_time) %>%
  create_gran("month_year") %>%
  ggplot(aes(x = month_year, y = kwh)) +
  geom_boxplot(width = 0.6, outlier.colour = "black", outlier.alpha = 0.5, fill = "#CC79A7", colour =  "#0072B2") + scale_y_log10()+
  geom_jitter(alpha = 0.04, colour = "#E69F00") +
  ggtitle("(b)")+
  ylab ("energy consumption (in kwh)")



# id2_tsibble_2gran <- id2_tsibble %>%
#   prob_plot("month_year",
#             "hour_day",
#             response = "kwh",
#             plot_type = "quantile",
#             symmetric = TRUE,
#             quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9),palette = "Blues") + ggtitle("")

ggpubr::ggarrange(id2_tsibble_hd, id2_tsibble_dw, ncol = 1,
                  common.legend =  TRUE)

## ----one-gran-wpd
sm <- elec %>%
  filter(id == 2) %>%
  as_tsibble(index = date_time)
gran_x <- "month_year"
gran_facet <- NA
v <- hakear::compute_pairwise_norm_scalar(sm, gran_x, gran_facet,
                                  response = kwh, lambda = 0.9, dist_ordered = FALSE)


gran_x <- "hour_day"
gran_facet <- NA
v <- hakear::compute_pairwise_norm_scalar(sm, gran_x, gran_facet,
                                          response = kwh, lambda = 0.9, dist_ordered = FALSE)


## ---- null4by2

set.seed(9999)

sim_varall_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  dist_normal((mean + seq(0,
                          (nx *
                             nfacet - 1),
                          by = 1
  ) * w1), (sd + seq(0,
                      (nx *
                         nfacet - 1),
                      by = 1
  ) * w2))
}


sim_panel_varall <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  #sim_dist = sim_varall_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varall_normal(2, 3, 0, 1, 3, 0)
) %>% unnest(data)


sim_panel_varall_sd <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  #sim_dist = sim_varall_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varall_normal(2, 3, 0, 1, 0, 0.5)
) %>% unnest(data)



sim_varx_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w1), (sd + seq(0, nx - 1, by = 1) * w2)), nfacet)
}

sim_panel_varx <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  # sim_dist = sim_varx_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varx_normal(2, 3, 0, 1, 3, 0)
) %>% unnest(data)



sim_varf_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w1), (sd + seq(0, nfacet - 1, by = 1) * w2)), each = nx)
}

sim_panel_varf <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  # sim_dist = sim_varf_normal(2, 3, 5, 10, 5, 5)
  sim_dist = sim_varf_normal(2, 3, 0, 1, 3, 0)
) %>% unnest(data)


sim_panel_null <- sim_panel(
  nx = 2,
  nfacet = 3,
  ntimes = 500,
  sim_dist = distributional
  ::dist_normal(0,1)
) %>% unnest(c(data))


sim_panel_null_sd <- sim_panel(
  nx = 2,
  nfacet = 3,
  ntimes = 500,
  sim_dist = distributional
  ::dist_normal(0,3)
) %>% unnest(c(data))

#
# sim_normal_skew <- function(n, tau, omega, alpha) {
#   rsn(n=n, tau=tau, omega = omega, alpha=alpha)
# }

# sim_panel_null_skew <- sim_panel(
#   nx = 2,
#   nfacet = 3,
#   ntimes = 500,
#   sim_dist = sim_normal_skew(n=10000, tau=0, omega = 1, alpha=10)
# ) %>% unnest(c(data))

set.seed(9999)



# p_null <- sim_panel_null %>%
#   rename("facet level" = "id_facet" ) %>%
#   ggplot(aes(x = as.factor(id_x), y = sim_data)) +
#   facet_wrap(~`facet level`,labeller = "label_both") +
#   geom_boxplot() +
#   ggtitle("") +
#   xlab("x level") +
#   ylab("simulated response")



set.seed(9999)
varall <- compute_pairwise_norm(sim_panel_varall,
                                gran_x = "id_x",
                                gran_facet = "id_facet",
                                response = sim_data,
                                nperm = 200
)

set.seed(9999)
varall_sd <- compute_pairwise_norm(sim_panel_varall_sd,
                                   gran_x = "id_x",
                                   gran_facet = "id_facet",
                                   response = sim_data,
                                   nperm = 200
)

# plot
p_varall <- sim_panel_varall %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot() +
  ggtitle(paste("(d)", round(varall, 2))) + xlab("x level") +
  ylab ("simulated response")


                  set.seed(9999)

null <- compute_pairwise_norm(sim_panel_null,
                              gran_x = "id_x",
                              gran_facet = "id_facet",
                              response = sim_data,
                              nperm = 200
)


null_sd <- compute_pairwise_norm(sim_panel_null_sd,
                              gran_x = "id_x",
                              gran_facet = "id_facet",
                              response = sim_data,
                              nperm = 200
)



# null_skew <- compute_pairwise_norm(sim_panel_null_skew,
#                                  gran_x = "id_x",
#                                  gran_facet = "id_facet",
#                                  response = sim_data,
#                                  nperm = 200
# )
#
#

p_null <- sim_panel_null %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot() +
  ggtitle(paste("(a)", round(null, 2))) + xlab("x level") +
  ylab ("simulated response")




set.seed(9999)
varf <- compute_pairwise_norm(sim_panel_varf,
                              gran_x = "id_x",
                              gran_facet = "id_facet",
                              response = sim_data,
                              nperm = 200
)

p_varf <- sim_panel_varf %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot() +
  ggtitle(paste("(b)", round(varf, 2))) + xlab("x level") +
  ylab ("simulated response")


set.seed(9999)
varx <- compute_pairwise_norm(sim_panel_varx,
                              gran_x = "id_x",
                              gran_facet = "id_facet",
                              response = sim_data,
                              nperm = 200
)

# plot
p_varx <- sim_panel_varx %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot() +
  ggtitle(paste("(c)", round(varx, 2))) + xlab("x level") +
  ylab ("simulated response")



(p_null + p_varf)/(p_varx + p_varall)

#
# ggpubr::ggarrange(p_null, p_varf,  p_varx, p_varall, nrow = 2, ncol = 2,
#                   common.legend = TRUE,
#                   labels = c("a", "b", "c", "d"))





## ---- notations
##
d1 <- tibble (variable = c("$N_C$", "$H_{N_C}$", "nx", "nfacet" , "$\\lambda$", "$\\omega$", "$wpd$", "$wpd_{norm}$","$nperm$", "$nsim$"),  description = c("number of cyclic granularities", "set of harmonies", "number of x-axis categories", "number of facet categories", "tuning parameter", "increment (mean or sd)", "raw weighted pairwise distance", "normalized weighted pairwise distance", "number of permutations for threshold/normalization", "number of simulations"))


d2 <- tibble(variable =  c(
                           "$wpd_{threshold}$",
                           "$D_{null}$",
                           "$D_{var_f}$",
                           "$D_{var_x}$",
                           "$D_{var_{all}}$"),
             description = c(
                             "threshold for significance",
                             "null design with no distributional difference across categories",
                             "design with distributional difference only across facets categories",
                             "design with distributional difference only across x-axis categories",
                             "design with distributional difference across both facet and x-axis"))


d <- bind_rows(d1, d2)
knitr::kable(d, format = "markdown",
             escape = FALSE,
             caption =
               "Nomenclature table")

##----distance-explain
knitr::include_graphics(here::here("img/dist_explain.png"))


##----raw
G21 <- read_rds("data/hakear/simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds")

summary_data <- G21 %>%
  group_by(nx, nfacet) %>%
  summarise(mean = mean(value))

G21 %>%
  ggplot(aes(x = value)) +
  geom_density(fill = "#999999") +
  facet_grid(nx~nfacet,
             labeller = "label_both") +
  xlab("raw values of wpd") +
  geom_vline(data = summary_data, aes(xintercept  = mean), color = "#0072B2") +
  geom_rug(sides = "b", colour = "#D55E00") +
  # scale_x_continuous(breaks = scales::breaks_extended(2)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

##----raw-onegran
G21 <- read_rds("data/hakear/simulations/supplementary/one-gran/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds")

summary_data <- G21 %>%
  group_by(nx, nfacet) %>%
  summarise(mean = mean(value))

G21 %>%
  ggplot(aes(x = value)) +
  geom_density(fill = "#999999") +
  facet_wrap(~nx, ncol = 1,
             labeller = "label_both", strip.position = "right") +
  xlab("raw values of wpd") +
  geom_vline(data = summary_data, aes(xintercept  = mean), color = "#0072B2") +
  geom_rug(sides = "b", colour = "#D55E00") +
  # scale_x_continuous(breaks = scales::breaks_extended(2)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))





##----quadratic
G21 %>%
  ggplot(aes(x=nx*nfacet, y = value)) +
  geom_point(alpha = 0.5, size = 0.5) + stat_summary(fun=median, geom="line", aes(group=1), color = "blue") + xlab("nx*nfacet") + ylab("wpd")

## ---- norm
G21_norm <- read_rds(here("data/hakear/simulations/norm/null_design_quantrans_nperm/data-agg/all_data_wpd_Gamma21.rds"))

G21_norm %>%
  ggplot(aes(x = value)) +
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") +
  scale_x_continuous(breaks = scales::breaks_extended(3)) +
  xlab("wpd normalised using permutation approach")


## ---- linear-model
G21 <- read_rds(here("data/hakear/simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds"))



G21_median <- G21 %>%
  group_by(nx*nfacet) %>%
  summarise(actual = median(value))


# fit model median to log(nx*nfacet)
fit_lm2 <- lm(actual ~ log(`nx * nfacet`) , data = G21_median)

broom::tidy(fit_lm2) %>% kable(caption = "Results of linear model to capture the relationship between wpd and number of comparisons.")

## ---- resi-linear
intercept <- fit_lm2$coefficients[1]
slope <- fit_lm2$coefficients[2]

G21 %>%
  ggplot(aes(x=log(nx*nfacet), y = (value - slope*log(nx*nfacet)))) +
  geom_point(alpha = 0.5, size = 0.5) + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") +
  ylab("wpd normalised using linear modelling approach")

## ---- glm-tab

G21 <- read_rds(here("data/hakear/simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds"))
G21_median <- G21 %>%
  group_by(nx*nfacet) %>%
  summarise(actual = median(value))
glm_fit <- glm(actual ~ log(`nx * nfacet`),
               family = Gamma(link = "inverse"),
               data = G21_median)
intercept <- glm_fit$coefficients[1]
slope <- glm_fit$coefficients[2]
G21_sd  = G21 %>%
  mutate(wpd_glm =  (value - (1/(intercept + slope*log(nx*nfacet)
  )
  )
  ))
#scale_fac <- 1/G21_sd$wpd_glm %>% sd()
# checking the fit of the residuals from glm fit
# fitted_glm <- fitted(glm_fit, type = "response")
# residuals <- residuals.glm(glm_fit, type = "response")
# hist(residuals)
#h = augment(glm_fit)
# ggplot(h) +
#   geom_histogram(aes(x = .resid))
# residual <- G21_median$actual  - fitted_glm
#
# hist(residual)
# G21 %>%
#   ggplot(aes(x=log(nx*nfacet),
#              y = (value - (1/(intercept + slope*log(nx*nfacet)
#              )
#              )
#              )
#   )
#   ) +
#   geom_point() + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") +
#   ylab("wpd_glm = wpd_raw - 1/(a  + b*log(nx*nfacet))")


G21_onegran <- read_rds(here("data/hakear/simulations/supplementary/one-gran/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds"))

G21_median_onegran  <- G21_onegran  %>%
  group_by(nx*nfacet) %>%
  summarise(actual = median(value))


glm_fit_onegran  <- glm(actual ~ log(`nx * nfacet`),
               family = Gamma(link = "inverse"),
               data = G21_median_onegran)

bind_rows(broom::tidy(glm_fit_onegran),
          broom::tidy(glm_fit), .id = "m") %>%
  mutate(estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         statistic = round(statistic, 2),
         p.value = round(p.value, 2)
         ) %>% kable(caption = "Results of generalised linear model to capture the relationship between $wpd_{raw}$ and number of comparisons.")


## ---- wpd-glm-dist

G21_glm <- G21 %>%
  mutate(wpd_glm =  (value - (1/(intercept + slope*log(nx*nfacet)
  )
  )
  ),
  wpd_glm_scaled = ((wpd_glm*320)))

#G21_glm$wpd_glm_scaled %>% sd()
#
# G21_glm %>%
#   ggplot() +
#   geom_density(aes(x = wpd_glm),
#                fill = "blue") +
#   facet_grid(nx~nfacet,
#              labeller = "label_both") +
#   theme(legend.position = "bottom")


##----hist-qq-new
hist <- ggplot(G21_glm) +
  geom_histogram(aes(x = wpd_glm_scaled)) +
  ggtitle("a") + xlab("transformed wpd using glm approach")


p <- ggplot(G21_glm, aes(sample = wpd_glm_scaled))
qqplot <-  p + geom_qq() + geom_qq_line() + coord_fixed(ratio = 4/5) +
  ggtitle("b")

# G21_glm <- G21 %>%
#     mutate(wpd_glm =  (value - (1/(intercept + slope*log(nx*nfacet)
#     )
#     )
#     ),
#     wpd_glm_scaled = 300*(wpd_glm))

# G21_glm$wpd_glm_scaled %>%
#   range()
library(patchwork)
hist + qqplot

##----dist-new-same-scale-link
G21_permutation <- read_rds(here("data/hakear/simulations/norm/null_design_quantrans_nperm/data-agg/all_data_wpd_N01.rds")) %>%
  rename("wpd_permutation" = "value")


# G21_model_data <- G21 %>%
#   mutate(model =
#            ((1/value)
#                   - intercept -
#                     slope*log(nx*nfacet))/slope) %>%
#   mutate(model_trans =
#            (model - mean(model))/sd(model))

# G21_model_data$model %>% summary()

G21_all_data <- G21_permutation %>%
  # left_join(G21_lm, by = c("nx", "nfacet", "perm_id")) %>%
  left_join(G21_glm, by = c("nx", "nfacet", "perm_id")) %>%
  pivot_longer(cols = c(3, 7),
               names_to = "type_estimate",
               values_to = "value_estimate")
G21_all_data$type_estimate = factor(G21_all_data$type_estimate , levels = c( "wpd_permutation", "wpd_glm_scaled"))


summary_data <- G21_all_data %>%
  group_by(nx, nfacet, type_estimate) %>%
  summarise(mean = mean(value_estimate)) %>% ungroup()


G21_all_data %>%
  filter(type_estimate %in% c("wpd_glm_scaled", "wpd_permutation")) %>%
  ggplot(aes(x = value_estimate)) +
  geom_density(aes(fill = type_estimate), alpha = 0.5, size = .5) +
  #geom_vline(data = summary_data, aes(xintercept = mean, color = type_estimate)) +
  geom_rug(aes(color = type_estimate), length = unit(0.09,"cm"), alpha = 0.5) +
  #coord_cartesian(clip = "off") +
  facet_grid(nx~nfacet,
             labeller = "label_both") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c( "#D55E00", "#0072B2")) +
  scale_color_manual(values = c( "#D55E00", "#0072B2")) +
  xlab("adjusted values of wpd") +
  scale_x_continuous(breaks = c(-5, -3, 0, 3, 5))
# G21_all_data %>%
#   filter(type_estimate %in% c("wpd_permutation", "wpd_glm")) %>%
#   ggplot() +
#   geom_density(aes(x = value_estimate,
#                    fill = type_estimate),
#                alpha = 0.7) +
#   facet_grid(nx~nfacet,
#              labeller = "label_both") +
#   theme(legend.position = "bottom") +
#   scale_fill_manual(values = c("#CC79A7", "#0072B2")) +
#   xlab("wpd_norm2")

# are scales same
#
# G21_model_data %>%
#   group_by(nx, nfacet) %>%
#   summarize(sd_model = sd(model))
#
# G21_all_data %>%
#   filter(type_estimate == c("perm_trans")) %>%
#   select(value_estimate) %>%
#   range()
#
# G21_all_data %>%
#   filter(type_estimate == c("wpd_glm_scaled")) %>%
#   select(value_estimate) %>%
#   range()


## ---- varall-new

# sim_varall_normal <- function(nx, nfacet, mean, sd, w) {
#   dist_normal((mean + seq(0,
#                           (nx *
#                              nfacet - 1),
#                           by = 1
#   ) * w), sd)
# }
# sim_panel_varall <- sim_panel(
#   nx = 2, nfacet = 3,
#   ntimes = 500,
#   sim_dist = sim_varall_normal(2, 3, 0, 1, 2)
# ) %>% unnest(data)



## ---- varx-new

# sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
#   rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
# }
#
# sim_panel_varx <- sim_panel(
#   nx = 2, nfacet = 3,
#   ntimes = 500,
#   sim_dist = sim_varx_normal(2, 3, 0, 1, 2)
# ) %>% unnest(data)



##----varf-new

# sim_varf_normal <- function(nx, nfacet, mean, sd, w) {
#   rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w), sd), each = nx)
# }
# sim_panel_varf <- sim_panel(
#   nx = 2, nfacet = 3,
#   ntimes = 500,
#   sim_dist = sim_varf_normal(2, 3, 0, 1, 2)
# ) %>% unnest(data)



##----null-new

# sim_panel_null <- sim_panel(
#   nx = 2,
#   nfacet = 3,
#   ntimes = 500,
#   sim_dist = distributional
#   ::dist_normal(0, 1)
# ) %>% unnest(c(data))



## ---- plot-all-new3

# {r raw-nqt-8, fig.height = 5, fig.cap="The raw density of the half-hourly demand for the eight households is shown in Fig a. Fig b shows the normal-score-transformed half-hourly demand for the same households which has resulted in more symmetric distribution of half-hourly demand.", eval = FALSE}
elec_raw <- elec %>%
  ggplot() +
  geom_density(aes(x = kwh)) +
  facet_wrap(~id, scales = "free_y", ncol = 1) +
  xlab("demand (in kwh)") + ggtitle("a") +
  theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))

# elec_nqt <- elec %>%
#   dplyr::mutate(kwh_transformed = stats::qqnorm(kwh, plot.it = FALSE)$x) %>%
#   ggplot() +
#   geom_density(aes(x = kwh_transformed))+
#   facet_wrap(~id, scales = "free_y", ncol = 1) +
#   xlab("NQT demand (in kwh)") + ggtitle("b") + theme(
#     strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))

# hour_day_raw <- elec %>%
#   tsibble::as_tsibble(index = date_time, key = id) %>%
#     create_gran("hour_day") %>%
#     ggplot() +
#     ggridges::geom_density_ridges(aes(y=hour_day, x = kwh))
#
# hour_day_trans <- elec %>%
#   tsibble::as_tsibble(index = date_time, key = id) %>%
#     create_gran("hour_day") %>%
#   dplyr::mutate(kwh_transformed = stats::qqnorm(kwh, plot.it = FALSE)$x) %>%
#   ggplot() +
#   ggridges::geom_density_ridges(aes(y=hour_day, x = kwh_transformed))

# elec_raw + elec_nqt



## ---- linear-scale-8

elec <- read_rds(here("data/hakear/elec_all-8.rds")) %>%
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id==1) %>%
  select(-meter_id) %>%
  rename("id" = "household_id",
         "date_time" = "reading_datetime") %>%
  mutate(date = date(date_time))


elec_linear <- elec %>%
  ggplot() +
  geom_line(aes(x = date_time, y = kwh),alpha = 0.7) +
  facet_wrap(~id, nrow = 8, labeller = "label_both",
             strip.position =  "right",
             scales = "free_y") + ggtitle("a")

elec_zoom <-  elec %>%
  as_tibble() %>%
  filter(date >as.Date("2019-09-01") & date < (as.Date("2019-09-30"))) %>%
  ggplot(aes(x=date_time, y = kwh)) +
  geom_line(size = 0.1, colour = "blue") +
  facet_wrap(~id,
             scales = "free_y",
             ncol = 1,
             strip.position =  "right") +
  gghighlight(date > as.Date("2019-09-15") & date < (as.Date("2019-09-21")), unhighlighted_params = list(colour = "black")) + ggtitle("b")

elec_linear + elec_zoom



## ---- tab-demography
demography <- tibble(id = c(1, 2, 3, 4, 5,6, 7, 8),
                     profession = c("academia", "mixed", "industry", "industry", "mixed", "mixed", "academia", "industry"),
                     total_members = c(2, 3, 5, 5, 2, 2, 2, 6),
                     kids = c("no", "no", "yes", "yes", "no", "no", "no", "yes"),
                     old_parents = c("no", "yes", "yes", "yes", "no", "no", "no", "yes"),
                     PhD_student = c("no", "yes", "no", "no", "yes", "yes", "yes", "no"))

demography <- demography %>% kable(format  = "markdown", caption = "Demographics of the eight households chosen for study")

demography %>% add_footnote(label = "***: 99% significant, **: 95% and *: 90%")

#
# code of elec_harmony_all to follow. Since it takes long to run data/elec_harmony_all.rds is called

## ---- rank-household-mclapply-8
# elec <- read_rds(here("data/hakear/elec_all-8.rds")) %>%
#   dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id==1) %>%
#   select(-meter_id) %>%
#   rename("id" = "household_id",
#          "date_time" = "reading_datetime")
#
#
#
# library(tictoc)
# #tic()
# elec_split = elec %>% group_split(id)
#
# elec_select_harmony = parallel::mclapply(1:8, function(x){
#
#   data_id <-  elec_split %>% magrittr::extract2(x) %>%
#     as_tsibble(index = date_time)
#
#   harmonies <- data_id %>%
#     harmony(
#       ugran = "month",
#       filter_in = "wknd_wday",
#       filter_out = c("hhour", "fortnight")
#     )
#
#   hakear::select_harmonies(data_id,
#                            harmony_tbl = harmonies,
#                            response = kwh,
#                            nperm = 200,
#                            nsamp = 200
#   )
#
# }, mc.cores = parallel::detectCores() - 1, mc.preschedule = FALSE, mc.set.seed = FALSE)
#


## ---- elec_select_harmony-8
#
# elec_harmony_all <- elec_select_harmony %>%
#   bind_rows(.id = "id") %>%
#   mutate(facet_variable = case_when(
#     facet_variable == "hour_day" ~ "hod" ,
#     facet_variable == "day_month" ~ "dom" ,
#     facet_variable == "day_week" ~ "dow" ,
#     facet_variable == "week_month" ~ "wom" ,
#     facet_variable == "wknd_wday" ~ "wdwnd"
#   )) %>%
#   mutate(x_variable = case_when(
#     x_variable == "hour_day" ~ "hod" ,
#     x_variable == "day_month" ~ "dom" ,
#     x_variable == "day_week" ~ "dow" ,
#     x_variable == "week_month" ~ "wom" ,
#     x_variable == "wknd_wday" ~ "wdwnd"
#   )) %>%
#   mutate(id = paste("id", id, sep = " ")) %>%
#   group_by(id) %>%
#   mutate(rank = row_number())

#write_rds(elec_harmony_all, "data/hakear/elec_harmony_all.rds")
#elec_harmony_all <- read_rds("data/hakear/elec_harmony_all.rds")


## ---- dotplot-8

elec_harmony_all <- read_rds("data/hakear/elec_harmony_all.rds")
select_split <- str_split(elec_harmony_all$select_harmony, " ", simplify = TRUE)[,2]

elec_sig_split <- elec_harmony_all %>%
  bind_cols(select_split = select_split) %>%
  mutate(significant = case_when(
    select_split == "***" ~ "highest",
    select_split == "**" ~ "high",
    select_split == "*" ~ "medium",
    select_split == "" ~ "low"
  )) %>%
  mutate(rank = case_when(
    select_split == "***" ~ paste(rank, "***", sep = " "),
    select_split == "**" ~  paste(rank, "**", sep = " "),
    select_split == "*" ~  paste(rank, "*", sep = " "),
    select_split == "" ~  paste(rank, "", sep = " ")
  ))


elec_sig_split$significant <-
  factor(elec_sig_split$significant, levels = c("highest", "high", "medium", "low"))

heatplot <- elec_sig_split %>%
  mutate(significance_95 = if_else(significant %in% c("high", "highest"), "*", "")) %>%
  ggplot(aes(x = x_variable,
             y = facet_variable)) +
  geom_tile(aes(fill = wpd)) +
  #color = as.factor(significance_95)),
  #size = 0.3) +
  geom_text(aes(label = significance_95), color = "#42275a") +
  scale_fill_gradient(low = "white",high = "#ba5370") +
  #scale_fill_manual(palette = "Dark2") +
  #scale_colour_manual(values = c("white","red")) +
  theme(legend.position = "none") +
  coord_fixed() +
  guides(fill = guide_legend()) +
  theme_void() +
  theme_gray(base_size = 12, base_family = "Times") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 60, hjust=1),legend.position = "bottom") + ggtitle("a") +
  theme(
    strip.background = element_blank(),
    strip.text.y  = element_blank(), plot.margin = unit(c(0, -2, 0, 0), "cm")) + ggtitle("(a)") + xlab("x") + ylab("facet")

elec <- read_rds(here("data/hakear/elec_all-8.rds")) %>%
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id==1) %>%
  select(-meter_id) %>%
  rename("id" = "household_id",
         "date_time" = "reading_datetime")


# elec1 <- elec %>%
#   filter(id == "1") %>%
#   create_gran("day_week") %>%
#   create_gran("hour_day") %>%
#   mutate(day_week = as.numeric(day_week),
#          hour_day = as.numeric(hour_day),
#          id = as.numeric(id))


# data_elec_order <- elec %>%
#   as_tibble() %>%
#   group_by(id) %>%
#   summarise(av = mean(kwh)) %>%
#   arrange(av)
#
#
#
# elec_zoom <-  elec %>%
#   as_tibble() %>%
#   left_join(data_elec_order) %>%
#   arrange(av)  %>%
#   dplyr::filter(date(date_time) > as.Date("2019-09-01") & date(date_time) < (as.Date("2019-09-30")))
#
# elec_zoom$id = factor(elec_zoom$id, levels = data_elec_order$id)

elec_zoom <-  elec %>%
    as_tibble() %>%
    dplyr::filter(date(date_time) > as.Date("2019-09-01") & date(date_time) < (as.Date("2019-09-30"))) %>%
  mutate(id = paste("id", id, sep = " ")) %>%
  ggplot(aes(x=date_time, y = kwh)) +
  #geom_point(size = 0.1, colour = "black", alpha = 0.3) +
  geom_line(size = 0.5, aes(color = id), alpha = 1) +
  facet_wrap(~id,
             scales = "free_y",
             ncol = 1,
             strip.position =  "right") +
  xlab("Time [30m]") +
  theme_grey() +
  ylab("Energy demand (in Kwh)") + ggtitle("(b)") +
  theme(panel.grid.major.x = element_blank()) +
  scale_x_datetime("Date", date_labels = "%b %d",
                   breaks = "1 week",
                   date_minor_breaks = "1 day")  + theme_bw() +
  theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
        panel.grid.minor.x =  element_line(colour = "#D3D3D3")) +
  theme(
    strip.text = element_text(size = 10,
                              margin = margin(b = 0, t = 0))) +
  scale_color_viridis(discrete=TRUE)



##parallel-coordinate-plot

data_order <- elec_harmony_all %>%
  mutate(comb = paste(facet_variable, x_variable, sep = "-")) %>%
  group_by(comb) %>%
  summarise(m = mean(wpd)) %>%
  arrange(desc(m))


data_pcp <- elec_harmony_all %>%
  mutate(wpd = as.numeric(wpd)) %>%
  mutate(comb = paste(facet_variable, x_variable, sep = "-")) %>%
  left_join(data_order) %>%
  arrange(desc(m)) %>%
  pivot_wider(-c(2, 3, 4, 5, 7, 8, 9, 10),
              names_from = comb,
              values_from = wpd)

parcoord <- GGally::ggparcoord(data_pcp,
                   columns = 2:ncol(data_pcp),
                   groupColumn = 1,
                   showPoints = FALSE,
                   title = "(c)",
                   alphaLines = 1 ,
                   scale = "globalminmax"
) + ggplot2::theme_bw() +
  scale_color_viridis(discrete=TRUE) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size=10)
  )+
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0)) +
  theme(legend.position = "bottom") +
  coord_flip() +
  xlab("") +
  ylab("wpd")


(heatplot +
    theme(legend.position = "none") +
    facet_grid(id~.) +
  elec_zoom +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  parcoord +
    plot_layout(widths = c(1, 3, 1.5), guides = "collect") &
    theme(legend.position='bottom') +
  theme(panel.spacing = unit(0.2, "lines"))

##----tab-rank-8

elec_rank <- elec_sig_split %>%
  select(-c(6, 7, 9, 10)) %>%
  pivot_wider(
    names_from = id,
    values_from = rank) %>%
  rename("facet variable" = "facet_variable",
         "x variable" = "x_variable") %>%
  select(-facet_levels, -x_levels)

elec_rank %>% kable(format = "markdown", caption = "Ranking of harmonies for the eight households with significance marked for different thresholds. Rankings are different and at most three harmonies are significant for any household. The number of harmonies to explore are reduced from 42 to 3.")




## ---- gravitas-plot-8

# id4_tsibble <- elec %>%
#   filter(id == 4)
#
# id5_tsibble <- elec %>%
#   filter(id == 5)
#
# p1 <- id4_tsibble %>%
#   prob_plot("hour_day",
#             "day_week",
#             response = "kwh",
#             plot_type = "quantile",
#             symmetric = TRUE,
#             quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
#   ggtitle("a) hod vs dow (id:4)") +
#   #scale_colour_brewer(name = "", palette = "Set2") +
#   theme(legend.position = "none",
#         strip.text = element_text(size = 7, margin = margin())) +
#   ylab("energy consumption (in kwh)")
#
#
# p2 <- id5_tsibble %>%
#   prob_plot("hour_day",
#             "day_week",
#             response = "kwh",
#             plot_type = "quantile",
#             symmetric = TRUE,
#             quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
#   ggtitle("a) hod vs dow (id:5)") +
#   #scale_colour_brewer(name = "", palette = "Set2")   +
#   theme(legend.position = "none",
#         strip.text = element_text(size = 7, margin = margin())) +
#   ylab("energy consumption (in kwh)")
#
#
# ggarrange(p1, p2, ncol=2, common.legend = TRUE)

id1_tsibble <- elec %>%
  filter(id == 1)

id7_tsibble <- elec %>%
  filter(id == 7)

p1 <- id1_tsibble %>%
  prob_plot("day_week",
            "hour_day",
            response = "kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9), nrow = 1) +
  ggtitle("a) hod vs dow (id:1)") +
  #scale_colour_brewer(name = "", palette = "Set2")
  ylab("energy consumption (in kwh)") +
  scale_y_log10() + theme_bw()  +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, margin = margin())) +
  theme(panel.spacing =unit(0, "lines"))

p2 <- id7_tsibble %>%
  prob_plot("day_week",
            "hour_day",
            response = "kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9), nrow = 1) +
  ggtitle("b) hod vs dow (id:7)") +
  #scale_colour_brewer(name = "", palette = "Set2")   +
  ylab("energy consumption (in kwh)") +
  scale_y_log10() + theme_bw()  +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, margin = margin()))+
  theme(panel.spacing =unit(0, "lines"))



ggarrange(p1, p2, nrow=2, common.legend = TRUE, widths= c(0.01, 1))
