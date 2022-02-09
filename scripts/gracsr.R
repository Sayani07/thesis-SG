## ---- load-lib

#theme_set(theme_bw())
library(ggplot2)
library(lubridate)
library(gravitas)
library(gracsr)
library(ggdendro)
library(dplyr)
library(readr)
library(visdat)
library(ggplot2)
library(tidyverse)
library(naniar)
library(here)
library(tsibble)
library(knitr)
library(patchwork)
library(GGally)
library(distributional)
library(viridis)
library(forcats)
library(tidyr)
library(purrr)
library(stringr)
library(pals)
remotes::install_github("Sayani07/gracsr")


##----load-data
load("data/ALL_DATA.Rdata")



##----theme-validation
theme_validation <- function() {
  theme_bw() +
    theme(
      strip.text = element_text(size = 8, margin = margin(b = 0, t = 1)),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.title.y = element_blank(),
      #axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )
}


##----theme_characterisation

theme_characterisation <- function() {

  theme_bw() + # seeting theme
    theme(strip.text = element_text(size = 10,
                                    margin = margin(b = 0, t = 0))) + # narrow facet space
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + # no axis ticks
    theme(panel.spacing =unit(0, "lines")) +  # to ensure no gap between facets
    theme(axis.text.x = element_text(angle=90, hjust=1, size = 10)) + # rotate the x-axis text
    theme(legend.position = "bottom")+
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
    theme(axis.text.x = element_text(size=5))
}


##----mytheme-application
theme_application <- function() {

  theme_light() + # setting theme
    #theme(strip.text = element_text(margin = margin(b = 0, t = 0))) + # narrow facet space
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + # no axis ticks
    theme(panel.spacing =unit(0, "lines")) +  # to ensure no gap between facets
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) + # no x-axis labels to further reduce the gap between facets
    #theme(axis.text.x = element_text(angle=90, hjust=1, size = 9)) + # rotate the x-axis text
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
    #theme(axis.text.x = element_text(size=5)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank()) +
    theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
          )
}


##----format-data

smart_meter_data <- EUDMData %>%
  dplyr::rename_all(tolower) %>%
  dplyr::mutate(reading_datetime = lubridate::ymd_hms(reading_datetime)) %>%
  dplyr::arrange(customer_id, reading_datetime) %>%
  dplyr::group_by(customer_id) %>%
  dplyr::mutate(reading_datetime = case_when(
    duplicated(reading_datetime) ~ reading_datetime + lubridate::hours(1),
    TRUE ~ reading_datetime
  ))

elec_ts <- smart_meter_data %>%
  build_tsibble(
    key = customer_id, index = reading_datetime,
    validate = FALSE, ordered = TRUE
  )

readr::write_rds(elec_ts, "data/elec_ts.rds")

## ---- elec-gaps
elec_ts <- read_rds("data/elec_ts.rds")
# 13735 customers in elec_ts
gap_df <- has_gaps(elec_ts)

nogap <- gap_df %>% filter(.gaps==FALSE)

elec_nogap <- elec_ts %>%
  filter(customer_id %in% nogap$customer_id)

readr::write_rds(elec_nogap, "data/elec_nogap.rds")
elec_nogap <- read_rds("data/elec_nogap.rds")
#8685 customers in elec_nogap


# sum(gap_df$.gaps) / NROW(gap_df)

## ---- count-gaps
count_na_df <- elec_ts %>%
  count_gaps()
#5050 customers in count_na_df

lumped_na_df <- count_na_df %>%
  mutate(
    customer_id = as.factor(customer_id) %>%
      fct_lump(264) %>%
      fct_reorder(.n, sum)
  )

p_264 <- lumped_na_df %>%
  filter(customer_id != "Other") %>%
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from), size = 0.6, shape = 4) +
  geom_point(aes(y = .to), size = 0.6, shape = 4) +
  coord_flip() +
  xlab("Top customers with more than
       10% observations missing") +
  ylab("") +  theme(axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0, 0, -1, -1), "line")) + scale_y_datetime(" ",
                   date_labels = "%b %d",
                      breaks = "1 month",
                      date_minor_breaks = "1 week") +
  theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
        panel.grid.minor.x =  element_line(colour = "#D3D3D3"))



p_other <- lumped_na_df %>%
  filter(customer_id == "Other") %>%
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to), alpha = 0.01) +
  geom_point(aes(y = .from), size = 1.2,
             shape = 4, alpha = 0.1) +
  geom_point(aes(y = .to), size = 1.2, shape = 4, alpha = 0.01) +
  coord_flip() +
  xlab("Rest") +
  ylab("Time gaps") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )  + scale_y_datetime("Month-year",
                        date_labels = "%b %y",
                        breaks = "1 month",
                        date_minor_breaks = "1 week") +
  theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
        panel.grid.minor.x =  element_line(colour = "#D3D3D3"))+
  theme(axis.text.x = element_text(angle = 90))

g = p_264 + p_other + patchwork::plot_layout(ncol = 1, heights = c(10, 1))

ggsave("figs/missing-data.png")

##----percentage_na_df

miss_obs_df <- count_na_df %>%
  as_tibble() %>%
  group_by(customer_id) %>%
summarise(n_miss = sum(.n)) %>%
  arrange(desc(n_miss))


total_obs_df <- elec_ts %>%
  filter(customer_id %in% miss_obs_df$customer_id) %>%
  fill_gaps() %>%
  as_tibble() %>%
  group_by(customer_id) %>%
  tally() %>%
  rename("n_total" = "n") %>%
  arrange(desc(n_total))


percent_obs_df <- total_obs_df %>%
  left_join(miss_obs_df, by = "customer_id") %>%
  arrange(desc(n_miss), desc(n_total)) %>%
  mutate(percent_miss = n_miss*100/n_total) %>%
  mutate(percent_miss = case_when(
    percent_miss>50 ~ ">50%",
    percent_miss>30 ~ "30-50%",
    percent_miss>10 ~ "10-30%",
    percent_miss<10 ~ "<10%",
    TRUE~ as.character(percent_miss)
  )) %>%
  group_by(percent_miss) %>%
  tally()


## ----customer-data
#
# customer_data <- write_rds(CustomerData, "data/customer-data.rds")


customer_data <- read_rds("data/customer-data.rds")

customer_sub_data <- customer_data %>%
  filter(CUSTOMER_KEY %in% elec_ts$customer_id)


customer_sub_data %>% mutate(
  LOCAL_GOV_AREA_NAME =  if_else(as.character(LOCAL_GOV_AREA_NAME)=="",as.character(TRIAL_REGION_NAME),as.character(LOCAL_GOV_AREA_NAME))) %>%
  group_by(LOCAL_GOV_AREA_NAME) %>%
  tally() %>% arrange(desc(n))

## ---- elec-raw

sm_50 <- elec %>%
  distinct(customer_id) %>%
  slice(1:50)

elec %>%
  filter(customer_id %in% sm_50$customer_id) %>%
  tibble() %>%
  ggplot(aes(x=reading_datetime,
             y= general_supply_kwh), alpha = 0.5) +
  geom_line() +
  scale_fill_gradient2() +
  facet_wrap(~customer_id, ncol= 2) +
  theme_void() +
  theme(strip.text.x = element_blank())

## ---- elec-raw-all

sm_50 <- elec_ts %>%
  distinct(customer_id)

set.seed(123)
sm_50only <- sample(sm_50$customer_id, 50)

elec %>%
  filter(customer_id %in% sm_50only) %>%
  tibble() %>%
  ggplot(aes(x = reading_datetime,
             y= customer_id), alpha = 0.5)



##----raw-50
set.seed(123)
sm_50 <- elec_ts %>%
  as_tibble() %>%
  distinct(customer_id) %>%
  slice_sample(n = 50)

p1 <- elec_ts %>%
  filter(customer_id %in% sm_50$customer_id) %>%
  tibble() %>%
  ggplot(aes(
    x = reading_datetime,
    y = general_supply_kwh
  ), alpha = 0.5) +
  geom_line() +
  scale_fill_gradient2() +
  facet_wrap(~customer_id, ncol = 2) +
  theme_void() +
  theme(strip.text.x = element_blank()) +
  xlab("Time [30m]") +
  ylab("electricity demand in kwh")


ggsave("figs/raw_plot_cust.png")


##----tab-distribution
granularity = c("g1", "g2", "g3")
alternate_design =  c("g10 ~ N(0, 1), g11 ~ N(2, 1)",
                      "g21 ~ N(2, 1), g22 ~ N(1, 1), g23 ~ N(0, 1)",
                      "g31 ~ N(0, 1), g32 ~ N(1, 1), g33 ~ N(2, 1), g34 ~ N(1, 1), g35 ~ N(0, 1)")

# alternate_design =  c("g11 ~ N(2, 1)",
#                       "g21 ~ N(2, 1), g22 ~ N(1, 1)",
#                       "g32 ~ N(1, 1), g33 ~ N(2, 1), g34 ~ N(1, 1)")

tab_dist <- tibble::tibble(granularity = granularity, `Varying distributions` = alternate_design)

# knitr::kable(tab_dist, caption = "Alternate distributions of different categories if they deviate from null.")


##----tab-design
design = c("design-1", "design-2", "design-3", "design-4", "design-5")
g1 = c("fixed", "vary", "fixed", "fixed", "vary")
g2 = c("fixed", "fixed", "vary", "fixed", "vary")
g3 = c("fixed", "fixed", "fixed", "vary", "vary")
table <- tibble(design, g1, g2, g3)

#%>% kable(caption = "5 different designs resulting from considering different distributions across categories.")


##----tab-dist-design
# library(kableExtra)
#
# knitr::kables(list(kable(caption = "Alternate distributions of different categories if they deviate from null.",
#    tab_dist
#     ) %>% kable_styling(),
#   kable(caption = "5 different designs resulting from considering different distributions across categories.",
#  tibble(design, g1, g2, g3)
#     ) %>% kable_styling()
#   )
# ) %>% kable_styling()

knitr::kable(
  list(tab_dist,  tibble(design, g1, g2, g3)),
  caption = 'For Scenario (a), distributions of different categories when they vary (top). If distributions are fixed, they are set to N(0, 1). 5 designs resulting from different distributions across categories (below)',
  booktabs = TRUE, valign = 't'
)

##----generate-design-3change
generate_design <- function(t, mu1, mu2, mu3){

  t <- seq(0, t, 1)
  g1 <- t %%2
  g2 <- t %%3
  g3 <- t %%5

  # null design
  g1_dnull <- rep( rep(0, each = length(unique(g1))), length.out= length(t))
  g2_dnull <- rep( rep(0, each = length(unique(g2))), length.out= length(t))
  g3_dnull <- rep( rep(0, each = length(unique(g3))), length.out= length(t))

  # mean changing across categories in varying ways

  g1_dvary <- rep(mu1, length.out= length(t))
  g2_dvary <- rep(mu2, length.out= length(t))
  g3_dvary <- rep(mu3, length.out= length(t))


  design1 = distributional::dist_normal(g1_dnull + g2_dnull + g3_dnull)
  design2 = distributional::dist_normal(g1_dvary + g2_dnull + g3_dnull)
  design3 = distributional::dist_normal(g1_dnull + g2_dvary + g3_dnull)
  design4 = distributional::dist_normal(g1_dnull + g2_dnull + g3_dvary)
  design5 = distributional::dist_normal(g1_dvary + g2_dvary + g3_dvary)

  data_bind <- tibble::tibble(
    index = t,
    g1 = g1,
    g2 = g2,
    g3 = g3,
    design1 = distributional::generate(design1, times = 1) %>% unlist(),
    design2 = distributional::generate(design2, times = 1) %>% unlist(),
    design3 = distributional::generate(design3, times = 1) %>% unlist(),
    design4 = distributional::generate(design4, times = 1) %>% unlist(),
    design5 = distributional::generate(design5, times = 1) %>% unlist()
  ) %>%
    pivot_longer(-c(1, 2, 3, 4), names_to = "design", values_to = "sim_data")

  data_bind
}

t = 300
mu1= c(0, 2)
mu2 = c(2, 1, 0)
mu3 = c(0, 1, 2, 1, 0)

data_bind <- generate_design(t, mu1, mu2, mu3)

##----plot-3gran-new
# plot_linear_data <- function(data){
# ggplot(data,
#              aes(x = index, y = sim_data)) +
#   geom_line() +
#   xlab("index")+
#   theme_bw()
# }

p1 <- ggplot(data_bind,
             aes(x = index, y = sim_data)) +
  geom_line() +
  xlab("index")+
  facet_wrap(~design, scales = "free_y",ncol =1) +
  theme_validation()

p2 <- ggplot(data_bind,
             aes(x = as.factor(g1), y = sim_data)) +
  geom_boxplot(alpha =0.5) + xlab("g1") +
  facet_wrap(~design, scales = "free_y", ncol = 1)+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") +
  theme_validation()



p3 <- ggplot(data_bind, aes(x = as.factor(g2), y = sim_data)) + geom_boxplot(alpha =0.5) + xlab("g2") + theme_bw() +
  facet_wrap(~design, scales = "free_y",ncol = 1)+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") + ylab("")+
  theme_validation()

p4 <- ggplot(data_bind, aes(x = as.factor(g3), y = sim_data)) + geom_boxplot(alpha =0.5) +
  xlab("g3") + theme_bw()+
  facet_wrap(~design, scales = "free_y", ncol = 1)+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue")+ ylab("")+
  theme_validation()


(p1+ (p2 + p3 + p4 + ggtitle("(a)")) + plot_layout(widths = c(2, 1)))&theme(plot.title = element_text(hjust = -3))

#
# plot_cyclic_data( data_bind %>% filter(design=="design2"))
# plot_data( data_bind %>% filter(design=="design3"))
# plot_data( data_bind %>% filter(design=="design4"))
# plot_data( data_bind %>% filter(design=="design5"))

##----generate-design-new

generate_design <- function(n = 300, #length of time series
                            mu11 = 0,
                            mu12 = 0,
                            mu21 = 0,
                            mu22 = 0 ,
                            mu23 = 0,
                            mu31 = 0,
                            mu32 = 0,
                            mu33 = 0,
                            mu34 = 0,
                            mu35 = 0)
{

  n <- n+500 # 500 burning observations
  t <- seq(0, n-1, 1)

  g1 <- t %%2
  g2 <- t %%3
  g3 <- t %%5

  str_gran <- bind_cols(index = t, g1 = g1, g2 = g2, g3 = g3)

  # calculation of g1
  g1_table <- bind_cols(g1 = unique(g1), dist_mean = c(mu11, mu12))

  g1_tally <- str_gran %>% group_by(g1) %>%
    count() %>%
    left_join(g1_table, by = "g1")

  g1_dist <- g1_tally %>%
    mutate(g1_d = list(rep(g1, each = n)),
           g1_dist = list(rnorm(n, dist_mean, 1))) %>%
    ungroup() %>%
    select(g1_d, g1_dist) %>%
    unnest(cols = c(g1_d, g1_dist))

  g1_data <- str_gran %>% arrange(g1) %>% bind_cols(g1_dist = g1_dist$g1_dist) %>% arrange(index)

  # calculation of g2

  g2_table<- bind_cols(g2 = unique(g2),
                       dist_mean = c(mu21, mu22, mu23))

  g2_tally <- str_gran %>%
    group_by(g2) %>%
    count() %>%
    left_join(g2_table, by = "g2")

  g2_dist <- g2_tally %>%
    mutate(g2_d = list(rep(g2, each = n)),
           g2_dist = list(rnorm(n, dist_mean, 1))) %>%
    ungroup() %>%
    select(g2_d, g2_dist) %>%
    unnest(cols = c(g2_d, g2_dist))

  g2_data <- str_gran %>% arrange(g2) %>% bind_cols(g2_dist = g2_dist$g2_dist) %>% arrange(index)

  # calculation of g3

  g3_table<- bind_cols(g3 = unique(g3), dist_mean = c(mu31, mu32, mu33, mu34, mu35))

  g3_tally <- str_gran %>% group_by(g3) %>% count() %>% left_join(g3_table, by = "g3")

  g3_dist <- g3_tally %>%
    mutate(g3_d = list(rep(g3, each = n)),
           g3_dist = list(rnorm(n, dist_mean, 1))) %>%
    ungroup() %>%
    select(g3_d, g3_dist) %>%
    unnest(cols = c(g3_d, g3_dist))

  g3_data <- str_gran %>% arrange(g3) %>% bind_cols(g3_dist = g3_dist$g3_dist) %>% arrange(index)


  innov_data <- g1_data %>%
    left_join(g2_data %>% select(index, g2_dist), by = "index") %>%
    left_join(g3_data %>% select(index, g3_dist), by = "index")


  nd_time = innov_data %>% mutate(ts = g1_dist + g2_dist + g3_dist)

  nd_time
}

##----generate-design-2gran-data

set.seed(123)

nTj = 300
mean_diffj = 2

design1 <- generate_design(n=nTj) # null design
design2 <- generate_design(n=nTj,
                           mu21 = mean_diffj,
                           mu32=mean_diffj ,
                           mu34=mean_diffj)

design3 <- generate_design(n=nTj,
                           mu22=mean_diffj,
                           mu32=mean_diffj ,
                           mu34=mean_diffj)

design4 <- generate_design(n=nTj,
                           mu23 = mean_diffj,
                           mu31 = 2*mean_diffj ,
                           mu32=mean_diffj)
data_bind <- bind_rows(design1, design2, design3, design4, .id = "design")


##----generate-design-2gran-plot

p1 <- ggplot(data_bind,
             aes(x = index, y = ts)) +
  geom_line() +
  xlab("index")+
  facet_wrap(~design, scales = "free_y",ncol =1, labeller = "label_both") +
  theme_validation()

p2 <- ggplot(data_bind,
             aes(x = as.factor(g1), y = ts)) +
  geom_boxplot(alpha =0.5) + xlab("g1") +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both")+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") +
  theme_validation()


p3 <- ggplot(data_bind, aes(x = as.factor(g2), y = ts)) + geom_boxplot(alpha =0.5) + xlab("g2") + theme_bw() +
  facet_wrap(~design, scales = "free_y",ncol = 1, labeller = "label_both")+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") + ylab("")+
  theme_validation()

p4 <- ggplot(data_bind, aes(x = as.factor(g3), y = ts)) + geom_boxplot(alpha =0.5) +
  xlab("g3") + theme_bw()+
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both")+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue")+ ylab("")+
  theme_validation()

gran2_change <- (p2 + p3 + p4) *
  theme_validation()
#+  plot_annotation('(a)')

##----generate-design-1gran-data

set.seed(123)

nTj = 300
mean_diffj = 2

design1 <- generate_design(n=nTj)


design2 <- generate_design(n=nTj,
                           mu32=mean_diffj)

design3 <- generate_design(n=nTj,
                           mu35 = mean_diffj)

design4 <- generate_design(n=nTj,
                           mu32 = mean_diffj,
                           mu33 = mean_diffj)

data_bind <- bind_rows(design1, design2, design3, design4,  .id = "design")

##----generate-design-1gran-plot

p1 <- ggplot(data_bind,
             aes(x = index, y = ts)) +
  geom_line() +
  xlab("index")+
  facet_wrap(~design, scales = "free_y",ncol =1, labeller = "label_both") +
  theme_validation()

p2 <- ggplot(data_bind,
             aes(x = as.factor(g1), y = ts)) +
  geom_boxplot(alpha =0.5) + xlab("g1") +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both")+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") +
  theme_validation()


p3 <- ggplot(data_bind, aes(x = as.factor(g2), y = ts)) + geom_boxplot(alpha =0.5) + xlab("g2") + theme_bw() +
  facet_wrap(~design, scales = "free_y",ncol = 1, labeller = "label_both")+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") + ylab("")+
  theme_validation()

p4 <- ggplot(data_bind, aes(x = as.factor(g3), y = ts)) + geom_boxplot(alpha =0.5) +
  xlab("g3") + theme_bw()+
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both")+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue")+ ylab("")+
  theme_validation()

gran1_change <- (p2 + p3 + p4) *
  theme_validation()
#+  plot_annotation('(b)')


##----gran2and1-clubbed

 gran2_change <- gran2_change + ggtitle("(b)")+ theme(plot.title = element_text(hjust = -2.5))
 gran1_change <- gran1_change + ggtitle("(c)")+ theme(plot.title = element_text(hjust = -2.5))
 ggpubr::ggarrange(gran2_change, gran1_change)


# gran1_change + gran2_change + plot_layout(widths = c(1, 1, 1, 1, 1, 1))


##----interaction-gran

##designs

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


##data

library(hakear)

nx_val = 2 # number of x-axis levels
nfacet_val = 3 # number of facet levels
w1_val = 2 # increment in mean
w2_val = 0 # increment in sd
mean_val = 0 # mean of normal distribution of starting combination
sd_val = 2 # sd of normal distribution of starting combination
quantile_prob_val = seq(0.1, 0.9, 0.1)
ntimes_val = 300

sim_panel_varall <- sim_panel(
  nx = nx_val, nfacet =  nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varall_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varall_normal(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
) %>% unnest(data)  %>%
  rename( "g2" = "id_facet",
          "g1" = "id_x")

sim_panel_varx <- sim_panel(
  nx = nx_val, nfacet =  nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varx_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varx_normal(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
) %>% unnest(data)  %>%
  rename( "g2" = "id_facet",
          "g1" = "id_x")

sim_panel_varf <- sim_panel(
  nx = nx_val, nfacet =  nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varf_normal(2, 3, 5, 10, 5, 5)
  sim_dist = sim_varf_normal(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
) %>% unnest(data)  %>%
  rename( "g2" = "id_facet",
          "g1" = "id_x")

sim_panel_null <- sim_panel(
  nx = nx_val,
  nfacet =  nfacet_val,
  ntimes = ntimes_val,
  sim_dist = distributional
  ::dist_normal(mean_val, sd_val)
) %>% unnest(c(data)) %>%
  rename( "g2" = "id_facet",
          "g1" = "id_x")


##category-plots

p_null <- sim_panel_null %>%
  ggplot(aes(x = as.factor(g1), y = sim_data)) +
  facet_wrap(~g2, labeller = "label_both") +
  geom_boxplot() +
  ylab("") +
  #ggtitle(paste("(a)", round(null, 2))) +
  xlab("") +
  theme_bw()+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") +
  #ggtitle("design1")+
  theme_validation() +ylab("")  +
  theme(panel.spacing =unit(0, "lines"))+ theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme(plot.margin = margin(0, 0, 0, 0, "cm") )

p_varf <- sim_panel_varf %>%
  ggplot(aes(x = as.factor(g1), y = sim_data)) +
  facet_wrap(~g2, labeller = "label_both") +
  geom_boxplot() +
  ylab("") +
  #ggtitle(paste("(b)", round(varf, 2))) +
  xlab("")+
  theme_bw()+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") +
  #ggtitle("design2")+
  theme_validation()+ylab("") +
  theme(panel.spacing =unit(0, "lines"))+ theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme(plot.margin = margin(0, 0, 0, 0, "cm") )

p_varx <- sim_panel_varx %>%
  ggplot(aes(x = as.factor(g1), y = sim_data)) +
  facet_wrap(~g2, labeller = "label_both") +
  geom_boxplot()+
  ylab("")  +
  #ggtitle(paste("(c)", round(varx, 2))) +
  xlab("")+
  theme_bw()+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") +
  #ggtitle("design3")+
  theme_validation()+ylab("") +
  theme(panel.spacing =unit(0, "lines"))+ theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme(plot.margin = margin(0, 0, 0, 0, "cm") )

p_varall <- sim_panel_varall %>%
  ggplot(aes(x = as.factor(g1), y = sim_data)) +
  facet_wrap(~g2, labeller = "label_both") +
  geom_boxplot() +
  ylab("") +
  #ggtitle(paste("(d)", round(varall, 2))) +
  xlab("")+
  theme_bw()+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 0.8, color = "blue") +
  #ggtitle("design4") +
  theme_validation()+ylab("") +
  theme(panel.spacing =unit(0, "lines"))+ theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme(plot.margin = margin(0, 0, 0, 0, "cm") )


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
  scale_x_continuous(breaks = seq(1, endbreaks, 200))+
  theme_bw() +
  #geom_point(alpha = 0.5, color = "blue")
  #theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #    panel.grid.minor.x =  element_blank())+
  ylab("")+
  xlab("")


p2 <- change_index(sim_panel_varf) %>%
  ggplot(aes(x = time,
             y = sim_data)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, 200))+
  theme_bw() +
  #geom_point(alpha = 0.5, color = "blue") +
  #theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #   panel.grid.minor.x =  element_blank())+
  ylab("")+
  xlab("")

p3 <- change_index(sim_panel_varx) %>%
  ggplot(aes(x = time,
             y = sim_data)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, 200))+
  theme_bw() +
  #geom_point(alpha = 0.5, color = "blue")+
  #theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #     panel.grid.minor.x =  element_blank())+
  ylab("")+
  xlab("")


p4 <- change_index(sim_panel_varall) %>%
  ggplot(aes(x = time,
             y = sim_data)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, 200))+
  theme_bw() +
  #geom_point(alpha = 0.5, color = "blue") +
  theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
        panel.grid.minor.x =  element_blank())+
  ylab("")+
  xlab("")


#((p1 + p_null)/( p2 + p_varf)/(p3 + p_varx)/(p4 + p_varall))
#p_null+ p_varf + p_varx + p_varall
p_null +p_varf + p_varx + p_varall +
  plot_annotation(tag_levels = list(c('D1', 'D2', 'D2', 'D4')))

#labels = c("Design-1", "Design-2", "Design-3", "Design-4"))


##----parcoord-sim
append_files_plot <- function(folder_name, path){
  all_files = list.files(path = paste0(folder_name,path),
                         pattern = "data_validation_")

  names_levels <- map_dfr(all_files,
                          function(x){
                            z = str_split(str_remove(x, ".rds"), "_") %>%
                              unlist()
                            bind_cols(index = z[3])
                          })

  all_files_path <- paste0(folder_name, path,
                           all_files)


  all_data <- lapply(1:length(all_files_path), function(x){

    data = all_files_path %>% magrittr::extract2(x) %>%
      readRDS()

    names = names_levels %>% magrittr::extract(x,)
    names_rep =   names %>% slice(rep(1:n(), each = nrow(data)))
    bind_cols(names_rep, data)
  }) %>% bind_rows()
  #
  # all_data <- append_files("js-nqt")

  # compute all inter cluster distance "from" group
  all_mat_mat_from <- all_data%>%
    group_by(index, gran, group_item1, group_item2) %>%
    filter(group_item1!= group_item2) %>%
    summarise(sum = sum(distance),.groups = "drop") %>%
    pivot_wider(names_from = group_item2, values_from = sum) %>% ungroup %>%
    replace(is.na(.), 0) %>%
    mutate(distance = rowSums(across(where(is.numeric)))) %>%
    dplyr::select(c(index, gran, group_item1, distance)) %>%
    rename("group"="group_item1")


  # compute all inter cluster distance "to" group

  all_mat_mat_to <- all_data%>%
    group_by(index, gran, group_item1, group_item2) %>%
    filter(group_item1!= group_item2) %>%
    summarise(sum = sum(distance),.groups = "drop") %>%
    pivot_wider(names_from = group_item1, values_from = sum) %>% ungroup %>%
    replace(is.na(.), 0) %>%
    mutate(distance = rowSums(across(where(is.numeric)))) %>%
    dplyr::select(c(index, gran, group_item2, distance))%>%
    rename("group"="group_item2")

  # compute all inter cluster distance to and from

  data_pcp <- bind_rows(all_mat_mat_from, all_mat_mat_to) %>%
    group_by(index, gran, group) %>%
    summarise(inter_distance = sum(distance),.groups = "drop") %>%
    pivot_wider(names_from = gran, values_from = inter_distance) %>% ungroup %>%
    mutate(group = as.factor(group))

  parcoord_validation <- GGally::ggparcoord(data_pcp ,
                                 columns = 3:ncol(data_pcp),
                                 groupColumn = "group",
                                 showPoints = TRUE,
                                 alphaLines = 0.5,
                                 scale = "globalminmax")+ scale_colour_manual(values = rep("black", each = 5))
  parcoord_validation

  #
  # pairsplot <- ggpairs(data_pcp,
  #                      columns = 3:5,
  #                      aes(fill=group, color = group), alpha = 0.5) +
  #   scale_fill_viridis_d(direction = -1) +
  #   scale_color_viridis_d(direction = -1) +
  #   theme_light()

}
design1 <- append_files_plot(folder_name = "data/gracsr/validation/js-nqt", path = "/3gran_change_5D/")
design2 <- append_files_plot(folder_name = "data/gracsr/validation/js-nqt", path = "/2gran_change_4D/")
design3 <- append_files_plot(folder_name = "data/gracsr/validation/js-nqt", path = "/1gran_change_5D/")

# (design1 + design2 + design3)&theme(legend.position = "bottom")
#
#
# design1 <- append_files_plot(folder_name = "js-robust", path = "/3gran_change_5D/")
# design2 <- append_files_plot(folder_name = "js-robust", path = "/2gran_change_4D/")
# design3 <- append_files_plot(folder_name = "js-robust", path = "/1gran_change_5D/")
#
# (design1 + design2 + design3)&theme(legend.position = "bottom")
#
#
# design1 <- append_files_plot(folder_name = "js-nqt", path = "/3gran_change_5D/")
# design2 <- append_files_plot(folder_name = "js-nqt", path = "/2gran_change_4D/")
# design3 <- append_files_plot(folder_name = "js-nqt", path = "/1gran_change_5D/")

(design1 + design2 + design3 + plot_layout(guides = "collect")+ plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')'))*theme_characterisation()&theme(legend.position = "none")&xlab("")


##----mds-plot-validation
##
mds_data <- function(data){
  total_distance <- data %>%
    group_by(item1, item2) %>%
    summarise(total = sum(distance), .groups = "drop") %>% ungroup()

  total_distance_wide <- total_distance%>%
    pivot_wider(names_from = item2, values_from = total)

  rownames(total_distance_wide) <- total_distance_wide$item1

  mds_data <- total_distance_wide %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    tibble::rownames_to_column() %>%
    dplyr::select(-item1) %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from=rowname, values_from=value)

  rownames(mds_data) <- total_distance_wide$item1

  df <- mds_data[-1] %>% as.matrix()
  DM <- matrix(0, ncol(mds_data), ncol(mds_data))
  DM[lower.tri(DM)] = df[lower.tri(df, diag=TRUE)] # distance metric
  f = as.dist(DM)

  mds <- f %>%
    cmdscale() %>%
    as_tibble()

  colnames(mds) <- c("Dim.1", "Dim.2")

  groups<-bind_rows(data %>%
                      distinct(item1, group_item1) %>%
                      rename("item" = "item1",
                             "group" = "group_item1"),
                    data %>%
                      distinct(item2, group_item2) %>%
                      rename("item" = "item2",
                             "group" = "group_item2")) %>% distinct(item, group)



  all_data_cluster <- cbind(groups, mds) %>%
    mutate(group = as.factor(group)) %>% as_tibble()
}


# original simulation table
niter <- c(5, 50, 100) #number of series you are clustering
nT <-  c(300, 1000, 5000) # length of the time series
mean_diff <- c(1, 2, 5) # difference between consecutive categories

simtable <- expand.grid(mean_diff = mean_diff,
                        niter = niter,
                        #time_series = time_series,
                        nT = nT)

data1 <- mds_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_25.rds")) %>% mutate(diff = 1 , design= "a")
data2 <-mds_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_26.rds")) %>% mutate(diff = 2 , design= "a")
data3 <-mds_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_27.rds")) %>% mutate(diff = 5 , design= "a")

data4 <- mds_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_7.rds")) %>% mutate(diff = 1 , design= "b")
data5 <- mds_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_17.rds")) %>% mutate(diff = 2 , design= "b")
data6 <- mds_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_9.rds")) %>% mutate(diff = 5 , design= "b")


data7 <- mds_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_16.rds"))%>% mutate(diff = 1 , design= "c")
data8 <-mds_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_17.rds"))%>% mutate(diff = 2 , design= "c")
data9 <-mds_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_18.rds"))%>% mutate(diff = 5 , design= "c")

all_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9) %>% rename("Scenario" ="design")


mds_plot_10 <- ggplot(all_data,
                      aes(x = Dim.1,
                          y = Dim.2,
                          color = group)) +
  geom_point(size = 2, alpha = 0.5, aes(shape = group)) +
  facet_grid(diff~Scenario, labeller = "label_both") +
  #geom_text(check_overlap = TRUE)  +
  theme_bw()+
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2")+
  ylab("mds1") + xlab("mds2") +
  scale_shape_manual(values = c(3, 4, 2, 0, 1)) 

mds_plot_10

##### application
##----prototype-data-pick
quantile_prob_graph <- c(0.25, 0.5, 0.75)

# data_pick_one <- c(8618759, 8291696, 10357256, 8290374) %>% as_tibble %>% set_names("customer_id")
# data_pick_two <- c(9044864, 8642053, 10534367, 9021526,11162275) %>% as_tibble %>% set_names("customer_id")
# data_pick_three <- c(8221762, 8273636, 10359424, 8232822)%>% as_tibble %>% set_names("customer_id")


data_pick_one <- c(8541744, 9355808, 8603880, 8619309, 10542667) %>% as_tibble %>% set_names("customer_id")
#data_pick_two <- c(8688242, 8643837, 8184707, 10534355, 8684420) %>% as_tibble%>% set_names("customer_id")
data_pick_three <- c(9792072, 8589936, 8454235, 10692366, 8603828)%>% as_tibble%>% set_names("customer_id")
data_pick_four <- c(8618759, 8291696, 10357256, 8290374) %>% as_tibble %>% set_names("customer_id")
data_pick_five <- c(9044864, 8642053, 10534367, 9021526,11162275) %>% as_tibble %>% set_names("customer_id")
data_pick_six <- c(8221762, 8273636, 10359424, 8232822, 11450499)%>% as_tibble %>% set_names("customer_id")


##----assemble
data_pick_cust <- bind_rows(
data_pick_one,
#data_pick_two,
data_pick_three,
data_pick_four,
data_pick_five,
data_pick_six,
.id = "design") %>%
  mutate(customer_id = as.character(customer_id))

##----data-pick
data_pick <- read_rds(here::here("data/gracsr/elec_nogap_2013_clean_356cust.rds")) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  dplyr::filter(customer_id %in% data_pick_cust$customer_id) %>%
  gracsr::scale_gran( method = "robust",
                      response = "general_supply_kwh")

##----hod-moy-wkndwday plots
data_hod <- quantile_gran(data_pick,
                          "hour_day",
                          quantile_prob_val = quantile_prob_graph) %>%
  pivot_wider(names_from = quantiles,
              values_from = quantiles_values) %>%
  left_join(data_pick_cust, by = c("customer_id"))

#data_heatmap_hod$customer_id = factor(data_heatmap_hod$customer_id, levels = data_pick_cust$value)
data_hod$category <- factor(data_hod$category, levels = 0:23)

hod_ind_design <- data_hod %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(ymin = `25%`,
                  ymax = `75%`,
                  group=customer_id),
              alpha = 0.5) +
  geom_line(aes(y = `50%`,
                group=customer_id),
            size = 1) +
  facet_wrap(~customer_id,
             scales = "free_y",
             nrow = 24) +
  theme_application() +
  xlab("hod") +
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  scale_x_discrete(breaks = seq(0, 23, 3))

data_moy <- quantile_gran(data_pick,
                          "month_year",
                          quantile_prob_val = quantile_prob_graph) %>%
  pivot_wider(names_from = quantiles,
              values_from = quantiles_values) %>%
  left_join(data_pick_cust, by = c("customer_id"))

data_moy$category <- factor(data_moy$category, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

moy_ind_design <- data_moy %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(ymin = `25%`,
                  ymax = `75%`,
                  group=customer_id),
              alpha = 0.5) +
  geom_line(aes(y = `50%`, group=customer_id), size = 1) +
  facet_wrap(~customer_id,
             scales = "free_y",
             nrow = 24) +
  ylab("demand (in Kwh)") +
  xlab("moy")  +
  theme_application()

data_wkndwday <- data_pick  %>%
  create_gran("wknd_wday")  %>%
  left_join(data_pick_cust, by = c("customer_id"))

ylim1 = boxplot.stats(data_wkndwday$general_supply_kwh)$stats[c(1, 5)]

wkndwday_ind_design <- data_wkndwday%>%
  ggplot(aes(x=wknd_wday, y = general_supply_kwh)) +
  #lvplot::geom_lv(aes(fill = as.factor(design),
  #                   color = as.factor(design)), k=5, alpha = 0.5) +
  geom_boxplot(alpha = 0.5, fill = "black")+
  #geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = ylim1*1.05)+
  facet_wrap(~customer_id,
             scales = "free_y",
             labeller = "label_value",
             nrow = 24)  +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 1, color = "black")+
  xlab("wnwd")+
  theme_application()
##----all-data
data_pick <- read_rds(here::here("data/gracsr/elec_nogap_2013_clean_356cust.rds")) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  dplyr::filter(customer_id %in% data_pick_cust$customer_id) %>%
  gracsr::scale_gran( method = "nqt",
                      response = "general_supply_kwh")


##----clustering
hod <- suppressMessages(data_pick %>%
                          dist_gran(gran1 = "hour_day", response = "general_supply_kwh"))

moy <- suppressMessages(data_pick %>%
                          dist_gran(gran1 = "month_year", response = "general_supply_kwh"))

wkndwday <- suppressMessages(data_pick %>%
                               dist_gran(gran1 = "wknd_wday", response = "general_supply_kwh"))

distance <- wkndwday/2 + moy/12 + hod/24

f = as.dist(distance)

##----opt-clusters
library(fpc)
library(cluster)
k = array()
for(i in 2:20)
{
  group <- f %>% hclust (method = "ward.D") %>% cutree(k=i)
  p <- cluster.stats(f, clustering = group, silhouette = TRUE)
  k[i]=p$sindex
}
#
# ggplot(k %>% as_tibble %>% mutate(k = row_number()), aes(x=k, y = value)) + geom_line() + scale_x_continuous(breaks = seq(2, 20, 1))

##----groups-24
cluster_result <- suppressMessages(f %>%
                                     clust_gran(kopt = 5)) %>%
  rename("customer_id" = "id") %>%
  mutate(group = as.factor(group))

##----hod-ind-group
hod_ind_group <- data_hod %>%
  left_join(cluster_result, by = c("customer_id")) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(ymin = `25%`,
                  ymax = `75%`,
                  group=customer_id, fill = group),
              alpha = 0.5) +
  geom_line(aes(y = `50%`,
                group=customer_id,
                color = group), size = 1) +
  facet_wrap(group~customer_id,
             scales = "free_y",
             nrow=8) +
  theme_application()+
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7")) +    xlab("hod")  +
  scale_x_discrete(breaks = seq(0, 23, 3))+ theme(legend.position = "bottom")+theme(plot.margin = unit(c(0,0,0,-1), "cm"))
# + scale_y_continuous(breaks = NULL)

moy_ind_group <- data_moy %>%
  left_join(cluster_result, by = c("customer_id")) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(ymin = `25%`,
                  ymax = `75%`,
                  group=customer_id, fill = group), alpha = 0.5) +
  geom_line(aes(y = `50%`, group=customer_id, color = group), size = 1) +
  facet_wrap(group~customer_id,
             scales = "free_y",
             nrow=8) +
  ylab("demand (in Kwh)") +
  xlab("moy")  +
  theme_application() +
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7")) + theme(legend.position = "bottom")+
  theme(plot.margin = unit(c(0,-1,0, -1), "cm"))



wkndwday_ind_group <- data_wkndwday%>%
  left_join(cluster_result, by = c("customer_id")) %>%
  mutate(group  = as.factor(group)) %>%
  ggplot(aes(x=wknd_wday, y = general_supply_kwh)) +
  #lvplot::geom_lv(aes(fill = as.factor(group),
  #            color = as.factor(group)), k=5, alpha = 0.5) +
  geom_boxplot(aes(color = group, fill = group),alpha = 0.5)+
  #geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = ylim1*1.05)+
  facet_wrap(group~customer_id,
             scales = "free_y",
             labeller = "label_value",
             nrow=8)  +
  theme_application()+
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7"))  + theme(legend.position = "none")+
  xlab("wnwd")+theme(plot.margin = unit(c(0,-1,0,0), "cm"))

# hod_ind_design + moy_ind_design + wkndwday_ind_design
hod_ind_group + moy_ind_group + wkndwday_ind_group +  plot_layout(guides = "collect", ncol = 6) + plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & theme(legend.position = 'none')


##----data-heatmap-hod-group
legend_title <- "group"

data_group <- data_pick  %>%
  left_join(cluster_result, by = c("customer_id"))

data_heatmap_hod_group <- quantile_gran(data_group,
                                        gran1="hour_day",
                                        quantile_prob_val = c(0.25, 0.5, 0.75),
                                        group="group") %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)


data_heatmap_hod_group$category <- factor(data_heatmap_hod_group$category, levels = 0:23)

data_heatmap_hod_group$group <- paste("group", data_heatmap_hod_group$group, sep = "-")

hod_group <- data_heatmap_hod_group %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(ymin = `25%`,
                  ymax = `75%`,
                  group=group,
                  fill = as.factor(group), alpha = 0.5),
              alpha = 0.5) +
  geom_line(aes(y = `50%`,
                group=group,
                color = as.factor(group)), size = 1)+
  facet_wrap(~group,
             scales = "free_y",
             nrow = 5) +
  #labeller = labeller(xfacet = c(`1` = "Group 2", `2` = "Group 4",`3` = "Group 1",`4` = "Group 3"))
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("hod") +
  ylab("demand (in Kwh)") +
  theme_bw()  +
  scale_x_discrete(breaks = seq(1, 24, 3))+
  #theme(strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme_application() +
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7")) +
  theme(legend.position = "bottom")

##----data-heatmap-moy-group
data_heatmap_moy_group <- quantile_gran(data_group,
                                        gran1="month_year",
                                        quantile_prob_val = c(0.25, 0.5, 0.75),
                                        group="group") %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)

data_heatmap_moy_group$category <- factor(data_heatmap_moy_group$category, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


data_heatmap_moy_group$group <- paste("group", data_heatmap_moy_group$group, sep = "-")


moy_group <- data_heatmap_moy_group %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(ymin = `25%`,
                  ymax = `75%`, group=group, fill = as.factor(group)), alpha = 0.5) +
  geom_line(aes(y = `50%`, group=group, color = as.factor(group)), size = 1 ) +
  facet_wrap(~group,
             scales = "free_y",
             labeller = "label_value",
             nrow = 5) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("moy") +
  ylab("demand (in Kwh)") +
  theme_bw() + theme_application() +
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7")) +
  theme(legend.position = "bottom")

##----data-heatmap-wkndwday-group
wkndwday_data <- data_group %>% create_gran("wknd_wday") %>%
  create_gran("hour_day")

ylim1 = boxplot.stats(wkndwday_data$general_supply_kwh)$stats[c(1, 5)]

wkndwday_group <- wkndwday_data%>%
  ggplot(aes(x=hour_day, y = general_supply_kwh)) +
  #lvplot::geom_lv(aes(fill = as.factor(group)), k=5) +
  geom_boxplot(aes(fill = group, color = group),alpha = 0.5, outlier.alpha = 0.05)+
  #geom_boxplot(outlier.size = 1) +
  coord_cartesian(ylim = ylim1*1.05)+
  #ggridges::geom_density_ridges2(aes(x = general_supply_kwh, y = wknd_wday,fill = as.factor(group))) + coord_flip() +
  #geom_boxplot(aes(fill = as.factor(group))) +
  #scale_fill_lv() +
  xlab("wnwd") +
  ylab("demand (in Kwh)") +
  facet_grid(group~wknd_wday,
             scales = "free_y",
             labeller = "label_both") +
  theme_bw() + theme_application() +
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7")) +
  theme(legend.position = "none")


##----combined-groups-js
(hod_group + moy_group + wkndwday_group) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')+
  plot_layout(guides = "collect")& theme(legend.position = 'bottom')

##----data-pick-wpd
elec_600_wpd <- read_rds(here::here("data/gracsr/algo2-cust600-wpd-rawdata.rds"))

elec_pick <- elec_600_wpd %>%
  filter(customer_id %in% data_pick_cust$customer_id)

elec_pick_wide <- elec_pick %>% pivot_wider(-c(1, 2), names_from = "x_variable", values_from = wpd)

scaled_var <- elec_pick_wide

f <- elec_pick_wide[-1] %>% dist()

k = array()
for(i in 2:20)
{
  group <- f %>% hclust (method = "ward.D") %>% cutree(k=i)
  p <- cluster.stats(f, clustering = group, silhouette = TRUE)
  k[i]=p$sindex
}
# ggplot(k %>% as_tibble %>% mutate(k = row_number()), aes(x=k, y = value)) + geom_line() + scale_x_continuous(breaks = seq(2, 20, 1))

group <- f%>% hclust (method = "ward.D") %>% cutree(k=3)


cluster_result_wpd <- bind_cols(id = elec_pick_wide$customer_id, group = group)

data_pcp <- scaled_var %>%
  #bind_cols(customer_id =  elec_pick_wide$customer_id) %>%
  left_join(cluster_result_wpd , by = c("customer_id" = "id")) %>%
  select(customer_id, group, everything()) %>%
  mutate(group = as.factor(group))

data_table <- data_pcp %>% group_by(group) %>%
  summarise(nobs = n(),
            moy = round(median(month_year),2),
            hod = round(median(hour_day),2),
            wnwd = round(median(wknd_wday),2)) %>%
  select(-group)

rownames(data_table) <- c("group-1", "group-2", "group-3")

##----parcoord
parcoord <- GGally::ggparcoord(data_pcp ,
                               columns = 3:ncol(data_pcp),
                               groupColumn = "group",
                               showPoints = FALSE,
                               alphaLines = 0.8,
                               order = "anyClass",
                               scale = "globalminmax"
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size=10)
  )+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 10)) +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("wpd") + scale_fill_viridis_d(direction = 1) +
  scale_color_viridis_d(direction = 1) + theme_light()

(parcoord + gridExtra::tableGrob(data_table))+ plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & theme(legend.position = "bottom")

