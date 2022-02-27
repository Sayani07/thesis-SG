## Validation

## ----tab-distribution-------------------------------------------------------------------
granularity <- c("g1", "g2", "g3")
alternate_design <- c(
  "g10 ~ N(0, 1), g11 ~ N(2, 1)",
  "g21 ~ N(2, 1), g22 ~ N(1, 1), g23 ~ N(0, 1)",
  "g31 ~ N(0, 1), g32 ~ N(1, 1), g33 ~ N(2, 1),
                      g34 ~ N(1, 1), g35 ~ N(0, 1)"
)

tab_dist <- tibble::tibble(
  granularity = granularity,
  `Varying distributions` = alternate_design
)



## ----tab-design-------------------------------------------------------------------------
design <- c("design-1", "design-2", "design-3", "design-4", "design-5")
g1 = c("(0,0)", "(0,2)", "(0,0)", "(0,0)", "(0,2)")
g2 = c("(0,0,0)", "(0,0,0)", "(2,1,0)", "(0,0,0)", "(2,1,0)")
g3 = c("(0,0,0,0,0)", "(0,0,0,0,0)", "(0,0,0,0,0)", "(0,1,2,1,0)", "(0,1,2,1,0)")

table <- tibble(design, g1, g2, g3)



## ----tab-dist-design--------------------------------------------------------------------

knitr::kable(
  table,
  caption = "Summary of the data generating process for S1. The various distributions across levels of different granularities result in five designs. The granularities g1, g2 and g3 have 2,3 and 5 levels, each of which follows a normal distribution. The means of the distribution are presented for each level, and the standard deviation is set to 1. For example, g1 in design-2 is (0,2) implies $g1_{1} \\sim {N(0,1)}$ and $g1_{2} \\sim {N(2,1)}$.",booktabs = TRUE, valign = "t", escape= FALSE
) %>%
  kable_styling(latex_options = "hold_position")



## ----generate-design-3change------------------------------------------------------------
generate_design <- function(t, mu1, mu2, mu3) {
  t <- seq(0, t, 1)
  g1 <- t %% 2
  g2 <- t %% 3
  g3 <- t %% 5

  # null design
  g1_dnull <- rep(rep(0, each = length(unique(g1))), length.out = length(t))
  g2_dnull <- rep(rep(0, each = length(unique(g2))), length.out = length(t))
  g3_dnull <- rep(rep(0, each = length(unique(g3))), length.out = length(t))

  # mean changing across categories in varying ways

  g1_dvary <- rep(mu1, length.out = length(t))
  g2_dvary <- rep(mu2, length.out = length(t))
  g3_dvary <- rep(mu3, length.out = length(t))


  design1 <- distributional::dist_normal(g1_dnull + g2_dnull + g3_dnull)
  design2 <- distributional::dist_normal(g1_dvary + g2_dnull + g3_dnull)
  design3 <- distributional::dist_normal(g1_dnull + g2_dvary + g3_dnull)
  design4 <- distributional::dist_normal(g1_dnull + g2_dnull + g3_dvary)
  design5 <- distributional::dist_normal(g1_dvary + g2_dvary + g3_dvary)

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

t <- 300
mu1 <- c(0, 2)
mu2 <- c(2, 1, 0)
mu3 <- c(0, 1, 2, 1, 0)

data_bind <- generate_design(t, mu1, mu2, mu3)

## ----plot-3gran-new


p1 <- ggplot(
  data_bind,
  aes(x = index, y = sim_data)
) +
  geom_line() +
  xlab("index") +
  facet_wrap(~design, scales = "free_y", ncol = 1) +
  theme_validation()

p2 <- ggplot(
  data_bind,
  aes(x = as.factor(g1), y = sim_data)
) +
  geom_boxplot(alpha = 0.5) +
  xlab("g1") +
  facet_wrap(~design, scales = "free_y", ncol = 1) +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  theme_validation()



p3 <- ggplot(data_bind, aes(x = as.factor(g2), y = sim_data)) +
  geom_boxplot(alpha = 0.5) +
  xlab("g2") +
  theme_bw() +
  facet_wrap(~design, scales = "free_y", ncol = 1) +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  ylab("") +
  theme_validation()

p4 <- ggplot(data_bind, aes(x = as.factor(g3), y = sim_data)) +
  geom_boxplot(alpha = 0.5) +
  xlab("g3") +
  theme_bw() +
  facet_wrap(~design, scales = "free_y", ncol = 1) +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  ylab("") +
  theme_validation()


(p1 + (p2 + p3 + p4 + ggtitle("S1")) + plot_layout(widths = c(2, 1))) &
  theme(plot.title = element_text(hjust = -3.25))



## ----generate-design-new----------------------------------------------------------------

generate_design <- function(n = 300, # length of time series
                            mu11 = 0,
                            mu12 = 0,
                            mu21 = 0,
                            mu22 = 0,
                            mu23 = 0,
                            mu31 = 0,
                            mu32 = 0,
                            mu33 = 0,
                            mu34 = 0,
                            mu35 = 0) {
  n <- n + 500 # 500 burning observations
  t <- seq(0, n - 1, 1)

  g1 <- t %% 2
  g2 <- t %% 3
  g3 <- t %% 5

  str_gran <- bind_cols(index = t, g1 = g1, g2 = g2, g3 = g3)

  # calculation of g1
  g1_table <- bind_cols(g1 = unique(g1), dist_mean = c(mu11, mu12))

  g1_tally <- str_gran %>%
    group_by(g1) %>%
    count() %>%
    left_join(g1_table, by = "g1")

  g1_dist <- g1_tally %>%
    mutate(
      g1_d = list(rep(g1, each = n)),
      g1_dist = list(rnorm(n, dist_mean, 1))
    ) %>%
    ungroup() %>%
    select(g1_d, g1_dist) %>%
    unnest(cols = c(g1_d, g1_dist))

  g1_data <- str_gran %>%
    arrange(g1) %>%
    bind_cols(g1_dist = g1_dist$g1_dist) %>%
    arrange(index)

  # calculation of g2

  g2_table <- bind_cols(
    g2 = unique(g2),
    dist_mean = c(mu21, mu22, mu23)
  )

  g2_tally <- str_gran %>%
    group_by(g2) %>%
    count() %>%
    left_join(g2_table, by = "g2")

  g2_dist <- g2_tally %>%
    mutate(
      g2_d = list(rep(g2, each = n)),
      g2_dist = list(rnorm(n, dist_mean, 1))
    ) %>%
    ungroup() %>%
    select(g2_d, g2_dist) %>%
    unnest(cols = c(g2_d, g2_dist))

  g2_data <- str_gran %>%
    arrange(g2) %>%
    bind_cols(g2_dist = g2_dist$g2_dist) %>%
    arrange(index)

  # calculation of g3

  g3_table <- bind_cols(g3 = unique(g3), dist_mean = c(mu31, mu32, mu33, mu34, mu35))

  g3_tally <- str_gran %>%
    group_by(g3) %>%
    count() %>%
    left_join(g3_table, by = "g3")

  g3_dist <- g3_tally %>%
    mutate(
      g3_d = list(rep(g3, each = n)),
      g3_dist = list(rnorm(n, dist_mean, 1))
    ) %>%
    ungroup() %>%
    select(g3_d, g3_dist) %>%
    unnest(cols = c(g3_d, g3_dist))

  g3_data <- str_gran %>%
    arrange(g3) %>%
    bind_cols(g3_dist = g3_dist$g3_dist) %>%
    arrange(index)


  innov_data <- g1_data %>%
    left_join(g2_data %>% select(index, g2_dist), by = "index") %>%
    left_join(g3_data %>% select(index, g3_dist), by = "index")


  nd_time <- innov_data %>% mutate(ts = g1_dist + g2_dist + g3_dist)

  nd_time
}



## ----generate-design-2gran-data---------------------------------------------------------

set.seed(123)

nTj <- 300
mean_diffj <- 2

design1 <- generate_design(n = nTj) # null design
design2 <- generate_design(
  n = nTj,
  mu21 = mean_diffj,
  mu32 = mean_diffj,
  mu34 = mean_diffj
)

design3 <- generate_design(
  n = nTj,
  mu22 = mean_diffj,
  mu32 = mean_diffj,
  mu34 = mean_diffj
)

design4 <- generate_design(
  n = nTj,
  mu23 = mean_diffj,
  mu31 = 2 * mean_diffj,
  mu32 = mean_diffj
)
data_bind <- bind_rows(design1, design2, design3, design4, .id = "design")



## ----generate-design-2gran-plot---------------------------------------------------------

p1 <- ggplot(
  data_bind,
  aes(x = index, y = ts)
) +
  geom_line() +
  xlab("index") +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both") +
  theme_validation()

p2 <- ggplot(
  data_bind,
  aes(x = as.factor(g1), y = ts)
) +
  geom_boxplot(alpha = 0.5) +
  xlab("g1") +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both") +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  theme_validation()


p3 <- ggplot(data_bind, aes(x = as.factor(g2), y = ts)) +
  geom_boxplot(alpha = 0.5) +
  xlab("g2") +
  theme_bw() +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both") +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  ylab("") +
  theme_validation()

p4 <- ggplot(data_bind, aes(x = as.factor(g3), y = ts)) +
  geom_boxplot(alpha = 0.5) +
  xlab("g3") +
  theme_bw() +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both") +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  ylab("") +
  theme_validation()

gran2_change <- (p2 + p3 + p4) *
  theme_validation()
#+  plot_annotation('(a)')



## ----generate-design-1gran-data---------------------------------------------------------

set.seed(123)

nTj <- 300
mean_diffj <- 2

design1 <- generate_design(n = nTj)


design2 <- generate_design(
  n = nTj,
  mu32 = mean_diffj
)

design3 <- generate_design(
  n = nTj,
  mu35 = mean_diffj
)

design4 <- generate_design(
  n = nTj,
  mu32 = mean_diffj,
  mu33 = mean_diffj
)

data_bind <- bind_rows(design1, design2, design3, design4, .id = "design")



## ----generate-design-1gran-plot---------------------------------------------------------

p1 <- ggplot(
  data_bind,
  aes(x = index, y = ts)
) +
  geom_line() +
  xlab("index") +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both") +
  theme_validation()

p2 <- ggplot(
  data_bind,
  aes(x = as.factor(g1), y = ts)
) +
  geom_boxplot(alpha = 0.5) +
  xlab("g1") +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both") +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  theme_validation()


p3 <- ggplot(data_bind, aes(x = as.factor(g2), y = ts)) +
  geom_boxplot(alpha = 0.5) +
  xlab("g2") +
  theme_bw() +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both") +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  ylab("") +
  theme_validation()

p4 <- ggplot(data_bind, aes(x = as.factor(g3), y = ts)) +
  geom_boxplot(alpha = 0.5) +
  xlab("g3") +
  theme_bw() +
  facet_wrap(~design, scales = "free_y", ncol = 1, labeller = "label_both") +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  ylab("") +
  theme_validation()

gran1_change <- (p2 + p3 + p4) *
  theme_validation()
#+  plot_annotation('(b)')


## ----gran2and1-clubbed----------------------------------------------------------------

gran2_change <- gran2_change + ggtitle("S2") +
  theme(plot.title = element_text(hjust = -2.75))
gran1_change <- gran1_change + ggtitle("S3") +
  theme(plot.title = element_text(hjust = -2.75))
ggpubr::ggarrange(gran2_change, gran1_change)



## ----interaction-gran-------------------------------------------------------------------

## designs

sim_null_normal <- function(nxj, nfacetj, mean, sd, w1 = 0, w2 = 0) {
  rep(distributional::dist_normal(mu = mean, sigma = sd),
    times = nxj * nfacetj
  )
}

sim_varf_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  rep(dist_normal(
    (mean + seq(0, nfacet - 1, by = 1) * w1),
    (sd + seq(0, nfacet - 1, by = 1) * w2)
  ), each = nx)
}

sim_varx_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  rep(dist_normal(
    (mean + seq(0, nx - 1, by = 1) * w1),
    (sd + seq(0, nx - 1, by = 1) * w2)
  ), nfacet)
}

sim_varall_normal <- function(nx, nfacet, mean, sd, w1, w2) {
  dist_normal(
    (mean + seq(0, (nx * nfacet - 1), by = 1) * w1),
    (sd + seq(0, (nx * nfacet - 1), by = 1) * w2)
  )
}


## data

library(hakear)

nx_val <- 2 # number of x-axis levels
nfacet_val <- 3 # number of facet levels
w1_val <- 2 # increment in mean
w2_val <- 0 # increment in sd
mean_val <- 0 # mean of normal distribution of starting combination
sd_val <- 2 # sd of normal distribution of starting combination
quantile_prob_val <- seq(0.1, 0.9, 0.1)
ntimes_val <- 300

sim_panel_varall <- sim_panel(
  nx = nx_val, nfacet = nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varall_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varall_normal(
    nx_val, nfacet_val, mean_val,
    sd_val, w1_val, w2_val
  )
) %>%
  unnest(data) %>%
  rename(
    "g2" = "id_facet",
    "g1" = "id_x"
  )

sim_panel_varx <- sim_panel(
  nx = nx_val, nfacet = nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varx_normal(2, 3, 5, 10, 5, -1.5)
  sim_dist = sim_varx_normal(
    nx_val, nfacet_val, mean_val,
    sd_val, w1_val, w2_val
  )
) %>%
  unnest(data) %>%
  rename(
    "g2" = "id_facet",
    "g1" = "id_x"
  )

sim_panel_varf <- sim_panel(
  nx = nx_val, nfacet = nfacet_val,
  ntimes = ntimes_val,
  # sim_dist = sim_varf_normal(2, 3, 5, 10, 5, 5)
  sim_dist = sim_varf_normal(
    nx_val, nfacet_val, mean_val,
    sd_val, w1_val, w2_val
  )
) %>%
  unnest(data) %>%
  rename(
    "g2" = "id_facet",
    "g1" = "id_x"
  )

sim_panel_null <- sim_panel(
  nx = nx_val,
  nfacet = nfacet_val,
  ntimes = ntimes_val,
  sim_dist = distributional
  ::dist_normal(mean_val, sd_val)
) %>%
  unnest(c(data)) %>%
  rename(
    "g2" = "id_facet",
    "g1" = "id_x"
  )


## category-plots

p_null <- sim_panel_null %>%
  ggplot(aes(x = as.factor(g1), y = sim_data)) +
  facet_wrap(~g2, labeller = "label_both") +
  geom_boxplot() +
  ylab("") +
  # ggtitle(paste("(a)", round(null, 2))) +
  xlab("") +
  theme_bw() +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  # ggtitle("design1")+
  theme_validation() +
  ylab("") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0))
  ) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

p_varf <- sim_panel_varf %>%
  ggplot(aes(x = as.factor(g1), y = sim_data)) +
  facet_wrap(~g2, labeller = "label_both") +
  geom_boxplot() +
  ylab("") +
  # ggtitle(paste("(b)", round(varf, 2))) +
  xlab("") +
  theme_bw() +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  # ggtitle("design2")+
  theme_validation() +
  ylab("") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0))
  ) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

p_varx <- sim_panel_varx %>%
  ggplot(aes(x = as.factor(g1), y = sim_data)) +
  facet_wrap(~g2, labeller = "label_both") +
  geom_boxplot() +
  ylab("") +
  # ggtitle(paste("(c)", round(varx, 2))) +
  xlab("") +
  theme_bw() +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  # ggtitle("design3")+
  theme_validation() +
  ylab("") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0))
  ) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

p_varall <- sim_panel_varall %>%
  ggplot(aes(x = as.factor(g1), y = sim_data)) +
  facet_wrap(~g2, labeller = "label_both") +
  geom_boxplot() +
  ylab("") +
  # ggtitle(paste("(d)", round(varall, 2))) +
  xlab("") +
  theme_bw() +
  stat_summary(
    fun = median,
    geom = "line",
    aes(group = 1), size = 0.8, color = "blue"
  ) +
  # ggtitle("design4") +
  theme_validation() +
  ylab("") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0))
  ) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))


change_index <- function(data) {
  index_new <- map(seq_len(ntimes_val), function(i) {
    map((seq_len(nx_val * nfacet_val)), function(j) {
      value <- i + (j - 1) * ntimes_val
    })
  }) %>% unlist()

  data_new <- data %>%
    ungroup() %>%
    mutate(
      index_old = row_number(),
      index_new = index_new
    )

  y <- data_new[match(index_new, data_new$index_old), ]

  y <- y %>%
    mutate(time = row_number())

  return(y)
}

endbreaks <- nrow(sim_panel_null)

p1 <- change_index(sim_panel_null) %>%
  ggplot(aes(
    x = time,
    y = sim_data
  )) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, 200)) +
  theme_bw() +
  # geom_point(alpha = 0.5, color = "blue")
  # theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #    panel.grid.minor.x =  element_blank())+
  ylab("") +
  xlab("")


p2 <- change_index(sim_panel_varf) %>%
  ggplot(aes(
    x = time,
    y = sim_data
  )) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, 200)) +
  theme_bw() +
  # geom_point(alpha = 0.5, color = "blue") +
  # theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #   panel.grid.minor.x =  element_blank())+
  ylab("") +
  xlab("")

p3 <- change_index(sim_panel_varx) %>%
  ggplot(aes(
    x = time,
    y = sim_data
  )) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, 200)) +
  theme_bw() +
  # geom_point(alpha = 0.5, color = "blue")+
  # theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
  #     panel.grid.minor.x =  element_blank())+
  ylab("") +
  xlab("")


p4 <- change_index(sim_panel_varall) %>%
  ggplot(aes(
    x = time,
    y = sim_data
  )) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, endbreaks, 200)) +
  theme_bw() +
  # geom_point(alpha = 0.5, color = "blue") +
  theme(
    panel.grid.major.x = element_line(colour = "#A9A9A9"),
    panel.grid.minor.x = element_blank()
  ) +
  ylab("") +
  xlab("")


p_null + p_varf + p_varx + p_varall +
  plot_annotation(tag_levels = list(c("D1", "D2", "D2", "D4")))


## ----parcoord-sim-----------------------------------------------------------------------
append_files_plot <- function(folder_name, path) {
  all_files <- list.files(
    path = paste0(folder_name, path),
    pattern = "data_validation_"
  )

  names_levels <- map_dfr(
    all_files,
    function(x) {
      z <- str_split(str_remove(x, ".rds"), "_") %>%
        unlist()
      bind_cols(index = z[3])
    }
  )

  all_files_path <- paste0(
    folder_name, path,
    all_files
  )


  all_data <- lapply(1:length(all_files_path), function(x) {
    data <- all_files_path %>%
      magrittr::extract2(x) %>%
      readRDS()

    names <- names_levels %>% magrittr::extract(x, )
    names_rep <- names %>% slice(rep(1:n(), each = nrow(data)))
    bind_cols(names_rep, data)
  }) %>% bind_rows()
  #
  # all_data <- append_files("js-nqt")

  # compute all inter cluster distance "from" group
  all_mat_mat_from <- all_data %>%
    group_by(index, gran, group_item1, group_item2) %>%
    filter(group_item1 != group_item2) %>%
    summarise(sum = sum(distance), .groups = "drop") %>%
    pivot_wider(names_from = group_item2, values_from = sum) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    mutate(distance = rowSums(across(where(is.numeric)))) %>%
    dplyr::select(c(index, gran, group_item1, distance)) %>%
    rename("group" = "group_item1")


  # compute all inter cluster distance "to" group

  all_mat_mat_to <- all_data %>%
    group_by(index, gran, group_item1, group_item2) %>%
    filter(group_item1 != group_item2) %>%
    summarise(sum = sum(distance), .groups = "drop") %>%
    pivot_wider(names_from = group_item1, values_from = sum) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    mutate(distance = rowSums(across(where(is.numeric)))) %>%
    dplyr::select(c(index, gran, group_item2, distance)) %>%
    rename("group" = "group_item2")

  # compute all inter cluster distance to and from

  data_pcp <- bind_rows(all_mat_mat_from, all_mat_mat_to) %>%
    group_by(index, gran, group) %>%
    summarise(inter_distance = sum(distance), .groups = "drop") %>%
    pivot_wider(names_from = gran, values_from = inter_distance) %>%
    ungroup() %>%
    mutate(group = as.factor(group))

  parcoord <- GGally::ggparcoord(data_pcp,
    columns = 3:ncol(data_pcp),
    groupColumn = "group",
    showPoints = TRUE,
    alphaLines = 0.5,
    scale = "globalminmax"
  ) + scale_colour_manual(values = rep("black", each = 5))
  parcoord
}
design1 <- append_files_plot(
  folder_name = "data/gracsr/validation/js-nqt",
  path = "/3gran_change_5D/"
)
design2 <- append_files_plot(
  folder_name = "data/gracsr/validation/js-nqt",
  path = "/2gran_change_4D/"
)
design3 <- append_files_plot(
  folder_name = "data/gracsr/validation/js-nqt",
  path = "/1gran_change_5D/"
)

(design1 + design2 + design3 + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "1", tag_prefix = "S", tag_suffix = "")) * theme_characterisation() * theme(plot.title = element_text(hjust = 0.5)) &
  theme(legend.position = "none") & xlab("")



## ----sindex-data-validation------------------------------------------------------------

sindex_data <- function(data) {
  total_distance <- data %>%
    group_by(item1, item2) %>%
    summarise(total = sum(distance), .groups = "drop") %>%
    ungroup()

  total_distance_wide <- total_distance %>%
    pivot_wider(names_from = item2, values_from = total)

  rownames(total_distance_wide) <- total_distance_wide$item1

  distance_lowertriangle <- total_distance_wide %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    tibble::rownames_to_column() %>%
    dplyr::select(-item1) %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from = value)

  rownames(distance_lowertriangle) <- total_distance_wide$item1

  df <- distance_lowertriangle[-1] %>% as.matrix()
  DM <- matrix(0, ncol(distance_lowertriangle), ncol(distance_lowertriangle))
  DM[lower.tri(DM)] <- df[lower.tri(df, diag = TRUE)] # distance metric
  # print(i)
  distance_matrix <- as.dist(DM)

  groups <- bind_rows(
    data %>%
      distinct(item1, group_item1) %>%
      rename(
        "item" = "item1",
        "group" = "group_item1"
      ),
    data %>%
      distinct(item2, group_item2) %>%
      rename(
        "item" = "item2",
        "group" = "group_item2"
      )
  ) %>% distinct(item, group)


  k <- array()
  for (j in 2:10)
  {
    group <- distance_matrix %>%
      hclust(method = "ward.D") %>%
      cutree(k = j)
    # print(j)
    p <- cluster.stats(distance_matrix,
      clustering = group,
      silhouette = TRUE
    )
    k[j] <- p$sindex
  }
  k %>%
    as_tibble() %>%
    set_names(c("sindex")) %>%
    mutate(k = row_number())
}


data1 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_25.rds")) %>% mutate(diff = 1, design = "S1")
data2 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_26.rds")) %>% mutate(diff = 2, design = "S1")
data3 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_27.rds")) %>% mutate(diff = 5, design = "S1")

data4 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_7.rds")) %>% mutate(diff = 1, design = "S2")
data5 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_17.rds")) %>% mutate(diff = 2, design = "S2")
data6 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_9.rds")) %>% mutate(diff = 5, design = "S2")


data7 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_16.rds")) %>% mutate(diff = 1, design = "S3")
data8 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_17.rds")) %>% mutate(diff = 2, design = "S3")
data9 <- sindex_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_18.rds")) %>% mutate(diff = 5, design = "S3")

all_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9) %>% rename("Scenario" = "design")



## ----sindex-plot-validation-------------------------------------------------------------

sindex_plot <- ggplot(
  all_data,
  aes(
    x = k,
    y = sindex
  )
) +
  geom_line(size = 1, aes(group = Scenario)) +
  facet_grid(diff ~ Scenario, labeller = "label_value", scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  xlab("number of clusters") +
  ylab("sindex") +
  scale_x_continuous(breaks = seq(2, 10, 1), minor_breaks = 1)

sindex_plot



## ----mds-data-validation----------------------------------------------------------------

mds_data <- function(data) {
  total_distance <- data %>%
    group_by(item1, item2) %>%
    summarise(total = sum(distance), .groups = "drop") %>%
    ungroup()

  total_distance_wide <- total_distance %>%
    pivot_wider(names_from = item2, values_from = total)

  rownames(total_distance_wide) <- total_distance_wide$item1

  mds_data <- total_distance_wide %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    tibble::rownames_to_column() %>%
    dplyr::select(-item1) %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from = value)

  rownames(mds_data) <- total_distance_wide$item1

  df <- mds_data[-1] %>% as.matrix()
  DM <- matrix(0, ncol(mds_data), ncol(mds_data))
  DM[lower.tri(DM)] <- df[lower.tri(df, diag = TRUE)] # distance metric
  f <- as.dist(DM)

  mds <- f %>%
    cmdscale() %>%
    as_tibble()

  colnames(mds) <- c("Dim.1", "Dim.2")

  groups <- bind_rows(
    data %>%
      distinct(item1, group_item1) %>%
      rename(
        "item" = "item1",
        "group" = "group_item1"
      ),
    data %>%
      distinct(item2, group_item2) %>%
      rename(
        "item" = "item2",
        "group" = "group_item2"
      )
  ) %>% distinct(item, group)



  all_data_cluster <- cbind(groups, mds) %>%
    mutate(group = as.factor(group)) %>%
    as_tibble()
}

# original simulation table
niter <- c(5, 50, 100) # number of series you are clustering
nT <- c(300, 1000, 5000) # length of the time series
mean_diff <- c(1, 2, 5) # difference between consecutive categories

simtable <- expand.grid(
  mean_diff = mean_diff,
  niter = niter,
  # time_series = time_series,
  nT = nT
)

data1 <- mds_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_25.rds")) %>% mutate(diff = 1, design = "S1")
data2 <- mds_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_26.rds")) %>% mutate(diff = 2, design = "S1")
data3 <- mds_data(read_rds("data/gracsr/validation/js-nqt/3gran_change_5D/data_validation_27.rds")) %>% mutate(diff = 5, design = "S1")

data4 <- mds_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_7.rds")) %>% mutate(diff = 1, design = "S2")
data5 <- mds_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_17.rds")) %>% mutate(diff = 2, design = "S2")
data6 <- mds_data(read_rds("data/gracsr/validation/js-nqt/2gran_change_4D/data_validation_9.rds")) %>% mutate(diff = 5, design = "S2")


data7 <- mds_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_16.rds")) %>% mutate(diff = 1, design = "S3")
data8 <- mds_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_17.rds")) %>% mutate(diff = 2, design = "S3")
data9 <- mds_data(read_rds("data/gracsr/validation/js-nqt/1gran_change_5D/data_validation_18.rds")) %>% mutate(diff = 5, design = "S3")

all_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9) %>% rename("Scenario" = "design")



## ----mds-plot-validation----------------------------------------------------------------

mds_plot_10 <- ggplot(
  all_data,
  aes(
    x = Dim.1,
    y = Dim.2,
    color = group
  )
) +
  geom_point(size = 2, alpha = 0.5, aes(shape = group)) +
  facet_grid(diff ~ Scenario, labeller = "label_value") +
  # geom_text(check_overlap = TRUE)  +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  ylab("mds1") +
  xlab("mds2") +
  theme(axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(aspect.ratio = 1)+
  scale_shape_manual(values = c(3, 4, 2, 0, 1)) + 
  theme(panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0.3, "lines"))

mds_plot_10


##----wpd-sindex-validation----------------------------------------
data1 <- sindex_data(read_rds("validation/wpd/3gran_change_5D/data_validation_25.rds")) %>% mutate(diff = 1, design = "S1")
data2 <- sindex_data(read_rds("validation/wpd/3gran_change_5D/data_validation_26.rds")) %>% mutate(diff = 2, design = "S1")
data3 <- sindex_data(read_rds("validation/wpd/3gran_change_5D/data_validation_27.rds")) %>% mutate(diff = 5, design = "S1")

data4 <- sindex_data(read_rds("validation/wpd/2gran_change_4D/data_validation_7.rds")) %>% mutate(diff = 1, design = "S2")
data5 <- sindex_data(read_rds("validation/wpd/2gran_change_4D/data_validation_17.rds")) %>% mutate(diff = 2, design = "S2")
data6 <- sindex_data(read_rds("validation/wpd/2gran_change_4D/data_validation_9.rds")) %>% mutate(diff = 5, design = "S2")


data7 <- sindex_data(read_rds("validation/wpd/1gran_change_5D/data_validation_16.rds")) %>% mutate(diff = 1, design = "S3")
data8 <- sindex_data(read_rds("validation/wpd/1gran_change_5D/data_validation_17.rds")) %>% mutate(diff = 2, design = "S3")
data9 <- sindex_data(read_rds("validation/wpd/1gran_change_5D/data_validation_18.rds")) %>% mutate(diff = 5, design = "S3")

all_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9) %>% rename("Scenario" = "design")



## ----sindex-plot-validation-wpd-------------------------------------------------------------

sindex_plot <- ggplot(
  all_data,
  aes(
    x = k,
    y = sindex
  )
) +
  geom_line(size = 1, aes(group = Scenario)) +
  facet_grid(diff ~ Scenario, labeller = "label_value", scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  xlab("number of clusters") +
  ylab("sindex") +
  scale_x_continuous(breaks = seq(2, 10, 1), minor_breaks = 1)

sindex_plot
