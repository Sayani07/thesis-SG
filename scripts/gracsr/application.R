##### application

## ----prototype-data-pick-------------------------------------------------------
quantile_prob_graph <- c(0.25, 0.5, 0.75)

data_pick_one <- c(8541744, 9355808, 8603880, 8619309, 10542667) %>%
  as_tibble() %>%
  set_names("customer_id")
# data_pick_two <- c(8688242, 8643837, 8184707, 10534355, 8684420) %>% as_tibble%>% set_names("customer_id")
data_pick_three <- c(9792072, 8589936, 8454235, 10692366, 8603828) %>%
  as_tibble() %>%
  set_names("customer_id")
data_pick_four <- c(8618759, 8291696, 10357256, 8290374) %>%
  as_tibble() %>%
  set_names("customer_id")
data_pick_five <- c(9044864, 8642053, 10534367, 9021526, 11162275) %>%
  as_tibble() %>%
  set_names("customer_id")
data_pick_six <- c(8221762, 8273636, 10359424, 8232822, 11450499) %>%
  as_tibble() %>%
  set_names("customer_id")



## ----assemble------------------------------------------------------------------
data_pick_cust <- bind_rows(
  data_pick_one,
  # data_pick_two,
  data_pick_three,
  data_pick_four,
  data_pick_five,
  data_pick_six,
  .id = "design"
) %>%
  mutate(customer_id = as.character(customer_id))

## ----all-data-----------------------------------------------------------------
data_pick <- read_rds(here::here("data/gracsr/elec_nogap_2013_clean_356cust.rds")) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  dplyr::filter(customer_id %in% data_pick_cust$customer_id) %>%
  gracsr::scale_gran(
    method = "nqt",
    response = "general_supply_kwh"
  )



## ----clustering---------------------------------------------------------------
hod <- suppressMessages(data_pick %>%
  dist_gran(
    gran1 = "hour_day",
    response = "general_supply_kwh"
  ))

moy <- suppressMessages(data_pick %>%
  dist_gran(
    gran1 = "month_year",
    response = "general_supply_kwh"
  ))

wkndwday <- suppressMessages(data_pick %>%
  dist_gran(gran1 = "wknd_wday", response = "general_supply_kwh"))

distance <- wkndwday / 2 + moy / 12 + hod / 24

distance_jsd <- as.dist(distance)



## ----opt-clusters-jsd---------------------------------------------------------

all_index <- map_dfr(2:10, function(x) {
  group <- distance_jsd %>%
    hclust(method = "ward.D") %>%
    cutree(k = x)
  p <- cluster.stats(distance_jsd, clustering = group, silhouette = TRUE, wgap = TRUE)
  index <- c(
    k = x,
    sindex = p$sindex
  )
})

opt_clusters <- all_index %>% ggplot(aes(
  x = k,
  y = sindex
)) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(2, 10, 1),
    minor_breaks = 1
  ) +
  theme_bw() +
  ylab("sindex") +
  xlab("number of clusters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))



## ----groups-24----------------------------------------------------------------
# clustering customers with 5 clusters
cluster_result <- suppressMessages(distance_jsd %>%
  clust_gran(kopt = 5)) %>%
  rename("customer_id" = "id") %>%
  mutate(group = as.factor(group))

cluster_result_id <- cluster_result %>%
  arrange(group) %>%
  mutate(divide_cust = rep(c(1, 2), each = 12)) %>%
  mutate(id = row_number())




## ----data-pick-robust----------------------------------------------------------------
# For visualization, robust scaling done on each customer
data_pick_robust <- read_rds(here::here("data/gracsr/elec_nogap_2013_clean_356cust.rds")) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  dplyr::filter(customer_id %in% data_pick_cust$customer_id) %>%
  gracsr::scale_gran(
    method = "robust",
    response = "general_supply_kwh"
  )


## ----hod-moy-wkndwday-data---------------------------------------------------

data_hod <- quantile_gran(data_pick_robust,
  "hour_day",
  quantile_prob_val = quantile_prob_graph
) %>%
  pivot_wider(
    names_from = quantiles,
    values_from = quantiles_values
  ) %>%
  left_join(data_pick_cust, by = c("customer_id"))


data_hod$category <- factor(data_hod$category, levels = 0:23)

data_moy <- quantile_gran(data_pick_robust,
  "month_year",
  quantile_prob_val = quantile_prob_graph
) %>%
  pivot_wider(
    names_from = quantiles,
    values_from = quantiles_values
  ) %>%
  left_join(data_pick_cust, by = c("customer_id"))

data_moy$category <- factor(data_moy$category,
  levels = c(
    "Jan", "Feb", "Mar", "Apr",
    "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"
  )
)

data_wkndwday <- data_pick_robust %>%
  create_gran("wknd_wday") %>%
  left_join(data_pick_cust, by = c("customer_id"))

ylim1 <- boxplot.stats(data_wkndwday$general_supply_kwh)$stats[c(1, 5)]



## ----hod-ind-group------------------------------------------------------------


hod_ind_group1 <- data_hod %>%
  left_join(cluster_result_id, by = c("customer_id")) %>%
  filter(divide_cust == 1) %>%
  mutate(label = if_else(category == first(category),
    as.character(id), NA_character_
  )) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = customer_id
  ),
  alpha = 0.5
  ) +
  geom_line(aes(
    y = `50%`,
    group = customer_id
  ), size = 1) +
  facet_wrap(~id,
    scales = "free_y",
    ncol = 1
  ) +
  theme_application2() +
  scale_fill_manual(values = c(
    "#E69F00", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  xlab("hod") +
  scale_x_discrete(breaks = seq(0, 23, 3)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(0, 0, 0, -1), "cm")) +
  ggrepel::geom_label_repel(aes(label = label, y = `50%`),
    nudge_x = -1,
    na.rm = TRUE, max.overlaps = 12
  )



hod_ind_group2 <- data_hod %>%
  left_join(cluster_result_id, by = c("customer_id")) %>%
  filter(divide_cust == 2) %>%
  mutate(label = if_else(category == first(category),
    as.character(id), NA_character_
  )) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = customer_id
  ),
  alpha = 0.5
  ) +
  geom_line(aes(
    y = `50%`,
    group = customer_id
  ), size = 1) +
  facet_wrap(~id,
    scales = "free_y",
    ncol = 1
  ) +
  theme_application2() +
  scale_fill_manual(values = c(
    "#E69F00", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  xlab("hod") +
  scale_x_discrete(breaks = seq(0, 23, 3)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(0, 0, 0, -1), "cm")) +
  ggrepel::geom_label_repel(aes(label = label, y = `50%`),
    nudge_x = -1,
    na.rm = TRUE
  )


moy_ind_group1 <- data_moy %>%
  left_join(cluster_result_id, by = c("customer_id")) %>%
  filter(divide_cust == 1) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = customer_id
  ), alpha = 0.5) +
  geom_line(aes(y = `50%`, group = customer_id), size = 1) +
  facet_wrap(~id,
    scales = "free_y",
    ncol = 1
  ) +
  ylab("demand (in Kwh)") +
  xlab("moy") +
  theme_application2() +
  scale_fill_manual(values = c(
    "#E69F00", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(0, -1, 0, -1), "cm"))


moy_ind_group2 <- data_moy %>%
  left_join(cluster_result_id, by = c("customer_id")) %>%
  filter(divide_cust == 2) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = customer_id
  ), alpha = 0.5) +
  geom_line(aes(y = `50%`, group = customer_id), size = 1) +
  facet_wrap(~id,
    scales = "free_y",
    ncol = 1
  ) +
  ylab("demand (in Kwh)") +
  xlab("moy") +
  theme_application2() +
  scale_fill_manual(values = c(
    "#E69F00", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(0, -1, 0, -1), "cm"))



wkndwday_ind_group1 <- data_wkndwday %>%
  left_join(cluster_result_id, by = c("customer_id")) %>%
  filter(divide_cust == 1) %>%
  mutate(divide_cust = as.factor(divide_cust)) %>%
  ggplot(aes(x = wknd_wday, y = general_supply_kwh)) +
  # lvplot::geom_lv(aes(fill = as.factor(group),
  #            color = as.factor(group)), k=5, alpha = 0.3) +
  geom_boxplot(aes(fill = divide_cust), alpha = 0.5) +
  # geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = ylim1 * 1.05) +
  facet_wrap(~id,
    scales = "free_y",
    labeller = "label_value",
    ncol = 1
  ) +
  theme_application2() +
  scale_fill_manual(values = c(
    "black", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  theme(legend.position = "none") +
  xlab("wnwd") +
  theme(plot.margin = unit(c(0, -1, 0, 0), "cm"))

wkndwday_ind_group2 <- data_wkndwday %>%
  left_join(cluster_result_id, by = c("customer_id")) %>%
  filter(divide_cust == 2) %>%
  mutate(divide_cust = as.factor(divide_cust)) %>%
  ggplot(aes(x = wknd_wday, y = general_supply_kwh)) +
  # lvplot::geom_lv(aes(fill = as.factor(group),
  #            color = as.factor(group)), k=5, alpha = 0.3) +
  geom_boxplot(aes(fill = divide_cust), alpha = 0.5) +
  # geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = ylim1 * 1.05) +
  facet_wrap(~id,
    scales = "free_y",
    labeller = "label_value",
    ncol = 1
  ) +
  theme_application2() +
  scale_fill_manual(values = c(
    "black", "#009E73", "#0072B2",
    "#D55E00", "#CC79A7"
  )) +
  theme(legend.position = "none") +
  xlab("wnwd") +
  theme(plot.margin = unit(c(0, -1, 0, 0), "cm"))


cust_div1 <- hod_ind_group1 + moy_ind_group1 + wkndwday_ind_group1
cust_div2 <- hod_ind_group2 + moy_ind_group2 + wkndwday_ind_group2


plot <- ggpubr::ggarrange(cust_div1, cust_div2,
  ncol = 2,
  labels = c("a", "b"), hjust = -1
)



##----hod-ind-group-png----------------------------------------------------
#ggsave("figs/ind-groups-24.png")
knitr::include_graphics("img/ind-groups-24.png")
#plot



## ----groups-4and5-------------------------------------------------------------

cluster_result_kopt4 <- suppressMessages(distance_jsd %>%
  clust_gran(kopt = 4)) %>%
  rename("customer_id" = "id") %>%
  mutate(group = as.factor(group))

cluster_result_id4and5 <- cluster_result_id %>%
  left_join(cluster_result_kopt4, by = "customer_id") %>%
  rename("group4" = "group.y") %>%
  rename("group5" = "group.x")

# data-heatmap-hod-group-new-4

data_group <- data_pick_robust %>%
  left_join(cluster_result_id4and5, by = c("customer_id"))

data_heatmap_hod_group <- quantile_gran(data_group,
  gran1 = "hour_day",
  quantile_prob_val = c(0.25, 0.5, 0.75),
  group = "group4"
) %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)


data_heatmap_hod_group$category <- factor(data_heatmap_hod_group$category,
  levels = 0:23
)

data_heatmap_hod_group$group4 <- paste("A", data_heatmap_hod_group$group4,
  sep = "-"
)

hod_group <- data_heatmap_hod_group %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = group4,
    fill = as.factor(group4), alpha = 0.5
  ),
  alpha = 0.5
  ) +
  geom_line(aes(
    y = `50%`,
    group = group4,
    color = as.factor(group4)
  ), size = 1) +
  facet_wrap(~group4,
    scales = "free_y",
    nrow = 5, labeller = "label_value"
  ) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("hod") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  scale_x_discrete(breaks = seq(1, 24, 3)) +
  theme_application3() +
  scale_fill_manual(values = as.vector(tableau20(20))) +
  scale_color_manual(values = as.vector(tableau20(20))) +
  theme(legend.position = "bottom")

# data-heatmap-moy-group-new-4
data_heatmap_moy_group <- quantile_gran(data_group,
  gran1 = "month_year",
  quantile_prob_val = c(0.25, 0.5, 0.75),
  group = "group4"
) %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)

data_heatmap_moy_group$category <- factor(data_heatmap_moy_group$category,
  levels = c(
    "Jan", "Feb", "Mar", "Apr",
    "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"
  )
)


data_heatmap_moy_group$group4 <- paste("A", data_heatmap_moy_group$group4,
  sep = "-"
)


moy_group <- data_heatmap_moy_group %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`, group = group4, fill = as.factor(group4)
  ), alpha = 0.5) +
  geom_line(aes(y = `50%`, group = group4, color = as.factor(group4)),
    size = 1
  ) +
  facet_wrap(~group4,
    scales = "free_y",
    labeller = "label_value",
    nrow = 5
  ) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("moy") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  theme_application3() +
  scale_fill_manual(values = as.vector(tableau20(20))) +
  scale_color_manual(values = as.vector(tableau20(20))) +
  theme(legend.position = "bottom")

# data-heatmap-wkndwday-group-4
wkndwday_data <- data_group %>%
  create_gran("wknd_wday") %>%
  create_gran("hour_day")

ylim1 <- boxplot.stats(wkndwday_data$general_supply_kwh)$stats[c(1, 5)]

wkndwday_data$group4 <- paste("A", wkndwday_data$group4, sep = "-")


wkndwday_group <- wkndwday_data %>%
  ggplot(aes(x = wknd_wday, y = general_supply_kwh)) +
  geom_boxplot(aes(fill = group4, color = group4),
    alpha = 0.5,
    outlier.alpha = 0.05
  ) +
  coord_cartesian(ylim = ylim1 * 1.05) +
  xlab("wnwd") +
  ylab("demand (in Kwh)") +
  facet_wrap(~group4,
    ncol = 1,
    scales = "free_y",
    labeller = "label_value"
  ) +
  theme_bw() +
  scale_fill_manual(values = as.vector(tableau20(10))) +
  scale_color_manual(values = as.vector(tableau20(10))) +
  theme(legend.position = "none") +
  theme_application3()

# combined-groups-js-4s
combined_groups_js4 <- (hod_group + moy_group + wkndwday_group) +
  plot_layout(guides = "collect") & theme(legend.position = "none")


# data-heatmap-hod-group-5

data_group <- data_pick_robust %>%
  left_join(cluster_result_id4and5, by = c("customer_id"))

data_heatmap_hod_group <- quantile_gran(data_group,
  gran1 = "hour_day",
  quantile_prob_val = c(0.25, 0.5, 0.75),
  group = "group5"
) %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)


data_heatmap_hod_group$category <- factor(data_heatmap_hod_group$category,
  levels = 0:23
)

data_heatmap_hod_group$group5 <- paste("B", data_heatmap_hod_group$group5,
  sep = "-"
)

hod_group <- data_heatmap_hod_group %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = group5,
    fill = as.factor(group5), alpha = 0.5
  ),
  alpha = 0.5
  ) +
  geom_line(aes(
    y = `50%`,
    group = group5,
    color = as.factor(group5)
  ), size = 1) +
  facet_wrap(~group5,
    scales = "free_y",
    nrow = 5, labeller = "label_value"
  ) +
  xlab("hod") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  scale_x_discrete(breaks = seq(1, 24, 3)) +
  theme_application3() +
  scale_fill_manual(values = c(
    "#E69F00", "#009E73", "#999999",
    "#D55E00", "#CC79A7"
  )) +
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#999999",
    "#D55E00", "#CC79A7"
  )) +
  theme(legend.position = "bottom")

# data-heatmap-moy-group-new-5
data_heatmap_moy_group <- quantile_gran(data_group,
  gran1 = "month_year",
  quantile_prob_val = c(0.25, 0.5, 0.75),
  group = "group5"
) %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)

data_heatmap_moy_group$category <- factor(data_heatmap_moy_group$category,
  levels = c(
    "Jan", "Feb", "Mar", "Apr",
    "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"
  )
)


data_heatmap_moy_group$group5 <- paste("B", data_heatmap_moy_group$group5,
  sep = "-"
)


moy_group <- data_heatmap_moy_group %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`, group = group5, fill = as.factor(group5)
  ), alpha = 0.5) +
  geom_line(aes(y = `50%`, group = group5, color = as.factor(group5)), size = 1) +
  facet_wrap(~group5,
    scales = "free_y",
    labeller = "label_value",
    nrow = 5
  ) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("moy") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  theme_application3() +
  scale_fill_manual(values = c(
    "#E69F00", "#009E73", "#999999",
    "#D55E00", "#CC79A7"
  )) +
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#999999",
    "#D55E00", "#CC79A7"
  )) +
  theme(legend.position = "bottom")

# data-heatmap-wkndwday-group-4and5
wkndwday_data <- data_group %>%
  create_gran("wknd_wday") %>%
  create_gran("hour_day")

ylim1 <- boxplot.stats(wkndwday_data$general_supply_kwh)$stats[c(1, 5)]

wkndwday_data$group5 <- paste("B", wkndwday_data$group5, sep = "-")


wkndwday_group <- wkndwday_data %>%
  ggplot(aes(x = wknd_wday, y = general_supply_kwh)) +
  geom_boxplot(aes(fill = group5, color = group5),
    alpha = 0.5,
    outlier.alpha = 0.05
  ) +
  coord_cartesian(ylim = ylim1 * 1.05) +
  xlab("wnwd") +
  ylab("demand (in Kwh)") +
  facet_wrap(~group5,
    ncol = 1,
    scales = "free_y",
    labeller = "label_value"
  ) +
  theme_bw() +
  scale_fill_manual(values = c(
    "#E69F00", "#009E73", "#999999",
    "#D55E00", "#CC79A7"
  )) +
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#999999",
    "#D55E00", "#CC79A7"
  )) +
  theme(legend.position = "none") +
  theme_application3()

# combined-groups-js-4and5
combined_groups_js5 <- (hod_group + moy_group + wkndwday_group) +
  plot_layout(guides = "collect") & theme(legend.position = "none")


ggpubr::ggarrange(combined_groups_js4, combined_groups_js5,
  labels = c("a", "b"), hjust = 0
)



### wpd clustering starts
## ----data-pick-wpd------------------------------------------------------------
elec_600_wpd <- read_rds(here::here("data/gracsr/algo2-cust600-wpd-rawdata.rds"))

elec_pick <- elec_600_wpd %>%
  filter(customer_id %in% data_pick_cust$customer_id)

elec_pick_wide <- elec_pick %>% pivot_wider(-c(1, 2), names_from = "x_variable", values_from = wpd)

scaled_var <- elec_pick_wide

distance_wpd <- elec_pick_wide[-1] %>% dist()


all_index <- map_dfr(2:10, function(x) {
  group <- distance_wpd %>%
    hclust(method = "ward.D") %>%
    cutree(k = x)
  p <- cluster.stats(distance_wpd, clustering = group, silhouette = TRUE)
  index <- c(k = x, sindex = p$sindex)
})

opt_clusters_wpd <- all_index %>% ggplot(aes(x = k, y = sindex)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2, 10, 1), minor_breaks = 1) +
  theme_bw() +
  ylab("sindex") +
  xlab("number of clusters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))



## ----wpd-clustering-----------------------------------------------------------
group <- distance_wpd %>%
  hclust(method = "ward.D") %>%
  cutree(k = 4)


cluster_result_wpd <- bind_cols(id = elec_pick_wide$customer_id, group = group)

data_pcp <- scaled_var %>%
  # bind_cols(customer_id =  elec_pick_wide$customer_id) %>%
  left_join(cluster_result_wpd, by = c("customer_id" = "id")) %>%
  select(customer_id, group, everything()) %>%
  mutate(group = as.factor(group)) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  rename(
    "moy" = "month_year",
    "hod" = "hour_day",
    "wnwd" = "wknd_wday"
  )

data_table <- data_pcp %>%
  group_by(group) %>%
  summarise(
    nobs = n(),
    moy = round(median(moy), 2),
    hod = round(median(hod), 2),
    wnwd = round(median(wnwd), 2)
  ) %>%
  select(-group)

rownames(data_table) <- c("group-1", "group-2", "group-3", "group-4")



## ----parcoord-application-gracsr-----------------------------------------------------
parcoord <- GGally::ggparcoord(data_pcp %>% left_join(cluster_result_id, by = "customer_id"),
  columns = 3:5,
  groupColumn = "group.x",
  showPoints = FALSE,
  alphaLines = 0.8,
  order = "anyClass",
  scale = "globalminmax"
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 10)) +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("wpd") + scale_fill_viridis_d(direction = 1) +
  scale_color_viridis_d(direction = 1) + theme_light() +
  labs(colour = "group") +
  theme(panel.border = element_blank())


parcoord + geom_text(aes(label = id)) +
  inset_element(gridExtra::tableGrob(data_table),
    left = 0.4, bottom = 0.6,
    right = 1, top = 1
  ) & theme(legend.position = "bottom")



## ----tsne-fit-----------------------------------------------------------------

data_356cust_hod <- read_rds("data/gracsr/quantile_data_356cust_hod_robust.rds") %>%
  filter(quantiles %in% "50%")

data_356cust_moy <- read_rds("data/gracsr/quantile_data_356cust_moy_robust.rds") %>%
  filter(quantiles %in% "50%")

data_356cust_wkndwday <- read_rds("data/gracsr/quantile_data_356cust_wkndwday_robust.rds") %>%
  filter(quantiles %in% "50%")


data_356cust_hod_wide <- data_356cust_hod %>%
  pivot_wider(
    names_from = c("gran", "category", "quantiles"),
    values_from = "quantiles_values"
  )

data_356cust_moy_wide <- data_356cust_moy %>%
  pivot_wider(
    names_from = c("gran", "category", "quantiles"),
    values_from = "quantiles_values"
  )

data_356cust_wkndwday_wide <- data_356cust_wkndwday %>%
  pivot_wider(
    names_from = c("gran", "category", "quantiles"),
    values_from = "quantiles_values"
  )

data_356cust_wide <- left_join(data_356cust_hod_wide,
  data_356cust_moy_wide,
  by = "customer_id"
) %>%
  left_join(data_356cust_wkndwday_wide, by = "customer_id")


data_24cust_wide <- data_356cust_wide %>%
  filter(customer_id %in% data_pick$customer_id)

set.seed(2935)

tSNE_fit <- data_24cust_wide %>%
  select(-customer_id) %>%
  Rtsne(pca = FALSE, perplexity = 2)



## ----tsne-df------------------------------------------------------------------

tsne_df <- data.frame(
  tsneX = tSNE_fit$Y[, 1],
  tsneY = tSNE_fit$Y[, 2],
  customer_id = as.character(data_24cust_wide$customer_id)
) %>%
  left_join(cluster_result_id, by = c("customer_id"))



## ----tsne-xy------------------------------------------------------------------
tsne_xy <- ggplot(tsne_df, aes(x = tsneX, y = tsneY)) +
  geom_point(size = 2, shape = ".") +
  ggrepel::geom_text_repel(aes(label = id), size = 2, seed = 2935, max.overlaps = 10) +
  # scale_color_manual(values = limn_pal_tableau10()) +
  scale_colour_viridis_d(direction = -1) +
  # guides(color = FALSE) +
  # labs(caption = "tSNE") +
  theme_light() +
  theme(aspect.ratio = 1)



## ----opt-cluster-tsne-jsd-----------------------------------------------------

tsne_xy + (opt_clusters / opt_clusters_wpd) +
  plot_annotation(tag_levels = list(c("", "JS", "wpd")))
