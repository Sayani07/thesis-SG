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
data_pick <- readr::read_rds(here::here("data/gracsr/elec_nogap_2013_clean_356cust.rds")) %>%
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

all_index_nqt <- map_dfr(2:10, function(x) {
  group <- distance_jsd %>%
    hclust(method = "ward.D") %>%
    cutree(k = x)
  p <- cluster.stats(distance_jsd, clustering = group, silhouette = TRUE, wgap = TRUE)
  index <- c(
    k = x,
    sindex = p$sindex
  )
})%>% mutate(method = "JS-NQT")

# 
# opt_clusters <- all_index %>% ggplot(aes(
#   x = k,
#   y = sindex
# )) +
#   geom_line() +
#   scale_x_continuous(
#     breaks = seq(2, 10, 1),
#     minor_breaks = 1
#   ) +
#   theme_bw() +
#   ylab("sindex") +
#   xlab("number of clusters") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
# 


## ----groups-24----------------------------------------------------------------
# clustering customers with 5 clusters
cluster_result <- suppressMessages(distance_jsd %>%
  clust_gran(kopt = 5)) %>%
  rename("customer_id" = "id") %>%
  mutate(group = as.factor(group))

cluster_result_id <- cluster_result %>%
  arrange(group) %>%
  mutate(divide_cust = rep(c(1, 2), each = 12)) %>%
  mutate(sort_group_id = row_number()) %>% 
  left_join(data_pick_cust, by = c("customer_id")) %>% 
  mutate(id = paste(sort_group_id, design, sep = "-"))


label_factor <- cluster_result_id$id %>% unique()

cluster_result_id$id = factor(cluster_result_id$id, levels = label_factor)



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
    "#A89030", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
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
    "#A89030", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
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
    "#A89030", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
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
    "#A89030", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
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
    "black", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
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
    "black", "#5D7050", "#0072B2",
    "#125070", "#50A6C2"
  )) +
  theme(legend.position = "none") +
  xlab("wnwd") +
  theme(plot.margin = unit(c(0, -1, 0, 0), "cm"))


cust_div1 <- hod_ind_group1 + moy_ind_group1 + wkndwday_ind_group1
cust_div2 <- hod_ind_group2 + moy_ind_group2 + wkndwday_ind_group2


plot <- ggpubr::ggarrange(cust_div1, cust_div2,
  ncol = 2, hjust = -1
)



##----hod-ind-group-png----------------------------------------------------
#ggsave("img/ind-groups-24-design.png")
knitr::include_graphics("img/ind-groups-24-design.png")
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

cluster_result_id4and5$group4 <- fct_recode(cluster_result_id4and5$group4, "1" = "4", "4"="1")

cluster_result_id4and5$group5 <- fct_recode(cluster_result_id4and5$group5, "1" = "5", "5" = "1")


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
  scale_fill_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070"
  ))+
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
  scale_fill_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070"
  ))+
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
  scale_fill_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070"
  ))+
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
    "#A89030", "#5D7050", "#481800",
    "#50A6C2", "#84C0D4"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050", "#481800",
    "#50A6C2", "#84C0D4"
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
    "#A89030", "#5D7050", "#481800",
    "#50A6C2", "#84C0D4"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050", "#481800",
    "#50A6C2", "#84C0D4"
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
    "#A89030", "#5D7050", "#481800",
    "#50A6C2", "#84C0D4"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050", "#481800",
     "#50A6C2", "#84C0D4"
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

distance_wpd <- elec_pick_wide[-1] %>% scale() %>%  dist()


## ----opt_clusters-wpd------------------------------------------------------------
all_index_wpd <- map_dfr(2:10, function(x) {
  group <- distance_wpd %>%
    hclust(method = "ward.D") %>%
    cutree(k = x)
  p <- cluster.stats(distance_wpd, clustering = group, silhouette = TRUE)
  index <- c(k = x, sindex = p$sindex)
})%>% mutate(method = "WPD")


opt_clusters_wpd <- all_index_wpd %>% ggplot(aes(x = k, y = sindex)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2, 10, 1), minor_breaks = 1) +
  theme_bw() +
  ylab("sindex") +
  xlab("number of clusters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

## ----summary-group-wpd------------------------------------------------------------

group3 <- distance_wpd %>%
  hclust(method = "ward.D") %>%
  cutree(k = 3)


cluster_result_wpd3 <- bind_cols(id = elec_pick_wide$customer_id, 
                                 group3 = group3) %>%
  mutate(group3 = as.factor(group3))


group5 <- distance_wpd %>%
  hclust(method = "ward.D") %>%
  cutree(k = 5)


cluster_result_wpd5 <- bind_cols(id = elec_pick_wide$customer_id,
                                 group5 = group5)%>%
  mutate(group5 = as.factor(group5))


cluster_result_wpd <- cluster_result_wpd3 %>% left_join(cluster_result_wpd5, by = "id")

cluster_result_wpd$group5 <- fct_recode(cluster_result_wpd$group5, "1" = "5", "5"="1")

cluster_result_wpd$group3 <- fct_recode(cluster_result_wpd$group3, "3" = "1", "1"="3")

data_pcp <- scaled_var %>%
  # bind_cols(customer_id =  elec_pick_wide$customer_id) %>%
  left_join(cluster_result_wpd, by = c("customer_id" = "id")) %>%
  select(customer_id, group3, everything()) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  rename(
    "moy" = "month_year",
    "hod" = "hour_day",
    "wnwd" = "wknd_wday"
  ) %>% 
  select(customer_id, group3, group5, everything())

data_pcp_plot <- data_pcp


## ----summary-table-wpd------------------------------------------------------------

data_pcp$group5 <- paste("Q", data_pcp$group5 ,
                         sep = "-"
)
data_pcp$group3 <- paste("P", data_pcp$group3 ,
                         sep = "-"
)

data_table3 <- data_pcp %>%
  group_by(group3) %>%
  summarise(
    nobs = n(),
    moy = round(median(moy), 1),
    hod = round(median(hod), 1),
    wnwd = round(median(wnwd), 1)
  ) %>% mutate(k = 3) %>% 
  rename("group" = "group3")

data_table5 <- data_pcp %>%
  group_by(group5) %>%
  summarise(
    nobs = n(),
    moy = round(median(moy), 1),
    hod = round(median(hod), 1),
    wnwd = round(median(wnwd), 1)
  ) %>% mutate(k = 5)%>% 
  rename("group" = "group5")

data_pcp_id <- data_pcp %>% left_join(cluster_result_id, by = "customer_id")

p1 <- data_pcp_id %>% filter(group3 =="P-1") %>% pull(sort_group_id) %>% paste(collapse = ", ")

p2 <- data_pcp_id %>% filter(group3 =="P-2") %>% pull(sort_group_id) %>% paste(collapse = ", ")

p3 <- data_pcp_id %>% filter(group3 =="P-3") %>% pull(sort_group_id) %>% paste(collapse = ", ")

q1 <- data_pcp_id %>% filter(group5 =="Q-1") %>% pull(sort_group_id) %>% paste(collapse = ", ")

q2 <- data_pcp_id %>% filter(group5 =="Q-2") %>% pull(sort_group_id) %>% paste(collapse = ", ")

q3 <- data_pcp_id %>% filter(group5 == "Q-3") %>% pull(sort_group_id) %>% paste(collapse = ", ")

q4 <- data_pcp_id %>% filter(group5 == "Q-4") %>% pull(sort_group_id) %>% paste(collapse = ", ")

q5 <- data_pcp_id %>% filter(group5 =="Q-5") %>% pull(sort_group_id) %>% paste(collapse = ", ")

data_table <- bind_rows(data_table3,
                        data_table5) %>%
  bind_cols(`customer-prototype id` = c(p1, p2, p3, q1, q2, q3, q4, q5)) %>% 
  select(k, group, everything()) %>% 
  bind_cols()

data_table %>% 
knitr::kable(format = "latex",
             escape = FALSE,
             caption = "Summary table from WPD clusters showing median $wpd$ values ($moy$, $hod$, $wnwd$), cluster size ($nobs$) and the list of the customer-prototype id for each cluster with $3$ and $5$ number of clusters ($k$).")%>% 
  kableExtra::collapse_rows(columns = 1)

## ----summary-plot-wpd------------------------------------------------------------

parcoord5 <- GGally::ggparcoord(data_pcp_plot %>% select(-group3)%>% left_join(cluster_result_id, by = "customer_id"),
                                columns = 3:5,
                                groupColumn = "group5",
                                showPoints = FALSE,
                                alphaLines = 0.8,
                                order = "anyClass",
                                scale = "std"
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 10)) +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("wpd") +
  labs(colour = "group") +
  theme(panel.border = element_blank()) +
  scale_color_discrete(labels = c(c("Q-1","Q-2", "Q-3", "Q-4", "Q-5")))+
  scale_color_manual(values = c("#3300FF", "#1B9E77", "#3B3178", "#1D7CF2", "#AE6D1C"), labels = c("Q-5","Q-2", "Q-3", "Q-4", "Q-1"))



parcoord3 <- GGally::ggparcoord(data_pcp_plot %>% select(-group5)%>% left_join(cluster_result_id, by = "customer_id"),
                                columns = 3:5,
                                groupColumn = "group3",
                                showPoints = FALSE,
                                alphaLines = 1,
                                order = "anyClass",
                                scale = "std"
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 10)) +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("wpd") +
  labs(colour = "group") +
  theme(panel.border = element_blank()) +
  scale_color_manual(values = c("#D8367D", "#1B9E77","#AE6D1C"), labels = c("P-3","P-2", "P-1"))


parcoord3 <- parcoord3 +geom_text_repel(aes(label = sort_group_id), size = 2) &theme_light()&theme(legend.position = "bottom") 


parcoord5 <- parcoord5 +geom_text_repel(aes(label = sort_group_id), size = 2) &theme_light()&theme(legend.position = "bottom") 

(parcoord3 + ylab("") + parcoord5 + ylab("")) + plot_annotation(tag_levels = "a")+ plot_layout(guides = "collect")&theme(legend.position = "bottom")&theme(legend.title=element_blank())
  

#&theme_application2() + plot_annotation(tag_levels = "a")



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
geom_text(aes(label = sort_group_id), size = 2)+
#, seed = 2935, max.overlaps = 10) +
  # scale_color_manual(values = limn_pal_tableau10()) +
  scale_colour_viridis_d(direction = -1) +
  # guides(color = FALSE) +
  # labs(caption = "tSNE") +
  theme_light() +
  theme(aspect.ratio = 1)


## ----all-data-rs-----------------------------------------------------------------
data_pick_rs <- readr::read_rds(here::here("data/gracsr/elec_nogap_2013_clean_356cust.rds")) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  dplyr::filter(customer_id %in% data_pick_cust$customer_id) %>%
  gracsr::scale_gran(
    method = "robust",
    response = "general_supply_kwh"
  )



## ----clustering-rs---------------------------------------------------------------
hod_rs <- suppressMessages(data_pick_rs %>%
                          dist_gran(
                            gran1 = "hour_day",
                            response = "general_supply_kwh"
                          ))

moy_rs <- suppressMessages(data_pick_rs %>%
                          dist_gran(
                            gran1 = "month_year",
                            response = "general_supply_kwh"
                          ))

wkndwday_rs <- suppressMessages(data_pick_rs %>%
                               dist_gran(gran1 = "wknd_wday", response = "general_supply_kwh"))

distance_rs <- wkndwday_rs / 2 + moy_rs / 12 + hod_rs / 24

distance_jsd_rs <- as.dist(distance_rs)



## ----opt-clusters-jsd-rs---------------------------------------------------------

all_index_rs <- map_dfr(2:10, function(x) {
  group <- distance_jsd_rs %>%
    hclust(method = "ward.D") %>%
    cutree(k = x)
  p <- cluster.stats(distance_jsd_rs, clustering = group, silhouette = TRUE, wgap = TRUE)
  index <- c(
    k = x,
    sindex = p$sindex
  )
}) %>% mutate(method = "JS-RS")




## ----opt-clusters-all---------------------------------------------------------

all_index <- bind_rows(all_index_rs, all_index_nqt, all_index_wpd)


opt_clusters <- all_index %>% ggplot(aes(
  x = k,
  y = sindex
)) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(2, 10, 1),
    minor_breaks = 1
  ) +
  facet_wrap(~method, scales = "free_y", ncol = 1)+
  theme_bw() +
  ylab("sindex") +
  xlab("number of clusters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))




## ----opt-cluster-tsne-jsd-----------------------------------------------------

tsne_xy + opt_clusters + plot_annotation(tag_levels = "a")
  #(opt_clusters / opt_clusters_wpd) +
  #plot_annotation(tag_levels = list(c("", "JS", "wpd")))
