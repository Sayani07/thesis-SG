## ----raw-data-50

knitr::include_graphics("figs/raw_plot_cust.png") # look at smart-meter.R for the code


## ----missing-data

knitr::include_graphics("figs/missing-data-5050.png")


## ----mds


knitr::include_graphics("figs/tour-tsne.png")



## ----sim-val

set.seed(9999)
nx_val <- 2 # number of x-axis levels
nfacet_val <- 3 # number of facet levels
w1_val <- 3 # increment in mean
w2_val <- 0 # increment in sd
mean_val <- 0 # mean of normal distribution of starting combination
sd_val <- 1 # sd of normal distribution of starting combination
quantile_prob_val <- seq(0.1, 0.9, 0.1)

## ----nobs50

ntimes_val <- 300

### ----sample-seed-few

sample_seed <- seq(10, 100, 10)


#### ----change-index-data-few

change_index_data <- function(ntimes_val = NULL,
                              nx_val = NULL,
                              nfacet_val = NULL,
                              sim_function = sim_varx_normal) {
  data <- sim_panel(
    nx = nx_val, nfacet = nfacet_val,
    ntimes = ntimes_val,
    # sim_dist = sim_varx_normal(2, 3, 5, 10, 5, -1.5)
    sim_dist = sim_function(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
  ) %>% unnest(data)


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
    mutate(time = row_number()) %>%
    select(-c(index_old, index_new))

  return(y)
}

## ---- data-make-few

data_null <- map(sample_seed, function(seed) {
  set.seed(seed)
  change_index_data(5, 2, 3, sim_null_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_null")


data_varf <- map(sample_seed, function(seed) {
  set.seed(seed)
  change_index_data(5, 2, 3, sim_varf_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varf")

data_varx <- map(sample_seed, function(seed) {
  set.seed(seed)
  change_index_data(5, 2, 3, sim_varx_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varx")

data_varall <- map(sample_seed, function(seed) {
  set.seed(seed)
  change_index_data(5, 2, 3, sim_varall_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varall")

data_q <- bind_rows(
  data_null,
  data_varf,
  data_varx,
  data_varall
) %>%
  mutate(seed_id = sprintf(
    "%02d",
    as.numeric(seed_id)
  )) %>%
  mutate(unique_data = paste(data_type, seed_id, sep = "-"))

data_q$data_type <- factor(data_q$data_type,
  levels = c(
    "data_null", "data_varf",
    "data_varx", "data_varall"
  )
)


## ----JS-mydata-few
data_q_wide <- data_q %>%
  select(id_facet, id_x, unique_data, sim_data_quantile) %>%
  pivot_wider(
    names_from = unique_data,
    values_from = sim_data_quantile
  ) %>%
  select(-c(1, 2))

ndata <- data_q %>%
  distinct(unique_data) %>%
  mutate(index = row_number())
ldata <- nrow(ndata)
lcomb <- nx_val * nfacet_val

dist_data <- map(1:ldata, function(x) { # first data
  map(1:ldata, function(y) { # 2nd data
    map(1:lcomb, function(z) { # number of combinations nx*nfacet
      JS(
        prob = quantile_prob_val,
        unlist(data_q_wide[z, x]),
        unlist(data_q_wide[z, y])
      ) %>% as_tibble()
    }) %>% bind_rows(.id = "combinations")
  }) %>% bind_rows(.id = "data_type1")
}) %>% bind_rows(.id = "data_type2")



dist_mat <- dist_data %>%
  group_by(data_type1, data_type2) %>%
  summarise(dist = sum(value)) %>%
  pivot_wider(
    names_from = data_type2,
    values_from = dist
  ) %>%
  mutate(data_type1 = as.numeric(data_type1)) %>%
  left_join(ndata, by = c("data_type1" = "index"))


dist_mat_format <- dist_mat %>%
  ungroup() %>%
  select(-data_type1, -unique_data)


rownames(dist_mat_format) <- dist_mat$unique_data

## ----hier-clust-few
d <- stats::as.dist(dist_mat_format)
par(cex = 0.5)
hc <- stats::hclust(d, method = "complete")
plot(hc)
rect.hclust(hc, k = 4, border = "red")

## ----mds-plot-interact-few
mds <- d %>%
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")
rownames(mds) <- dist_mat$unique_data

groups <- cutree(hc, k = 4)

all_data_cluster <- cbind(dist_mat_format, groups) %>%
  cbind(mds) %>%
  mutate(groups = as.factor(groups)) %>%
  as_tibble()


mds_plot_10 <- ggplot(
  all_data_cluster,
  aes(
    x = Dim.1,
    y = Dim.2,
    color = groups
  )
) +
  geom_text(label = rownames(mds), check_overlap = TRUE) +
  geom_point(size = 1) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  coord_fixed(ratio = 1) +
  theme(text = element_text(size = 6))

mds_plot_10

## ----sample-seed
sample_seed <- seq(1, 100, 1)

## ----change-index-data
change_index_data <- function(ntimes_val = NULL,
                              nx_val = NULL,
                              nfacet_val = NULL,
                              sim_function = sim_varx_normal) {
  data <- sim_panel(
    nx = nx_val, nfacet = nfacet_val,
    ntimes = ntimes_val,
    # sim_dist = sim_varx_normal(2, 3, 5, 10, 5, -1.5)
    sim_dist = sim_function(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
  ) %>% unnest(data)


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
    mutate(time = row_number()) %>%
    select(-c(index_old, index_new))

  return(y)
}

## ----data-make

data_null <- map(sample_seed, function(seed) {
  set.seed(sample_seed[seed])
  change_index_data(5, 2, 3, sim_null_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_null")


data_varf <- map(sample_seed, function(seed) {
  set.seed(sample_seed[seed])
  change_index_data(5, 2, 3, sim_varf_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varf")

data_varx <- map(sample_seed, function(seed) {
  set.seed(sample_seed[seed])
  change_index_data(5, 2, 3, sim_varx_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varx")

data_varall <- map(sample_seed, function(seed) {
  set.seed(sample_seed[seed])
  change_index_data(5, 2, 3, sim_varall_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varall")

data_q <- bind_rows(
  data_null,
  data_varf,
  data_varx,
  data_varall
) %>%
  mutate(seed_id = sprintf(
    "%02d",
    as.numeric(seed_id)
  )) %>%
  mutate(unique_data = paste(data_type, seed_id, sep = "-"))

data_q$data_type <- factor(data_q$data_type,
  levels = c(
    "data_null", "data_varf",
    "data_varx", "data_varall"
  )
)

## ----JS-mydata
data_q_wide <- data_q %>%
  select(id_facet, id_x, unique_data, sim_data_quantile) %>%
  pivot_wider(
    names_from = unique_data,
    values_from = sim_data_quantile
  ) %>%
  select(-c(1, 2))

ndata <- data_q %>%
  distinct(unique_data) %>%
  mutate(index = row_number())
ldata <- nrow(ndata)
lcomb <- nx_val * nfacet_val

dist_data <- map(1:ldata, function(x) { # first data
  map(1:ldata, function(y) { # 2nd data
    map(1:lcomb, function(z) { # number of combinations nx*nfacet
      JS(
        prob = quantile_prob_val,
        unlist(data_q_wide[z, x]),
        unlist(data_q_wide[z, y])
      ) %>% as_tibble()
    }) %>% bind_rows(.id = "combinations")
  }) %>% bind_rows(.id = "data_type1")
}) %>% bind_rows(.id = "data_type2")



dist_mat <- dist_data %>%
  group_by(data_type1, data_type2) %>%
  summarise(dist = sum(value)) %>%
  pivot_wider(
    names_from = data_type2,
    values_from = dist
  ) %>%
  mutate(data_type1 = as.numeric(data_type1)) %>%
  left_join(ndata, by = c("data_type1" = "index"))


dist_mat_format <- dist_mat %>%
  ungroup() %>%
  select(-data_type1, -unique_data)


rownames(dist_mat_format) <- dist_mat$unique_data
# write_rds(dist_mat_format, "validation/interact-4D/distmat_400series.rds")
# write_rds(dist_mat, "validation/interact-4D/distmat_400series_withnames.rds")

## ----hier-clust

d <- stats::as.dist(dist_mat_format)
par(cex = 0.5)
hc <- stats::hclust(d, method = "complete")
plot(hc)
rect.hclust(hc, k = 4, border = "red")

## ----mds-plot-interact
mds <- d %>%
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")
rownames(mds) <- dist_mat$unique_data

groups <- cutree(hc, k = 4)

all_data_cluster <- cbind(dist_mat_format, groups) %>%
  cbind(mds) %>%
  mutate(groups = as.factor(groups)) %>%
  as_tibble()


mds_plot_10 <- ggplot(
  all_data_cluster,
  aes(
    x = Dim.1,
    y = Dim.2,
    color = groups
  )
) +
  # geom_text(label = rownames(mds),check_overlap = TRUE)+
  geom_point(size = 0.5) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  coord_fixed(ratio = 1)

mds_plot_10

## ----heatmap-interact
heatmap_raw <- data_q %>%
  # select(unique_data, sim_data_quantile, id_facet, id_x) %>%
  mutate(category = paste0("facet= ", id_facet, ",x= ", id_x)) %>%
  unnest(sim_data_quantile) %>%
  group_by(unique_data, category) %>%
  mutate(D = row_number()) %>%
  arrange(D, category) %>%
  left_join(dist_mat, by = c("unique_data")) %>%
  select(c(2:9))


data2 <- cbind(dist_mat_format, groups)

heatmap_final <- bind_cols(
  serial_data = rownames(data2) %>%
    as_tibble(),
  group = data2$groups
) %>%
  left_join(heatmap_raw, by = c("value" = "unique_data")) %>%
  select(-id_facet, -id_x, -data_type1) %>%
  arrange(D, category, group)


heatmap_final$data_type <- factor(heatmap_final$data_type,
  levels = c(
    "data_null", "data_varf",
    "data_varx", "data_varall"
  )
)

heatmap_final$category <- reorder(heatmap_final$category, heatmap_final$data_type)

ggplot(
  heatmap_final,
  aes(x = reorder(value, data_type), y = as.factor(D))
) +
  geom_tile(aes(fill = sim_data_quantile)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  facet_wrap(~category, ncol = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Deciles") +
  xlab("data sets") +
  theme(legend.position = "bottom") +
  scale_x_discrete(breaks = c(
    "data_null", "data_varf",
    "data_varx", "data_varall"
  ))


## ----data-load-entire
# Read the nqt distances

wkndwday <- read_rds(here("data/dist_gran_wkndwday_356cust_nqt.rds")) %>% broom::tidy()

moy <- read_rds(here("data/dist_gran_moy_356cust_nqt.rds")) %>% broom::tidy()

hod <- read_rds(here("data/dist_gran_hod_356cust_nqt.rds")) %>% broom::tidy()


# Make the distance metrics

distance <- wkndwday %>%
  left_join(moy, by = c("item1", "item2")) %>%
  left_join(hod, by = c("item1", "item2")) %>%
  rename(
    "wkndwday" = "distance.x",
    "moy" = "distance.y",
    "hod" = "distance"
  ) %>%
  mutate(
    item1 = as.integer(as.character(item1)),
    item2 = as.integer(as.character(item2))
  )

filtered_distance <- distance %>%
  filter(!(item1 %in% c(8196183, 8508008, 8680538))) %>%
  filter(!(item2 %in% c(8196183, 8508008, 8680538)))

total_distance <- filtered_distance %>%
  mutate(total = wkndwday / 2 + moy / 12 + hod / 24)


total_distance_wide <- total_distance %>% pivot_wider(-c(2:5),
  names_from = item2,
  values_from = total
)


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


first_lot <- mds_data %>% names()

id <- c(first_lot[-1], mds_data$name[nrow(mds_data)])

## ----opt-clust-350

# Find optimal number of clusters

k <- array()
for (i in 5:50)
{
  group <- f %>%
    hclust(method = "ward.D") %>%
    cutree(k = i)
  p <- cluster.stats(f, clustering = group, silhouette = TRUE)
  k[i] <- p$sindex
}

ggplot(k %>% as_tibble() %>% mutate(k = row_number()), aes(x = k, y = value)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2, 50, 2), minor_breaks = 1) +
  xlab("number of clusters") +
  ylab("separation index")


## ----group-353
# plot(k, type = "l")
# 6 coming as the number of clusters with maximum silwidth

group <- f %>%
  hclust(method = "ward.D") %>%
  cutree(k = 17)

cluster_result <- bind_cols(customer_id = id, group = group) %>%
  arrange(group)
# %>%
#   mutate(divide_cust = c(rep(1, 177), rep(2, 176)))

# cluster_result %>% group_by(group) %>% count()


## ----data-heatmap-hod-group-entire
legend_title <- "group"

data_pick <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds")) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  gracsr::scale_gran(
    method = "robust",
    response = "general_supply_kwh"
  )

data_group <- data_pick %>%
  mutate(customer_id = as.character(customer_id)) %>%
  gracsr::scale_gran(
    method = "robust",
    response = "general_supply_kwh"
  ) %>%
  left_join(cluster_result, by = c("customer_id"))

data_heatmap_hod_group <- quantile_gran(data_group,
  gran1 = "hour_day",
  quantile_prob_val = c(0.25, 0.5, 0.75),
  group = "group"
) %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)


data_heatmap_hod_group$category <- factor(data_heatmap_hod_group$category, levels = 0:23)

# data_heatmap_hod_group$group <- paste("group", data_heatmap_hod_group$group, sep = "-")

hod_group_entire1 <- data_heatmap_hod_group %>%
  filter(group < 9) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = group,
    fill = as.factor(group), alpha = 0.5
  ),
  alpha = 0.5
  ) +
  geom_line(aes(
    y = `50%`,
    group = group,
    color = as.factor(group)
  ), size = 1) +
  facet_wrap(~group,
    scales = "free_y",
    nrow = 8, labeller = "label_both"
  ) +
  # labeller = labeller(xfacet = c(`1` = "Group 2", `2` = "Group 4",`3` = "Group 1",`4` = "Group 3"))
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("hour-of-day") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  scale_x_discrete(breaks = seq(1, 24, 3)) +
  # theme(strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme_application() +
  scale_fill_manual(values = as.vector(okabe(n = 8))) +
  scale_color_manual(values = as.vector(okabe(n = 8))) +
  theme(legend.position = "bottom")

hod_group_entire2 <- data_heatmap_hod_group %>%
  filter(group >= 9) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = group,
    fill = as.factor(group), alpha = 0.5
  ),
  alpha = 0.5
  ) +
  geom_line(aes(
    y = `50%`,
    group = group,
    color = as.factor(group)
  ), size = 1) +
  facet_wrap(~group,
    scales = "free_y",
    nrow = 9, labeller = "label_both"
  ) +
  # labeller = labeller(xfacet = c(`1` = "Group 2", `2` = "Group 4",`3` = "Group 1",`4` = "Group 3"))
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("hour-of-day") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  scale_x_discrete(breaks = seq(1, 24, 3)) +
  # theme(strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme_application() +
  scale_fill_manual(values = as.vector(tableau20(20))) +
  scale_color_manual(values = as.vector(tableau20(20))) +
  theme(legend.position = "bottom")


## ----data-heatmap-moy-group-entire
data_heatmap_moy_group <- quantile_gran(data_group,
  gran1 = "month_year",
  quantile_prob_val = c(0.25, 0.5, 0.75),
  group = "group"
) %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)

data_heatmap_moy_group$category <- factor(data_heatmap_moy_group$category, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


# data_heatmap_moy_group$group <- paste("group", data_heatmap_moy_group$group, sep = "-")


moy_group_entire1 <- data_heatmap_moy_group %>%
  filter(group < 9) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`, group = group, fill = as.factor(group)
  ), alpha = 0.5) +
  geom_line(aes(y = `50%`, group = group, color = as.factor(group)), size = 1) +
  facet_wrap(~group,
    scales = "free_y",
    labeller = "label_both",
    nrow = 8
  ) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("month-of-year") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  theme_application() +
  scale_fill_manual(values = as.vector(okabe(n = 8))) +
  scale_color_manual(values = as.vector(okabe(n = 8))) +
  theme(legend.position = "bottom")


moy_group_entire2 <- data_heatmap_moy_group %>%
  filter(group >= 9) %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`, group = group, fill = as.factor(group)
  ), alpha = 0.5) +
  geom_line(aes(y = `50%`, group = group, color = as.factor(group)), size = 1) +
  facet_wrap(~group,
    scales = "free_y",
    labeller = "label_both",
    nrow = 9
  ) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("month-of-year") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  theme_application() +
  scale_fill_manual(values = as.vector(tableau20(20))) +
  scale_color_manual(values = as.vector(tableau20(20))) +
  theme(legend.position = "bottom")

## ----data-wnwd-group-entire
wkndwday_data <- data_group %>%
  create_gran("wknd_wday")
# wkndwday_data$group <- as.factor(wkndwday_data$group)
# %>%
#   create_gran("hour_day")

ylim1 <- boxplot.stats(wkndwday_data$general_supply_kwh)$stats[c(1, 5)]

wkndwday_group_entire1 <- wkndwday_data %>%
  filter(group < 9) %>%
  mutate(group = as.factor(group)) %>%
  ggplot(aes(x = wknd_wday, y = general_supply_kwh)) +
  # lvplot::geom_lv(aes(fill = as.factor(group)), k=5) +
  geom_boxplot(aes(fill = group, color = group), alpha = 0.5, outlier.alpha = 0.05) +
  # geom_boxplot(outlier.size = 1) +
  coord_cartesian(ylim = ylim1 * 1.05) +
  # ggridges::geom_density_ridges2(aes(x = general_supply_kwh, y = wknd_wday,fill = as.factor(group))) + coord_flip() +
  # geom_boxplot(aes(fill = as.factor(group))) +
  # scale_fill_lv() +
  xlab("wknd-wday") +
  ylab("demand (in Kwh)") +
  facet_wrap(~group,
    scales = "free_y",
    labeller = "label_both",
    nrow = 8
  ) +
  theme_bw() +
  theme_application() +
  scale_fill_manual(values = as.vector(okabe(n = 8))) +
  scale_color_manual(values = as.vector(okabe(n = 8))) +
  theme(legend.position = "none")

wkndwday_group_entire2 <- wkndwday_data %>%
  filter(group >= 9) %>%
  mutate(group = as.factor(group)) %>%
  ggplot(aes(x = wknd_wday, y = general_supply_kwh)) +
  # lvplot::geom_lv(aes(fill = as.factor(group)), k=5) +
  geom_boxplot(aes(fill = group, color = group), alpha = 0.5, outlier.alpha = 0.05) +
  # geom_boxplot(outlier.size = 1) +
  coord_cartesian(ylim = ylim1 * 1.05) +
  # ggridges::geom_density_ridges2(aes(x = general_supply_kwh, y = wknd_wday,fill = as.factor(group))) + coord_flip() +
  # geom_boxplot(aes(fill = as.factor(group))) +
  # scale_fill_lv() +
  xlab("wknd-wday") +
  ylab("demand (in Kwh)") +
  facet_wrap(~group,
    scales = "free_y",
    labeller = "label_both",
    nrow = 9
  ) +
  theme_bw() +
  theme_application() +
  scale_fill_manual(values = as.vector(tableau20(20))) +
  scale_color_manual(values = as.vector(tableau20(20))) +
  theme(legend.position = "none")

## ---- combined-groups-js-entire
group_entire1 <- (hod_group_entire1 + moy_group_entire1 + wkndwday_group_entire1) & theme_characterisation() & theme(legend.position = "none")

group_entire2 <- (hod_group_entire2 + moy_group_entire2 + wkndwday_group_entire2) & theme_characterisation() & theme(legend.position = "none")

ggpubr::ggarrange(group_entire1, group_entire2)
#
#
#  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')+
#  plot_layout(guides = "collect")& theme(legend.position = 'none')
#
#
# (hod_group_entire/moy_group_entire/ wkndwday_group_entire) +
#  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')+
#  plot_layout(guides = "collect")& theme(legend.position = 'none')
#  



## ----sindex-data-valid-supplement--------------------------------

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


data1 <- sindex_data(read_rds("validation/js-robust/3gran_change_5D/data_validation_25.rds")) %>% mutate(diff = 1, design = "S1")
data2 <- sindex_data(read_rds("validation/js-robust/3gran_change_5D/data_validation_26.rds")) %>% mutate(diff = 2, design = "S1")
data3 <- sindex_data(read_rds("validation/js-robust/3gran_change_5D/data_validation_27.rds")) %>% mutate(diff = 5, design = "S1")

data4 <- sindex_data(read_rds("validation/js-robust/2gran_change_4D/data_validation_7.rds")) %>% mutate(diff = 1, design = "S2")
data5 <- sindex_data(read_rds("validation/js-robust/2gran_change_4D/data_validation_17.rds")) %>% mutate(diff = 2, design = "S2")
data6 <- sindex_data(read_rds("validation/js-robust/2gran_change_4D/data_validation_9.rds")) %>% mutate(diff = 5, design = "S2")


data7 <- sindex_data(read_rds("validation/js-robust/1gran_change_5D/data_validation_16.rds")) %>% mutate(diff = 1, design = "S3")
data8 <- sindex_data(read_rds("validation/js-robust/1gran_change_5D/data_validation_17.rds")) %>% mutate(diff = 2, design = "S3")
data9 <- sindex_data(read_rds("validation/js-robust/1gran_change_5D/data_validation_18.rds")) %>% mutate(diff = 5, design = "S3")

all_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9) %>% rename("Scenario" = "design")



## ----sindex-plot-valid-supplement------------------------------
sindex_plot_robust <- ggplot(
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

sindex_plot_robust

