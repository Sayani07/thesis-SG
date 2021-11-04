## ---- load
library(knitr)
library(readr)
library(dplyr)
library(ggplot2)
library(tsibble)
library(patchwork)
library(lvplot)
if (requireNamespace("gravitas") == FALSE) {
  remotes::install_github("Sayani07/gravitas")
}
library(gravitas)
library(ggpubr)
library(kableExtra)

## ---- linear-time
include_graphics("img/linear-ex.png")

## ---- circular-dow
include_graphics("img/circular-ex.png")

## ----aperiodic-example
include_graphics("img/aperiodic-ex.png")
#include_graphics("img/aperiodic-ex3.png")

## ----tab-mayan
table_mayan <- tibble(
  `linear (G)` = c("kin", "uinal", "tun", "katun", "baktun"),
  `single-order-up cyclic (C)` = c("kin-of-uinal", "uinal-of-tun", " tun-of-katun", "katun-of-baktun", 1),
  `period length/conversion operator (K)` = c(20, 18, 20, 20, 1)
)
table_mayan %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "l",
    caption = "Hierarchy table for Mayan calendar with circular single-order-up granularities."
  ) %>%
  kable_styling()

## ----tab-gregorian
table_greg <- tibble(
  `linear (G)` = c("minute", "hour", "day", "month", "year"),
  `single-order-up cyclic (C)` = c("minute-of-hour", "hour-of-day", "day-of-month", "month-of-year", 1),
  `period length/conversion operator (K)` = c(60, 24, "k(day, month)", 12, 1)
)

table_greg %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Hierarchy table for the Gregorian calendar with both circular and quasi-circular single-order-up granularities."
  ) %>%
  # row_spec(0, bold = TRUE) %>%
  kable_styling()

## ----allFig
VIC <- gravitas::smart_meter10 %>%
  filter(customer_id == "10006486")

scene1 <- VIC %>%
  create_gran("quarter_year") %>%
  create_gran("wknd_wday") %>%
  ggplot(aes(x = wknd_wday,
             y = general_supply_kwh)) +
  facet_wrap(~quarter_year, nrow = 1) +
  geom_lv(aes(fill = ..LV..), k = 5) +
  ylab("") +
  xlab("") +
  scale_fill_brewer(type = "seq", direction = -1) +
  theme(legend.position = "right") +
  ggtitle("") +
  scale_y_log10() +
  theme_minimal() +
  theme(
    #panel.grid.major = element_line(colour = "#E0E0E0"),
    panel.border = element_rect(colour = "#E0E0E0",
                                fill = NA),
    panel.grid.major.x = element_blank(),
    legend.position = "top")



scene2 <- VIC %>%
  prob_plot(
    "wknd_wday", "quarter_year",
    response = "general_supply_kwh",
    plot_type = "lv",
    symmetric = FALSE
  ) +
  ylab("") +
  xlab("quarters of the year") +
  scale_fill_brewer(type = "seq", direction = -1)  +
  ggtitle("") +
  scale_y_log10() +
  theme_minimal() +
  theme(
    #panel.grid.major = element_line(colour = "#E0E0E0"),
    panel.border = element_rect(colour = "#E0E0E0",
                                fill = NA),
    panel.grid.major.x = element_blank())+
  theme(legend.position = "none")

scene3 <- VIC %>%
  prob_plot(
    "quarter_year", "month_year",
    response = "general_supply_kwh",
    plot_type = "lv",
    symmetric = FALSE
  ) +
  ylab("") +
  xlab("months of the year") +
  scale_fill_brewer(type = "seq", direction = -1) +
  ggtitle("") +
  scale_y_log10() +
  theme_minimal() +
  theme(
    #panel.grid.major = element_line(colour = "#E0E0E0"),
    panel.border = element_rect(colour = "#E0E0E0",
                                fill = NA)) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle = 90, vjust = 0.5))
#panel.grid.major.x = element_blank())

gg_fig <- ggarrange(
  scene1,
  ggarrange(
    scene2, scene3,
    ncol = 2, labels = c("b", "c")
  ),
  nrow = 2, labels = "a", common.legend = TRUE
)


# label.y = "electricity demand [KWh]"

gg_fig %>%
  annotate_figure(
    left = text_grob("electricity demand [KWh]", rot = 90)
  ) +
  theme_minimal()

## ----search
smart_meter10 %>%
  search_gran(filter_out = c(
    "semester",
    "quarter",
    "fortnight"
  ))

## ----search_gran_limit2
smart_meter10 %>%
  search_gran(
    highest_unit = "month",
    filter_out = c("hhour", "fortnight")
  ) %>%
  kable(
    format = "latex",
    booktabs = TRUE
  ) %>%
  # row_spec(0, bold = TRUE)%>%
  kable_styling()

## ----harmony-tab
sm <- smart_meter10 %>%
  filter(customer_id %in% c(10017936))

harmonies <- sm %>%
  harmony(
    ugran = "month",
    filter_in = "wknd_wday",
    filter_out = c("hhour", "fortnight")
  ) %>%
  rename(
    `facet variable` = facet_variable,
    `x-axis variable` = x_variable,
    `facet levels` = facet_levels,
    `x-axis levels` = x_levels
  )

harmonies %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Harmonies with pairs of cyclic granularities, one mapped to facets and the other to the x-axis. Only 16 of 42 possible combinations of cyclic granularities are harmony pairs."
  ) %>%
  # row_spec(0, bold = TRUE) %>%
  kable_styling(latex_options = "hold_position")

## ----bothcust
cust2_quantile <- smart_meter10 %>%
  filter(customer_id %in% c(10017936)) %>%
  prob_plot(
    "wknd_wday", "hour_day",
    response = "general_supply_kwh",
    plot_type = "quantile",
    symmetric = TRUE,
    quantile_prob = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
  ) +
  scale_y_sqrt() +
  ylab("") +
  # ylab("electricity demand [KWh]") +
  xlab("hours of the day") +
  ggtitle("") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "#E0E0E0", fill = NA)
  )

cust2_violin <- smart_meter10 %>%
  filter(customer_id %in% c(10017936)) %>%
  prob_plot(
    "wknd_wday",  "hour_day",
    response = "general_supply_kwh",
    plot_type = "violin"
  ) +
  scale_y_sqrt() +
  ylab("") +
  xlab("hours of the day") +
  ggtitle("") +
  scale_x_discrete(breaks = seq(0, 23, 5)) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "#E0E0E0", fill = NA)
  )

cust2_box <- smart_meter10 %>%
  filter(customer_id %in% c(10017936)) %>%
  prob_plot(
    "hour_day", "wknd_wday",
    response = "general_supply_kwh",
    plot_type = "boxplot"
  ) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  scale_x_discrete(labels = c("wday", "wend")) +
  scale_y_sqrt() +
  theme(axis.text.x = element_text(size = 7)) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "#E0E0E0", fill = NA)
  )

gg_fig <- ggarrange(
  cust2_box,
  ggarrange(
    cust2_quantile, cust2_violin,
    nrow = 2, labels = c("b", "c")
  ),
  ncol = 2, labels = "a"
)

gg_fig %>%
  annotate_figure(
    left = text_grob("electricity demand [KWh]", rot = 90)
  )

## ----hierarchy-cric
hierarchy_model <- tibble(
  `linear (G)` = c("over", "inning", "match", "season"),
  `single-order-up cyclic (C)` = c("over-of-inning", "inning-of-match", "match-of-season", 1),
  `period length/conversion operator (K)` = c(20, 2, "k(match, season)", 1)
)

hierarchy_model %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Hierarchy table for cricket where overs are nested within an innings, innings nested within a match and matches within a season."
  ) %>%
  # row_spec(0, bold = TRUE)%>%
  kable_styling()

## ----cricex
cricket_tsibble <- cricket %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index)

hierarchy_model <- tibble(
  units = c("index", "over", "inning", "match"),
  convert_fct = c(1, 20, 2, 1)
)

cricket_tsibble %>%
  filter(
    batting_team %in% c("Mumbai Indians", "Chennai Super Kings")
  ) %>%
  mutate(inning = paste0("innings: ", inning)) %>%
  prob_plot(
    "inning", "over",
    response = "runs_per_over",
    hierarchy_model,
    plot_type = "lv"
  ) +
  scale_fill_brewer(type = "seq", palette = "Blues", direction = -1) +
  # ggtitle("(a) Runs per over across over faceted by inning") +
  theme(legend.position = "right") +
  ggtitle("a") +
  ylab("runs per over") +
  xlab("overs of the innings")  +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "#E0E0E0", fill = NA)
  ) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)),
        panel.grid.major.x = element_blank())


cricket_all <- read_csv("data-raw/gravitas/deliveries_all.csv")
matches_all <- read_csv("data-raw/gravitas/matches_all.csv")

cricket_season <- cricket_all %>%
  left_join(matches_all, by = c("match_id" = "id"))

cricket_dot_field <- cricket_season %>%
  mutate(
    fielding_proxy = if_else(dismissal_kind %in% c("caught", "caught and bowled"), 1, 0),
    dot_ball_proxy = if_else(total_runs == 0, 1, 0),
    wicket_proxy = if_else(is.na(dismissal_kind), 0, 1)
  ) %>%
  group_by(
    season,
    match_id,
    batting_team,
    bowling_team,
    inning,
    over
  ) %>%
  summarise(
    runs_per_over = sum(total_runs),
    run_rate = sum(total_runs) * 6 / length(total_runs),
    wckts = sum(wicket_proxy),
    dot_balls = sum(dot_ball_proxy)
  ) %>%
  mutate(diff_run_rate = c(0, diff(run_rate)))

cricket_tsibble <- cricket_dot_field %>%
  ungroup() %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index)

cricket_data <- cricket_tsibble %>%
  mutate(
    field = if_else(wckts == 0, "0", "1+"),
    dot = if_else(dot_balls == 0, "no dot balls", ">0 dot balls"),
    lag_field = lag(field),
    lag_dot = lag(dot)
  ) %>%
  filter(lag_field != 0, lag_dot != 0)

cricket_data$lag_field <- factor(cricket_data$field, levels = c("0", "1+"))
#cricket_data$over <- factor(cricket_data$over, levels = seq(1:20))

cricket_data %>%
  filter(over != 1) %>%
  prob_plot(
    "over", "lag_field",
    hierarchy_tbl = hierarchy_model,
    response = "run_rate",
    plot_type = "quantile",
    symmetric = FALSE,
    quantile_prob = c(0.25, 0.5, 0.75),
    size = 2
  ) +
  ylab("runs per over") +
  xlab("number of wickets in previous over") +
  ggtitle("b") +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major.x = element_blank(),
    complete = TRUE,
    panel.grid.major = element_line(colour = "#E0E0E0"),
    panel.border = element_rect(colour = "#E0E0E0", fill = NA)
  ) + scale_colour_manual(values = c("#7093DB",
                                     "#3A5FCD", "#22316C")) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = c("0", "1+"))
