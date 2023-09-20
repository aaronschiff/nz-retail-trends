# Explore NZ real retail sales

library(tidyverse)
library(here)
library(janitor)
library(patchwork)
library(scales)
library(ragg)

# Stats NZ retail trade data
dat_retail <- read_csv(
  file = here("data/retail_trade.csv"),
  col_types = "ccnccciccccccc"
) |>
  clean_names()

# Quarterly real retail sales (seasonally adjusted)
dat_retail_real_sales <- dat_retail |>
  # Filter sales
  filter(
    group == "Sales and stocks by industry, in current and constant prices (SAFC)",
    series_title_2 == "Sales (operating income)",
    series_title_3 == "Deflated, at September 2010 quarter prices",
    series_title_4 == "Seasonally adjusted",
    series_title_1 != "All industries total",
    series_title_1 != "Core industries total"
  ) |>
  mutate(series_type = str_sub(string = series_reference, start = 1, end = 4)) |>
  filter(series_type == "RTTQ") |>  # Quarterly series
  # Process dates
  separate(col = period, into = c("year", "month"), sep = "\\.", convert = TRUE) |>
  mutate(quarter = as.integer(month / 3)) |>
  mutate(date = yq(paste0(year, "Q", quarter))) |>
  # Tidy up
  select(
    series_reference,
    sector = series_title_1,
    date,
    data_value
  ) |>
  arrange(sector, date)

# Classify trends over the past 18 months
dat_retail_real_sales_trends <- dat_retail_real_sales |>
  group_by(sector) |>
  slice_tail(n = 6) |>
  mutate(t = row_number()) |>
  ungroup() |>
  nest_by(series_reference, sector) |>
  mutate(m = list(lm(formula = data_value ~ t, data = data))) |>
  mutate(coef = list(enframe(coef(m)))) |>
  select(-m) |>
  unnest(cols = coef) |>
  ungroup() |>
  filter(name == "t") |>
  mutate(slope = ifelse(
    test = value > 0,
    yes = "pos",
    no = "neg"
  )) |>
  select(series_reference, sector, data, slope) |>
  unnest(cols = data) |>
  select(-t)

# Positive trend chart
chart_retail_real_sales_trend_pos <- dat_retail_real_sales_trends |>
  filter(slope == "pos") |>
  ggplot(mapping = aes(
    x = date,
    y = data_value
  )) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_point(colour = "cornflowerblue") +
  geom_line(colour = "cornflowerblue") +
  facet_wrap(
    facets = vars(sector),
    ncol = 5,
    labeller = label_wrap_gen(width = 22)
  ) +
  scale_x_date(
    labels = label_date(format = "%b\n%y"),
    breaks = seq(
      from = ymd("2022-01-01"),
      to = ymd("2023-06-01"),
      by = "3 months"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 5000),
    labels = comma_format(accuracy = 1)
  ) +
  labs(
    x = "Quarter starting date",
    y = "Real sales\n(2010 $m)",
    title = "Increasing trend"
  )

# Negative trend chart
chart_retail_real_sales_trend_neg <- dat_retail_real_sales_trends |>
  filter(slope == "neg") |>
  ggplot(mapping = aes(
    x = date,
    y = data_value
  )) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_point(colour = "firebrick") +
  geom_line(colour = "firebrick") +
  facet_wrap(
    facets = vars(sector),
    ncol = 5,
    labeller = label_wrap_gen(width = 22)
  ) +
  scale_x_date(
    labels = label_date(format = "%b\n%y"),
    breaks = seq(
      from = ymd("2022-01-01"),
      to = ymd("2023-06-01"),
      by = "3 months"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 5000),
    labels = comma_format(accuracy = 1)
  ) +
  labs(
    x = "Quarter starting date",
    y = "Real sales\n(2010 $m)",
    title = "Decreasing trend"
  )

# Combined chart
chart_retail_real_sales_trend <-
  chart_retail_real_sales_trend_pos /
    chart_retail_real_sales_trend_neg +
    plot_layout(heights = c(1, 2)) &
    theme_minimal() &
    theme(
      axis.title.y = element_text(
        angle = 0,
        hjust = 0,
        face = "italic",
        margin = margin(0, 10, 0, 0, "pt")
      ),
      axis.title.x = element_text(
        face = "italic",
        margin = margin(8, 0, 0, 0, "pt")
      ),
      strip.text = element_text(hjust = 0, vjust = 0, face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.spacing.x = unit(24, "pt")
    )

ggsave(
  filename = here("outputs/chart_retail_real_sales_trend.png"),
  plot = chart_retail_real_sales_trend,
  device = agg_png,
  width = 26,
  height = 18,
  units = "cm",
  bg = "white"
)

# Percentage changes over the 18 months
dat_retail_real_sales_change <- dat_retail_real_sales_trends |>
  filter(date %in% c(
    ymd("2023-04-01"),
    ymd("2022-01-01")
  )) |>
  arrange(sector, date) |>
  group_by(sector) |>
  mutate(pct_chg = data_value / lag(data_value) - 1) |>
  ungroup() |>
  filter(!is.na(pct_chg)) |>
  arrange(desc(pct_chg)) |>
  mutate(sign = ifelse(
    test = pct_chg > 0,
    yes = "pos",
    no = "neg"
  )) |>
  mutate(hjust = ifelse(
    test = pct_chg > 0,
    yes = 0,
    no = 1
  )) |>
  mutate(label_x = ifelse(
    test = pct_chg > 0,
    yes = pct_chg + 0.005,
    no = pct_chg - 0.005
  ))

chart_retail_real_sales_change <- dat_retail_real_sales_change |>
  ggplot(mapping = aes(
    y = fct_reorder(.f = sector, .x = pct_chg),
    x = pct_chg,
    label = percent(x = pct_chg, accuracy = 0.1),
    fill = sign,
    colour = sign
  )) +
  geom_vline(xintercept = 0, linewidth = 0.25) +
  geom_col(
    linewidth = 0
  ) +
  geom_text(mapping = aes(
    x = label_x,
    hjust = hjust
  ),
  size = 3,
  fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(-0.34, 0.24),
    breaks = seq(-0.35, 0.25, 0.05),
    labels = percent_format(accuracy = 1),
    expand = expansion(0, 0),
    position = "top"
  ) +
  scale_colour_manual(
    values = c(
      "pos" = "cornflowerblue",
      "neg" = "firebrick"
    ),
    guide = "none",
    aesthetics = c("colour", "fill")
  ) +
  labs(
    x = "Real retail sales mid-2023 vs early 2022",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x.top = element_text(
      face = "italic",
      margin = margin(0, 0, 8, 0, "pt")
    )
  )

ggsave(
  filename = here("outputs/chart_retail_real_sales_change.png"),
  plot = chart_retail_real_sales_change,
  device = agg_png,
  width = 26,
  height = 10,
  units = "cm",
  bg = "white"
)

# Quarterly price indexes
dat_retail_price_indexes <- dat_retail |>
  # Filter sales
  filter(
    group == "Retail trade sales deflators by industry - Base September 2010 quarter (=1000)",
    series_title_1 != "All industries total",
    series_title_1 != "Core industries total"
  ) |>
  # Process dates
  separate(col = period, into = c("year", "month"), sep = "\\.", convert = TRUE) |>
  mutate(quarter = as.integer(month / 3)) |>
  mutate(date = yq(paste0(year, "Q", quarter))) |>
  # Tidy up
  select(
    series_reference,
    sector = series_title_1,
    date,
    data_value
  ) |>
  filter(!is.na(data_value)) |>
  arrange(sector, date)

# Price index changes mid 2023 vs 2022
dat_retail_price_indexes_change <- dat_retail_price_indexes |>
  filter(date %in% c(
    ymd("2023-04-01"),
    ymd("2022-01-01")
  )) |>
  arrange(sector, date) |>
  group_by(sector) |>
  mutate(pct_chg = data_value / lag(data_value) - 1) |>
  ungroup() |>
  filter(year(date) == 2023) |>
  mutate(direction = ifelse(
    test = pct_chg > 0,
    yes = "up",
    no = "down"
  )) |>
  mutate(hjust = ifelse(
    test = pct_chg > 0,
    yes = 0,
    no = 1
  )) |>
  mutate(label_x = ifelse(
    test = pct_chg > 0,
    yes = pct_chg + 0.002,
    no = pct_chg - 0.002
  ))

chart_retail_price_indexes_change <- dat_retail_price_indexes_change |>
  ggplot(mapping = aes(
    y = fct_reorder(.f = sector, .x = pct_chg),
    x = pct_chg,
    fill = direction,
    colour = direction,
    label = percent(x = pct_chg, accuracy = 0.1)
  )) +
  geom_vline(xintercept = 0, linewidth = 0.25) +
  geom_vline(
    xintercept = 0.07793345,
    linewidth = 0.5,
    linetype = "dashed",
    colour = grey(0.5)
  ) +
  geom_col(
    linewidth = 0
  ) +
  geom_text(mapping = aes(
    x = label_x,
    hjust = hjust
  ),
  size = 3,
  fontface = "bold"
  ) +
  annotate(
    geom = "text",
    x = 0.081,
    y = 1,
    label = "CPI: 7.8%",
    colour = grey(0.5),
    hjust = 0,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(-0.069, 0.16),
    breaks = seq(-0.1, 0.2, 0.05),
    labels = percent_format(accuracy = 1),
    expand = expansion(0, 0),
    position = "top"
  ) +
  scale_colour_manual(
    values = c(
      "up" = "cornflowerblue",
      "down" = "firebrick"
    ),
    guide = "none",
    aesthetics = c("colour", "fill")
  ) +
  labs(
    x = "Retail price index mid-2023 vs early 2022",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x.top = element_text(
      face = "italic",
      margin = margin(0, 0, 8, 0, "pt")
    )
  )

ggsave(
  filename = here("outputs/chart_retail_price_indexes_change.png"),
  plot = chart_retail_price_indexes_change,
  device = agg_png,
  width = 26,
  height = 10,
  units = "cm",
  bg = "white"
)

# Scatterplot price index changes vs real sales changes Jan 2023 vs 2022
dat_retail_price_vs_real_sales_changes <-
  dat_retail_price_indexes_change |>
  select(sector, price_pct_chg = pct_chg) |>
  arrange(desc(price_pct_chg)) |>
  left_join(
    y = dat_retail_real_sales_change |>
      select(sector, volume = data_value, volume_pct_chg = pct_chg),
    by = "sector"
  ) |>
  mutate(label = case_when(
    sector == "Accommodation" ~ "Accommodation",
    sector == "Supermarket and grocery stores" ~ "Supermarket & grocery",
    sector == "Fuel retailing" ~ "Fuel",
    sector == "Electrical and electronic goods retailing" ~ "Electrical & electronic",
    sector == "Specialised food retailing (excluding liquor)" ~ "Specialised food",
    sector == "Department stores" ~ "Department stores",
    sector == "Hardware, building and garden supplies" ~ "Hardware, building & garden",
    sector == "Liquor retailing" ~ "Liquor",
    sector == "Food and beverage services" ~ "Food & beverage services",
    TRUE ~ ""
  )) |>
  mutate(hjust = case_when(
    label == "Accommodation" ~ -0.15,
    label == "Supermarket & grocery" ~ -0.18,
    label == "Fuel" ~ -0.6,
    label == "Electrical & electronic" ~ -0.15,
    label == "Specialised food" ~ -0.1,
    label == "Department stores" ~ -0.15,
    label == "Hardware, building & garden" ~ -0.12,
    label == "Liquor" ~ -0.23,
    label == "Food & beverage services" ~ 1.13
  ))

chart_retail_price_vs_real_sales_changes <-
  dat_retail_price_vs_real_sales_changes |>
  arrange(desc(volume)) |>
  ggplot(mapping = aes(
    x = price_pct_chg,
    y = volume_pct_chg
  )) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_vline(xintercept = 0, linewidth = 0.25) +
  geom_vline(
    xintercept = 0.07793345,
    linewidth = 0.5,
    linetype = "dashed",
    colour = grey(0.5)
  ) +
  geom_point(
    mapping = aes(size = volume),
    shape = 21,
    fill = "purple4",
    colour = "white"
  ) +
  geom_text(
    mapping = aes(label = label, hjust = hjust),
    colour = "purple4"
  ) +
  annotate(
    geom = "text",
    x = 0.081,
    y = -0.27,
    label = "CPI: 7.8%",
    colour = grey(0.5),
    hjust = 0,
    fontface = "bold"
  ) +
  scale_size_area(
    max_size = 15,
    name = "Mid-2023\nreal sales\n(2010 $m)",
    limits = c(0, 5000),
    breaks = seq(1000, 5000, 1000),
    labels = comma_format(accuracy = 1)
  ) +
  scale_x_continuous(
    limits = c(-0.065, 0.23),
    breaks = seq(-0.05, 0.5, 0.05),
    labels = percent_format(accuracy = 1),
    expand = expansion(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-0.31, 0.23),
    breaks = seq(-0.3, 0.5, 0.05),
    labels = percent_format(accuracy = 1),
    expand = expansion(0, 0)
  ) +
  labs(
    x = "Price index mid-2023 vs early 2022",
    y = "Real sales\nmid-2023 vs\nearly 2022"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(
      angle = 0,
      hjust = 0,
      face = "italic",
      margin = margin(0, 10, 0, 0, "pt")
    ),
    axis.title.x = element_text(
      face = "italic",
      hjust = 1,
      margin = margin(8, 0, 0, 0, "pt")
    )
  )

ggsave(
  filename = here("outputs/chart_retail_price_vs_real_sales_changes.png"),
  plot = chart_retail_price_vs_real_sales_changes,
  device = agg_png,
  width = 24,
  height = 16,
  units = "cm",
  bg = "white"
)
