library(tidyverse)
library(ggplot2)
library(ggimage)

result_exact <- read.csv("data/exhibitors_cphijp2026.csv")

# ── 1. Aggregate ──────────────────────────────────────────────────────────────
data <- result_exact %>%
  group_by(country, region, flag_url) %>%
  summarise(value = n(), .groups = "drop") %>%
  arrange(region, desc(value))

# ── 2. Add empty bars for spacing ────────────────────────────────────────────
empty_bar <- 5
to_add <- data.frame(matrix(
  NA,
  empty_bar * n_distinct(data$region),
  ncol(data)
))
colnames(to_add) <- colnames(data)
to_add$region <- rep(unique(data$region), each = empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(region)
data$id <- seq(1, nrow(data))

# ── 3. Label angles ───────────────────────────────────────────────────────────
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

# ── 4. Country flags ──────────────────────────────────────────────────────────
country_labels <- label_data %>%
  filter(!is.na(country)) %>%
  select(id, country, value, angle, hjust, flag_url)

# ── 5. Region labels ──────────────────────────────────────────────────────────
region_labels <- data %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(start_id = min(id), end_id = max(id), .groups = "drop") %>%
  mutate(
    mid_id = (start_id + end_id) / 2,
    angle = 90 - 360 * (mid_id - 0.5) / number_of_bar,
    hjust = 0.5,
    angle = ifelse(angle < -90, angle + 180, angle)
  )

# ── 6. Plot ───────────────────────────────────────────────────────────────────
max_value <- max(data$value, na.rm = TRUE)

p <- ggplot(data, aes(x = as.factor(id), y = value, fill = region)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.9) +

  # Value labels on bars
  geom_text(
    data = label_data %>% filter(!is.na(value)),
    aes(x = id, y = value + max_value * 0.05, label = value, fill = NULL),
    color = "black",
    size = 2.5,
    fontface = "bold"
  ) +

  # ── Circle backgrounds for flags ────────────────────────────────────────────
  geom_point(
    data = country_labels %>% filter(!is.na(flag_url)),
    aes(x = id, y = value + max_value * 0.25, fill = NULL),
    shape = 21,
    size = 14, # ← Adjust circle size (try 10-20)
    fill = "white",
    color = "gray70",
    stroke = 1
  ) +

  # ── FLAGS using geom_image (will be square but on circular background) ──────
  geom_image(
    data = country_labels %>% filter(!is.na(flag_url)),
    aes(x = id, y = value + max_value * 0.25, image = flag_url, fill = NULL),
    size = 0.04, # ← Keep smaller than circle background
    asp = 1.5
  ) +

  # Region labels along inner circle
  geom_text(
    data = region_labels,
    aes(
      x = mid_id,
      y = -max_value * 0.05,
      label = region,
      angle = angle,
      hjust = hjust,
      group = NULL,
      fill = NULL
    ),
    color = "black",
    fontface = "bold",
    size = 4.5,
    inherit.aes = FALSE
  ) +

  ylim(-max_value * 0.35, max_value * 1.6) +

  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(1.5, 4), "cm"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  coord_polar(start = 0) +
  labs(title = "Exhibitors by Country - CPHI JP 2026")

p
