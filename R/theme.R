# R/theme.R
# Custom ggplot2 theme
theme_satom <- function() {
  theme_minimal(base_size = 12, base_family = "Noto Sans") +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        family = "Alata",
        color = brand_colors["navy"]
      ),
      plot.subtitle = element_text(
        size = 12,
        color = brand_colors["dark"],
        margin = margin(b = 10)
      ),
      axis.title = element_text(
        size = 10,
        color = brand_colors["navy"]
      ),
      legend.title = element_text(
        face = "bold",
        family = "Alata",
        size = 10
      ),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}
