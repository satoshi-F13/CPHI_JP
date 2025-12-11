# R/plot_functions.R

# Function: Create trend line plot
plot_trend_line <- function(
  data,
  x_var,
  y_var,
  color,
  title = "",
  y_label = ""
) {
  p <- ggplot(data, aes(x = factor({{ x_var }}), y = {{ y_var }}, group = 1)) +
    geom_line(color = color, linewidth = 1.5) +
    geom_point(size = 5, color = color) +
    geom_text(
      aes(label = format({{ y_var }}, big.mark = ",")),
      vjust = -1.2,
      size = 8,
      fontface = "bold",
      color = brand_colors["navy"]
    ) +
    labs(
      title = title,
      x = "Year",
      y = y_label
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0.02, 0.15)) # 2% bottom, 15% top
    ) +
    theme_satom()

  return(p)
}

# Function: Create horizontal bar chart with plotly
plot_country_bars <- function(data, top_n = 10, max_value = 350) {
  top_data <- data %>%
    arrange(desc(exhibitors)) %>%
    head(top_n) %>%
    mutate(country = fct_reorder(country, exhibitors))

  plot_ly(
    data = top_data,
    x = ~exhibitors,
    y = ~ reorder(country, exhibitors),
    type = "bar",
    orientation = "h",
    color = ~region,
    colors = unname(cphi_colors),
    text = ~exhibitors,
    textposition = "outside",
    textfont = list(size = 14, color = brand_colors["navy"]),
    hovertext = ~ paste0(
      "<b>",
      country,
      "</b><br>",
      "Exhibitors: ",
      exhibitors,
      "<br>",
      "Region: ",
      region
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(
        title = "Number of Exhibitors",
        range = c(0, max_value)
      ),
      yaxis = list(title = ""),
      legend = list(
        title = list(text = "Region"),
        orientation = "h",
        y = 1.15,
        x = 0.5,
        xanchor = "center"
      ),
      margin = list(l = 0, r = 0, t = 10, b = 0),
      font = list(family = "Noto Sans", size = 12)
    ) %>%
    config(displaylogo = FALSE)
}

# Function: Create treemap
plot_regional_treemap <- function(data) {
  regional_summary <- data %>%
    group_by(region) %>%
    summarise(total = sum(exhibitors), .groups = "drop") %>%
    arrange(desc(total)) %>%
    mutate(
      percentage = (total / sum(total)) * 100,
      label = paste0(region, "\n", total, " (", round(percentage, 1), "%)")
    )

  plot_ly(
    data = regional_summary,
    type = "treemap",
    labels = ~region,
    parents = "",
    values = ~total,
    text = ~ paste0(
      total,
      " exhibitors<br>",
      round(percentage, 1),
      "%"
    ),
    textposition = "middle center",
    textfont = list(size = 16, color = "white", family = "Alata"),
    marker = list(
      colors = unname(cphi_colors),
      line = list(color = "white", width = 2)
    )
  ) %>%
    layout(
      margin = list(l = 0, r = 0, t = 0, b = 0),
      font = list(family = "Noto Sans")
    ) %>%
    config(displaylogo = FALSE)
}

# Function: Create styled flextable
create_styled_table <- function(data, highlight_top = 5) {
  data %>%
    flextable() %>%
    bg(bg = brand_colors["primary"], part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%
    bg(i = seq_len(highlight_top), bg = "#E8F4F8") %>%
    color(i = 1:3, j = 1, color = brand_colors["primary"]) %>%
    bold(i = 1:3, j = 1) %>%
    color(j = "Region", color = brand_colors["navy"]) %>%
    border_remove() %>%
    hline_top(
      border = fp_border(color = brand_colors["primary"], width = 2),
      part = "header"
    ) %>%
    hline_bottom(
      border = fp_border(color = brand_colors["primary"], width = 2),
      part = "header"
    ) %>%
    hline_bottom(
      border = fp_border(color = "grey70", width = 1),
      part = "body"
    ) %>%
    align(align = "center", j = 1, part = "all") %>%
    align(align = "left", j = c(2, 3), part = "all") %>%
    width(j = 1, width = 0.8) %>%
    width(j = 2, width = 2) %>%
    width(j = 3, width = 1.5) %>%
    font(fontname = "Noto Sans", part = "all") %>%
    fontsize(size = 11, part = "body") %>%
    fontsize(size = 12, part = "header")
}

# Function: Create leaflet map
create_venue_map <- function(
  lat = 35.6298,
  lng = 139.7946,
  name = "Tokyo Big Sight",
  zoom = 15
) {
  location_data <- data.frame(lat = lat, lng = lng, name = name)

  leaflet(location_data, width = "100%", height = "100%") %>%
    addProviderTiles(providers$Esri.WorldStreetMap) %>%
    addMarkers(
      lng = ~lng,
      lat = ~lat,
      popup = ~ paste0(
        "<div style='text-align: center;'>",
        "<h4>",
        name,
        "</h4>",
        "<p>CPHI Japan Venue</p>",
        "</div>"
      )
    ) %>%
    setView(lng = lng, lat = lat, zoom = zoom) %>%
    addMiniMap(
      toggleDisplay = TRUE,
      tiles = providers$Esri.WorldStreetMap
    )
}
