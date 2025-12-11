# R/data_processing.R

# Function: Load all datasets
load_cphi_data <- function(data_dir = "data") {
  list(
    cphi_main = read_csv(file.path(data_dir, "cphi_japan_data.csv")),
    visitor_demo_2024 = read_csv(file.path(
      data_dir,
      "cphi_2024_visitor_demographics.csv"
    )),
    visitor_countries_2024 = read_csv(file.path(
      data_dir,
      "cphi_2024_visitor_countries.csv"
    )),
    exhibitor_data_2024 = read_csv(file.path(
      data_dir,
      "cphi_2024_exhibitor_data.csv"
    )),
    exhibitor_countries_2024 = read_csv(file.path(
      data_dir,
      "cphi_2024_exhibitor_countries.csv"
    )),
    business_categories_2024 = read_csv(file.path(
      data_dir,
      "cphi_2024_business_categories.csv"
    )),
    exhibitor_countries_2025 = read_csv(file.path(
      data_dir,
      "cphi_2025_exhibitor_countries.csv"
    )),
    pavilions = read_csv(file.path(data_dir, "cphi_pavilions.csv"))
  )
}

# Function: Calculate growth metrics
calculate_growth_metrics <- function(current, previous) {
  ((current - previous) / previous) * 100
}
