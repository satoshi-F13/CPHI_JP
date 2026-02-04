library(tidyverse)
cphi_data <- read_csv("data/cphi_japan_all_years_combined.csv")


# ============================================================
# 1. BASIC DATASET OVERVIEW
# ============================================================
cat("=== BASIC DATASET OVERVIEW ===\n")
glimpse(cphi_data)

cat("\n=== DATASET DIMENSIONS ===\n")
cat("Rows:", nrow(cphi_data), "\n")
cat("Columns:", ncol(cphi_data), "\n")

cat("\n=== COLUMN NAMES ===\n")
print(names(cphi_data))

cat("\n=== DATA STRUCTURE ===\n")
str(cphi_data)

cat("\n=== FIRST FEW ROWS ===\n")
print(head(cphi_data))

# ============================================================
# 2. SUMMARY BY YEAR
# ============================================================
cat("\n\n=== SUMMARY BY YEAR ===\n")
year_summary <- cphi_data %>%
  group_by(year) %>%
  summarise(
    total_entries = n(),
    unique_companies = n_distinct(company_name_clean),
    unique_countries = n_distinct(country)
  ) %>%
  arrange(year)

print(year_summary)

# ============================================================
# 3. COUNTRY ANALYSIS
# ============================================================
cat("\n\n=== TOP 20 COUNTRIES BY ENTRIES ===\n")
country_summary <- cphi_data %>%
  count(country, country_code, name = "total_entries") %>%
  arrange(desc(total_entries)) %>%
  head(20)

print(country_summary)

cat("\n\n=== COUNTRY SUMMARY WITH UNIQUE COMPANIES ===\n")
country_detailed <- cphi_data %>%
  group_by(country) %>%
  summarise(
    total_entries = n(),
    unique_companies = n_distinct(company_name_clean),
    years_participating = paste(sort(unique(year)), collapse = ", "),
    n_years = n_distinct(year)
  ) %>%
  arrange(desc(total_entries)) %>%
  head(20)

print(country_detailed)

# ============================================================
# 4. COMPANY ANALYSIS
# ============================================================
cat("\n\n=== TOP 20 COMPANIES BY FREQUENCY ===\n")
top_companies <- cphi_data %>%
  count(company_name_clean, name = "appearances") %>%
  arrange(desc(appearances)) %>%
  head(20)

print(top_companies)

cat("\n\n=== COMPANIES WITH FULL DETAILS ===\n")
company_details <- cphi_data %>%
  group_by(company_name_clean) %>%
  summarise(
    total_appearances = n(),
    countries = paste(unique(country), collapse = ", "),
    years = paste(sort(unique(year)), collapse = ", "),
    n_years = n_distinct(year)
  ) %>%
  arrange(desc(total_appearances)) %>%
  head(20)

print(company_details)

# ============================================================
# 5. YEAR-OVER-YEAR TRENDS
# ============================================================
cat("\n\n=== ENTRIES BY YEAR AND COUNTRY (TOP 10 COUNTRIES) ===\n")
top_10_countries <- cphi_data %>%
  count(country, sort = TRUE) %>%
  head(10) %>%
  pull(country)

country_year_matrix <- cphi_data %>%
  filter(country %in% top_10_countries) %>%
  count(country, year) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
  arrange(desc(rowSums(select(., -country))))

print(country_year_matrix)

# ============================================================
# 6. MULTI-YEAR PARTICIPATION
# ============================================================
cat("\n\n=== COMPANIES APPEARING IN MULTIPLE YEARS ===\n")
multi_year_companies <- cphi_data %>%
  group_by(company_name_clean) %>%
  summarise(
    n_years = n_distinct(year),
    years = paste(sort(unique(year)), collapse = ", "),
    country = first(country)
  ) %>%
  filter(n_years > 1) %>%
  arrange(desc(n_years), company_name_clean)

cat("Total companies appearing in multiple years:", nrow(multi_year_companies), "\n")
cat("Companies in 3 years:", sum(multi_year_companies$n_years == 3), "\n")
cat("Companies in 2 years:", sum(multi_year_companies$n_years == 2), "\n\n")

print(head(multi_year_companies, 20))

# ============================================================
# 7. COUNTRY DIVERSITY BY YEAR
# ============================================================
cat("\n\n=== COUNTRY DIVERSITY BY YEAR ===\n")
country_diversity <- cphi_data %>%
  group_by(year) %>%
  summarise(
    total_countries = n_distinct(country),
    top_country = names(sort(table(country), decreasing = TRUE))[1],
    top_country_entries = max(table(country))
  )

print(country_diversity)

# ============================================================
# 8. NEW VS RETURNING ANALYSIS
# ============================================================
cat("\n\n=== NEW VS RETURNING COMPANIES BY YEAR ===\n")

# Companies by their first appearance
first_appearance <- cphi_data %>%
  group_by(company_name_clean) %>%
  summarise(first_year = min(year))

# Join back to see new vs returning
new_vs_returning <- cphi_data %>%
  left_join(first_appearance, by = "company_name_clean") %>%
  mutate(status = ifelse(year == first_year, "New", "Returning")) %>%
  group_by(year, status) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = status, values_from = count, values_fill = 0)

print(new_vs_returning)

# ============================================================
# 9. STATISTICAL SUMMARY
# ============================================================
cat("\n\n=== STATISTICAL SUMMARY ===\n")

# Overall statistics
cat("\nOVERALL STATISTICS:\n")
cat("Total records:", nrow(cphi_data), "\n")
cat("Unique companies:", n_distinct(cphi_data$company_name_clean), "\n")
cat("Unique countries:", n_distinct(cphi_data$country), "\n")
cat("Years covered:", paste(sort(unique(cphi_data$year)), collapse = ", "), "\n")

# Average entries per company
avg_entries <- cphi_data %>%
  count(company_name_clean) %>%
  summarise(
    mean_entries = mean(n),
    median_entries = median(n),
    max_entries = max(n)
  )

cat("\nENTRIES PER COMPANY:\n")
cat("Mean:", round(avg_entries$mean_entries, 2), "\n")
cat("Median:", avg_entries$median_entries, "\n")
cat("Max:", avg_entries$max_entries, "\n")

# ============================================================
# 10. MISSING DATA CHECK
# ============================================================
cat("\n\n=== MISSING DATA ANALYSIS ===\n")
missing_data <- cphi_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
  mutate(
    total_rows = nrow(cphi_data),
    missing_percent = round((missing_count / total_rows) * 100, 2)
  )

print(missing_data)

# ============================================================
# 11. EXPORT SUMMARY TABLES
# ============================================================
cat("\n\n=== EXPORTING SUMMARY TABLES ===\n")

write_csv(year_summary, "summary_by_year.csv")
write_csv(country_detailed, "summary_by_country.csv")
write_csv(company_details, "top_companies_detailed.csv")
write_csv(country_year_matrix, "country_year_trends.csv")
write_csv(multi_year_companies, "multi_year_companies.csv")
write_csv(new_vs_returning, "new_vs_returning_by_year.csv")

cat("Summary tables exported successfully!\n")
cat("\nFiles created:\n")
cat("  - summary_by_year.csv\n")
cat("  - summary_by_country.csv\n")
cat("  - top_companies_detailed.csv\n")
cat("  - country_year_trends.csv\n")
cat("  - multi_year_companies.csv\n")
cat("  - new_vs_returning_by_year.csv\n")

cat("\n=== ANALYSIS COMPLETE ===\n")