library(tidyverse)

# Read the datasets
exhibitors <- read_csv("data/cphijp26_exhibitors.csv")
products <- read_csv("data/cphi_japan_2026_products_fixed.csv")
all_years <- read_csv("data/cphi_japan_all_years_combined.csv")
# Join the datasets by company name
# Using left_join to keep all exhibitors and add their products
joined_data <- exhibitors %>%
  left_join(products, by = c("company_name" = "Company_Name"))

# Alternative: inner_join to keep only companies that exist in both datasets
# joined_data_inner <- exhibitors %>%
#   inner_join(products, by = c("company_name" = "Company_Name"))

# # Alternative: full_join to keep all records from both datasets
# joined_data_full <- exhibitors %>%
#   full_join(products, by = c("company_name" = "Company_Name"))

# View the result
glimpse(joined_data)

# Handling NA values
# Drop Booth_info
joined_data <-
  joined_data |>
  select(!Booth_Info)


# Handle NA values
cleaned_data <- joined_data %>%
  mutate(
    # Replace NA in show_type with "uncategorized"
    show_type = replace_na(show_type, "uncategorized"),

    # Replace NA in company_url with "not provided"
    company_url = replace_na(company_url, "not provided"),

    # Replace NA in Product_Name with "Contact companies"
    Product_Name = replace_na(Product_Name, "Contact companies"),
  )

library(janitor)

# Method 1: Standardize company names for exact matching
# Create standardized versions of company names
joined_data_clean <- joined_data %>%
  mutate(
    company_name_std = company_name %>%
      str_to_lower() %>% # Convert to lowercase
      str_remove_all("[.,]") %>% # Remove dots and commas
      str_squish()
  ) # Remove extra whitespace

all_years_clean <- all_years %>%
  mutate(
    company_name_std = company_name %>%
      str_to_lower() %>%
      str_remove_all("[.,]") %>%
      str_squish()
  )

# Join by standardized names
result_exact <- joined_data_clean %>%
  left_join(
    all_years_clean,
    by = "company_name_std",
    suffix = c("", "_historical")
  )

result_exact <-
  result_exact |>
  select(
    booth_number,
    company_name,
    show_type,
    Product_Name,
    company_url,
    country,
    country_code,
    country_code_lower,
    flag_url
  ) |>
  clean_names()

# Check NAs in the country
unknown_countries <-
  result_exact |>
  filter(is.na(country)) |>
  select(company_name)

write_csv(unknown_countries, "data/unknown_countries.csv")

na_companies <- read_csv(file = "data/company_countries_final.csv")
# rename as country_code
na_companies <-
  na_companies |>
  rename(country_code = country)

write_csv(na_companies, "data/na_companies.csv")

result_exact <-
  result_exact %>%
  left_join(na_companies, by = "company_name", suffix = c("", "_new")) %>%
  mutate(
    country_code = coalesce(country_code, country_code_new),
    # Update derived columns if needed
    country_code_lower = if_else(
      is.na(country_code_lower),
      str_to_lower(country_code_new),
      country_code_lower
    )
  ) %>%
  select(-country_code_new) |> # Remove the temporary joined column
  mutate(
    # Replace NA in Product_Name with "Contact companies"
    product_name = replace_na(product_name, "not provided"),
    # Replace NA in company_url with "not provided"
    company_url = replace_na(company_url, "not provided"),
  )


result_exact <-
  result_exact %>%
  mutate(
    # Fill country: uppercase country_code and replace common codes
    country = if_else(
      is.na(country),
      case_when(
        country_code == "IN" ~ "INDIA",
        country_code == "CN" ~ "CHINA",
        country_code == "JP" ~ "JAPAN",
        country_code == "US" ~ "USA",
        country_code == "UK" | country_code == "GB" ~ "UNITED KINGDOM",
        TRUE ~ str_to_upper(country_code)
      ),
      country
    ),

    # Fill flag_url using country_code_lower
    flag_url = if_else(
      is.na(flag_url) & !is.na(country_code_lower),
      paste0("https://flagcdn.com/w40/", country_code_lower, ".png"),
      flag_url
    )
  )

result_exact <-
  result_exact |>
  mutate(
    country = case_when(
      country == "AE" ~ "UAE",
      country == "BE" ~ "BELGIUM",
      country == "CA" ~ "CANADA",
      country == "CH" ~ "SWITZERLAND",
      country == "ES" ~ "SPAIN",
      country == "FI" ~ "FINLAND",
      country == "FR" ~ "FRANCE",
      country == "HK" ~ "HONG KONG",
      country == "IT" ~ "ITALY",
      country == "KR" ~ "SOUTH KOREA",
      country == "NL" ~ "NETHERLANDS",
      country == "PT" ~ "PORTUGAL",
      country == "RU" ~ "RUSSIAN FEDERATION",
      country == "SK" ~ "SLOVAKIA",
      country == "TH" ~ "THAILAND",
      country == "TW" ~ "TAIWAN",
      TRUE ~ country # This keeps all other values unchanged
    )
  )
# Save results
write_csv(result_exact, "data/exhibitors_cphijp2026.csv")


# Load necessary library
library(tidyverse)

# Read the data
exhibitors <- result_exact

# Basic overview
glimpse(exhibitors)
str(exhibitors)
summary(exhibitors)

# Dimensions
dim(exhibitors)
nrow(exhibitors)
ncol(exhibitors)

# Column names
colnames(exhibitors)

# First and last few rows
head(exhibitors)
tail(exhibitors)

# Check for missing values
colSums(is.na(exhibitors))
sum(is.na(exhibitors))

# Unique values count for each column
sapply(exhibitors, n_distinct)

# Frequency tables for categorical variables
table(exhibitors$show_type)
table(exhibitors$country)
table(exhibitors$country_code)

# Using dplyr for more detailed summaries
exhibitors %>% count(show_type, sort = TRUE)
exhibitors %>% count(country, sort = TRUE)
exhibitors %>% count(product_name, sort = TRUE)

# Cross-tabulation
exhibitors %>% count(country, show_type)

# Summary statistics by group
exhibitors %>%
  group_by(show_type) %>%
  summarise(
    n_exhibitors = n(),
    n_countries = n_distinct(country),
    n_products = n_distinct(product_name)
  )

exhibitors %>%
  group_by(country) %>%
  summarise(
    n_exhibitors = n(),
    n_show_types = n_distinct(show_type)
  ) %>%
  arrange(desc(n_exhibitors))
