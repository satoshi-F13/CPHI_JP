#!/usr/bin/env Rscript
# ============================================================================
# Extract Exhibitor List from CPhI Japan Website
# ============================================================================
# This script extracts company names, booth numbers, and show types
# from the CPhI Japan exhibitor list page
# ============================================================================

# Install packages if needed (uncomment to install)
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("stringr")

# Load required libraries
library(rvest)
library(dplyr)
library(stringr)

# ============================================================================
# Configuration
# ============================================================================
url <- "https://www.informa-japan.com/cphifcj/complist/en/index.php?kbntype=1"
output_file <- "data/cphijp26_exhibitors.csv"

# ============================================================================
# Fetch and Parse HTML
# ============================================================================
cat("Fetching webpage...\n")
page <- read_html(url)

cat("Parsing HTML content...\n")

# Extract all list items containing exhibitor information
exhibitor_items <- page %>% html_nodes("ul li")

# Initialize data storage
booth_numbers <- c()
show_types <- c()
company_names <- c()
company_urls <- c()

# ============================================================================
# Extract Data from Each List Item
# ============================================================================
cat("Extracting exhibitor data...\n")

show_type_options <- c(
  "Ingredients",
  "Outsourcing",
  "Bio Pharma",
  "Machinery/Equipment",
  "Formulation ＆ Packaging"
)

for (item in exhibitor_items) {
  # Get full text of the item
  full_text <- item %>% html_text(trim = TRUE)

  # Skip empty items
  if (nchar(full_text) == 0) {
    next
  }

  # Extract booth number (pattern: letters/numbers followed by hyphen and numbers)
  # Examples: 2C-05, 3A-24, 1D-38, IT-16, TBD
  booth_match <- str_extract(full_text, "^[A-Z0-9]+-[0-9]+|^IT-[0-9]+|^TBD")
  booth_number <- ifelse(is.na(booth_match), "", booth_match)

  # Extract show type
  show_type <- ""
  for (st in show_type_options) {
    if (grepl(st, full_text, fixed = TRUE)) {
      show_type <- st
      break
    }
  }

  # Extract company name from link
  company_link <- item %>% html_node("a")

  if (!is.na(company_link)) {
    # Company name from link text
    company_name <- company_link %>% html_text(trim = TRUE)
    # Company URL
    company_url <- company_link %>% html_attr("href")
    company_url <- ifelse(is.na(company_url), "", company_url)
  } else {
    # If no link, try to extract company name from remaining text
    # Remove booth number and show type from text
    remaining_text <- full_text
    if (booth_number != "") {
      remaining_text <- str_remove(
        remaining_text,
        paste0("^", fixed(booth_number), "\\s*")
      )
    }
    if (show_type != "") {
      remaining_text <- str_remove(
        remaining_text,
        paste0("^", fixed(show_type), "\\s*")
      )
    }
    company_name <- str_trim(remaining_text)
    company_url <- ""
  }

  # Only store if we have a company name
  if (nchar(company_name) > 0) {
    booth_numbers <- c(booth_numbers, booth_number)
    show_types <- c(show_types, show_type)
    company_names <- c(company_names, company_name)
    company_urls <- c(company_urls, company_url)
  }
}

# ============================================================================
# Create DataFrame
# ============================================================================
exhibitors_df <- data.frame(
  booth_number = booth_numbers,
  show_type = show_types,
  company_name = company_names,
  company_url = company_urls,
  stringsAsFactors = FALSE
)

# Remove any duplicate entries
exhibitors_df <-
  exhibitors_df %>%
  distinct() |>
  filter(booth_number != "")


# Display Results

cat("Total exhibitors extracted:", nrow(exhibitors_df), "\n\n")

cat("First 10 exhibitors:\n")
print(head(exhibitors_df, 10))

cat("\n\nExhibitors by show type:\n")
show_type_summary <- table(exhibitors_df$show_type)
print(show_type_summary)

# Search for specific company (example: Hunan Huateng)
cat("\n\nSearching for 'Hunan Huateng Pharmaceutical':\n")
hunan_results <- exhibitors_df %>%
  filter(grepl("Hunan Huateng", company_name, ignore.case = TRUE))
if (nrow(hunan_results) > 0) {
  print(hunan_results)
} else {
  cat("Not found in the list.\n")
}

# ============================================================================
# Save to CSV
# ============================================================================
write.csv(exhibitors_df, output_file, row.names = FALSE, fileEncoding = "UTF-8")
