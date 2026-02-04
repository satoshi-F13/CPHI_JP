# R Script to Extract Product Information from CPHI Japan 2026 PDF - FIXED VERSION
# Fixes:
# 1. Booth_Info: Ensure format is "digit+letter-digit+"
# 2. Company names: Include full names with Ltd., Ltd, Co., Inc., etc.
# 3. Country: Extract only 2-letter country codes

# Required libraries
# install.packages("pdftools")
# install.packages("stringr")
# install.packages("dplyr")

library(pdftools)
library(stringr)
library(dplyr)

# Read the PDF file
pdf_path <- "/mnt/user-data/uploads/CPHI_Japan_2026_ProductList.pdf"
pdf_text <- pdf_text(pdf_path)

# Combine all pages into one text
all_text <- paste(pdf_text, collapse = "\n")

# Split into lines and remove empty lines
lines <- str_split(all_text, "\n")[[1]]
lines <- str_trim(lines[lines != ""])

# Initialize empty data frame
products_df <- data.frame(
  Product_Name = character(),
  Company_Name = character(),
  Booth_Info = character(),
  Country = character(),
  stringsAsFactors = FALSE
)

# Define improved patterns
# Booth pattern: digits + letters + hyphen + digits
booth_pattern <- "\\b(\\d+[A-Z]+-\\d+)\\b"

# Valid 2-letter country codes
valid_country_codes <- c("CN", "IN", "JP", "US", "GB", "DE", "FR", "IT", "KR", 
                         "TW", "CH", "PT", "ES", "NL", "BE", "AT", "SE", "HU", 
                         "FI", "RO", "TR", "TH", "HK")

# Skip patterns
skip_patterns <- c("Featured products", "CPHI", "EXHIBIT", "REGISTER NOW",
                   "Click the flag", "Product\\(s\\) found", "21 Apr 2026",
                   "East Halls", "Tokyo Big Sight", "See all upcoming",
                   "See digital marketing", "Home", "Contact us", "Follow us",
                   "Copyright", "Informa Markets", "Accessibility", "Privacy Policy",
                   "Terms of Use", "Visitor Terms")

# Extract products
i <- 1
while (i <= length(lines) - 2) {
  current_line <- lines[i]
  
  # Skip unwanted lines
  should_skip <- FALSE
  for (pattern in skip_patterns) {
    if (grepl(pattern, current_line, ignore.case = TRUE)) {
      should_skip <- TRUE
      break
    }
  }
  
  if (should_skip) {
    i <- i + 1
    next
  }
  
  # Check if next line contains booth + country pattern
  next_line <- lines[i + 1]
  
  # Extract booth from next line (ensure proper format)
  booth_match <- str_extract(next_line, booth_pattern)
  
  # Extract country code (must be exactly 2 letters and in valid list)
  country <- NA
  country_matches <- str_extract_all(next_line, "\\b[A-Z]{2}\\b")[[1]]
  for (cc in country_matches) {
    if (cc %in% valid_country_codes) {
      country <- cc
      break
    }
  }
  
  # If we found both booth and country in the next line,
  # current line is product name and we need to collect company name
  if (!is.na(booth_match) && !is.na(country) && i + 2 <= length(lines)) {
    product_name <- current_line
    
    # Collect company name (may span multiple lines)
    company_lines <- c()
    j <- i + 2
    
    while (j <= length(lines) && length(company_lines) < 3) {
      line <- lines[j]
      
      # Check if this line is followed by a booth+country line
      if (j + 1 <= length(lines)) {
        next_check <- lines[j + 1]
        has_booth <- !is.na(str_extract(next_check, booth_pattern))
        
        country_check <- str_extract_all(next_check, "\\b[A-Z]{2}\\b")[[1]]
        has_country <- any(country_check %in% valid_country_codes)
        
        if (has_booth && has_country) {
          # This is the start of the next product
          break
        }
      }
      
      # Skip if it's a skip pattern
      skip_this <- FALSE
      for (pattern in skip_patterns) {
        if (grepl(pattern, line, ignore.case = TRUE)) {
          skip_this <- TRUE
          break
        }
      }
      if (skip_this) break
      
      # Skip if line looks like booth+country
      line_booth <- str_extract(line, booth_pattern)
      line_countries <- str_extract_all(line, "\\b[A-Z]{2}\\b")[[1]]
      line_has_country <- any(line_countries %in% valid_country_codes)
      if (!is.na(line_booth) && line_has_country) {
        break
      }
      
      company_lines <- c(company_lines, line)
      j <- j + 1
    }
    
    # Join company name parts
    company_name <- paste(company_lines, collapse = " ")
    company_name <- str_trim(company_name)
    
    # Add to dataframe if company name is valid
    if (nchar(company_name) > 0 && !grepl("^\\d", company_name)) {
      products_df <- rbind(products_df, data.frame(
        Product_Name = product_name,
        Company_Name = company_name,
        Booth_Info = booth_match,
        Country = country,
        stringsAsFactors = FALSE
      ))
      i <- j  # Move to position after company name
    } else {
      i <- i + 1
    }
  } else {
    i <- i + 1
  }
}

# Display summary
cat(paste0("\nTotal products extracted: ", nrow(products_df), "\n"))
cat("\nFirst 10 products:\n")
print(head(products_df, 10))

# Save to CSV
output_file <- "/mnt/user-data/outputs/cphi_japan_2026_products_r_fixed.csv"
write.csv(products_df, output_file, row.names = FALSE, fileEncoding = "UTF-8")

cat(paste0("\nData saved to: ", output_file, "\n"))

# Display country distribution
cat("\nProducts by country:\n")
country_table <- table(products_df$Country)
print(sort(country_table, decreasing = TRUE))

# Show sample booth formats
cat("\nSample booth formats:\n")
booth_samples <- head(unique(products_df$Booth_Info), 10)
print(booth_samples)

# Show sample company names with Ltd/Inc/Co
cat("\nSample company names with Ltd/Inc/Co:\n")
ltd_companies <- products_df$Company_Name[grepl("Ltd|Inc|Co\\.", products_df$Company_Name)]
print(head(ltd_companies, 10))
