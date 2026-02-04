#!/usr/bin/env Rscript
# ============================================================================
# WORKING SOLUTION: Extract Products with RSelenium
# ============================================================================

# Install required packages (uncomment if needed):
# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("dplyr")

library(RSelenium)
library(rvest)
library(dplyr)

cat("Starting Selenium WebDriver...\n")

# Start Selenium (use Chrome)
rD <- rsDriver(browser = "chrome", 
               port = 4567L, 
               chromever = "latest",
               verbose = FALSE)

driver <- rD$client

cat("Navigating to product list page...\n")

# Navigate to the product list page
driver$navigate("https://exhibitors.cphi.com/live/cphi/event46v2.jsp?site=46&type=product&eventid=590")

# Wait for page to load
Sys.sleep(5)

cat("Waiting for products to load...\n")

# Scroll down to load more products (the page may use lazy loading)
for (i in 1:10) {
  driver$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(2)
}

# Click "Show more results" button if it exists
tryCatch({
  show_more <- driver$findElements(using = "css selector", value = ".paging .button")
  if (length(show_more) > 0) {
    cat("Clicking Show More button...\n")
    for (i in 1:20) {  # Click multiple times to load all products
      tryCatch({
        show_more[[1]]$clickElement()
        Sys.sleep(2)
      }, error = function(e) {
        cat("No more products to load\n")
        break
      })
    }
  }
}, error = function(e) {})

cat("Extracting product data from rendered page...\n")

# Get the page source after JavaScript has rendered everything
page_source <- driver$getPageSource()[[1]]
page <- read_html(page_source)

# Find all product containers
# Based on the HTML you showed: div.exhibitor__title contains product and company
product_containers <- page %>% 
  html_nodes("div.exhibitor__title")

cat("Found", length(product_containers), "products\n\n")

# Extract data from each product
products_list <- list()

for (i in seq_along(product_containers)) {
  container <- product_containers[[i]]
  
  # Extract product name from h3
  product_name <- container %>%
    html_node("h3") %>%
    html_text(trim = TRUE)
  
  # Extract company name
  company_name <- container %>%
    html_node("div.exh-company-name__text") %>%
    html_text(trim = TRUE)
  
  # Extract country if available
  # Look for country in parent or nearby elements
  parent_container <- container %>% html_node(xpath = "../..")
  country <- parent_container %>%
    html_node(".exh-country, .country, [data-country]") %>%
    html_text(trim = TRUE)
  
  if (is.na(country)) country <- ""
  
  if (!is.na(product_name) && product_name != "") {
    products_list[[i]] <- data.frame(
      product_name = product_name,
      company_name = ifelse(is.na(company_name), "", company_name),
      country = country,
      stringsAsFactors = FALSE
    )
  }
  
  # Progress indicator
  if (i %% 50 == 0) {
    cat("Processed", i, "/", length(product_containers), "products\n")
  }
}

# Close browser
driver$close()
rD$server$stop()

# Combine all products
products_df <- bind_rows(products_list)

cat("\n============================================================================\n")
cat("EXTRACTION COMPLETE\n")
cat("============================================================================\n\n")

cat("Total products extracted:", nrow(products_df), "\n")
cat("Products with company names:", sum(products_df$company_name != ""), "\n\n")

cat("First 10 products:\n")
print(head(products_df, 10))

# Search for Cholesterol
cat("\n\nSearching for Cholesterol:\n")
cholesterol <- products_df %>%
  filter(grepl("Cholesterol", product_name, ignore.case = TRUE))
if (nrow(cholesterol) > 0) {
  print(cholesterol)
}

# Save to CSV
write.csv(products_df, "cphi_products_with_company_final.csv", row.names = FALSE)

cat("\n\nSUCCESS! Data saved to: cphi_products_with_company_final.csv\n")

