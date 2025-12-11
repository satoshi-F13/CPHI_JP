# CPHI Japan Exhibition Analysis Dashboard

> Interactive analytics dashboard for CPHI Japan pharmaceutical exhibition (2023-2025)

## Quick Start
```bash
# Clone repository
git clone https://github.com/satoshi-F13/cphi-jp2.git

# Install R packages
Rscript -e "install.packages(c('tidyverse', 'plotly', 'DT', 'leaflet', 'flextable'))"

# Render dashboard
quarto render dashboard.qmd
```

## Features

-  Multi-year trend analysis
-  Geographic distribution maps
-  Visitor & exhibitor demographics
-  Interactive data tables
-  Venue location map

## Built With

Quarto Dashboard | R | ggplot2 | plotly | leaflet

## Structure
```
├── dashboard.qmd          # Main dashboard
├── R/                     # Helper functions
├── data/                  # CSV data files
└── custom.scss           # Styling
```

##  Key Metrics (2025)

- **793** Exhibitors (+10.1%)
- **34,128** Visitors (+61.3%)
- **32** Countries
- **81%** Exhibitor Satisfaction

##  License

MIT License - SATOM OÜ

##  Contact

contact@satom.eu | [satom.eu](https://satom.eu/)

---

*Data source: CPHI Japan Post Show Report 2024*