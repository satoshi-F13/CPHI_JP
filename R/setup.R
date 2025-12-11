# R/setup.R
# Load all required libraries
library(tidyverse)
library(DT)
library(plotly)
library(scales)
library(knitr)
library(kableExtra)
library(treemapify)
library(flextable)
library(officer)
library(leaflet)

# SATOM Brand Colors
brand_colors <- c(
  primary = "#7093BF",
  secondary = "#D3D3D2",
  success = "#97CEA1",
  warning = "#F7B981",
  danger = "#CB6874",
  dark = "#375A9D",
  navy = "#323172"
)

# CPHI color palette
cphi_colors <- unname(c(
  brand_colors["primary"],
  brand_colors["danger"],
  brand_colors["warning"],
  brand_colors["success"],
  brand_colors["dark"]
))
