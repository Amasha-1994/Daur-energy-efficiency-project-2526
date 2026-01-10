# 03_regression.R
# Purpose: Estimate Hedonic Price Regression (The "Green Premium")
# Authors: Group 1 (Ahmed Amasha & Begüm Akyüz)

# -------------------------------------------------------------------
# 0. Setup and Libraries
# -------------------------------------------------------------------
rm(list = ls())

# Auto-install packages if missing
if (!require("modelsummary")) install.packages("modelsummary")
if (!require("fixest")) install.packages("fixest")
if (!require("tidyverse")) install.packages("tidyverse")

# Load libraries
library(tidyverse)
library(modelsummary) 
library(fixest)       

# Create output folder
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

# -------------------------------------------------------------------
# 1. Load Data
# -------------------------------------------------------------------
# Load master dataset
df <- readRDS("data/cleaned_data.rds")

# Verify reference category (Baseline should be "C")
message("Regression Baseline Category:")
print(levels(df$energieeffizienzklasse))

# -------------------------------------------------------------------
# 2. Run Regression Models
# -------------------------------------------------------------------

# Model 1: Baseline Hedonic Regression
# Estimates price premiums for energy efficiency levels relative to Class C.
# Note: We use Clustered Standard Errors (~kid2019) rather than Fixed Effects (|kid2019)
# to ensure the 'region_type' variable is not absorbed by the district dummies.
model_base <- feols(
  log_rent_sqm ~ energieeffizienzklasse + wohnflaeche + zimmeranzahl + 
    baujahr + letzte_modernisierung + 
    balkon + aufzug + einbaukueche + parkplatz + 
    region_type, 
  data = df,
  cluster = ~kid2019 
)

# Model 2: Interaction Model
# Estimates differential effects of energy efficiency in Urban vs. Rural regions.
model_interaction <- feols(
  log_rent_sqm ~ energieeffizienzklasse * region_type + 
    wohnflaeche + zimmeranzahl + baujahr + letzte_modernisierung + 
    balkon + aufzug + einbaukueche + parkplatz,
  data = df,
  cluster = ~kid2019
)

# -------------------------------------------------------------------
# 3. View Results
# -------------------------------------------------------------------
message("\n--- Model 1 Summary (Base) ---")
print(summary(model_base))

message("\n--- Model 2 Summary (Interaction) ---")
print(summary(model_interaction))

# -------------------------------------------------------------------
# 4. Save Results (With Professional Formatting)
# -------------------------------------------------------------------

# Create a dictionary to rename variables for the final table
# This makes the output much easier for non-technical readers to understand.
coef_mapping <- c(
  "energieeffizienzklasseAPLUS" = "Energy Class A+",
  "energieeffizienzklasseA"     = "Energy Class A",
  "energieeffizienzklasseB"     = "Energy Class B",
  "energieeffizienzklasseD"     = "Energy Class D",
  "energieeffizienzklasseE"     = "Energy Class E",
  "energieeffizienzklasseF"     = "Energy Class F",
  "energieeffizienzklasseG"     = "Energy Class G",
  "energieeffizienzklasseH"     = "Energy Class H",
  "region_typeurban"            = "Urban Region",
  "wohnflaeche"                 = "Living Area (sqm)",
  "zimmeranzahl"                = "Rooms",
  "baujahr"                     = "Year Built",
  "letzte_modernisierung"       = "Last Modernization",
  "(Intercept)"                 = "Constant"
)

# Export regression table to CSV
modelsummary(
  list("Base Model" = model_base, "Interaction Model" = model_interaction),
  output = "output/regression_results.csv",
  stars = TRUE,
  coef_map = coef_mapping, # Applies the readable names above
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Impact of Energy Efficiency on Rent Prices (Ref: Class C)"
)

message("Regression analysis complete. Results saved to 'output/regression_results.csv'.")