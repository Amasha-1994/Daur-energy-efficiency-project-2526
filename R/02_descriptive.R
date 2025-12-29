# 02_descriptive.R
# Purpose: Generate Descriptive Statistics Table (Urban vs. Rural)
# Authors: Group 1 (Ahmed Amasha & Begüm Akyüz)

# -------------------------------------------------------------------
# 0. Setup and Libraries
# -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(readr)

# Create output folder if it does not exist
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

# -------------------------------------------------------------------
# 1. Load Data
# -------------------------------------------------------------------
# Load the cleaned dataset created in 01_import.R
df <- readRDS("data/cleaned_data.rds")

# Export CSV copy for inspection
write_csv(df, "data/cleaned_data.csv")

# Verify existence of critical variables
stopifnot(
  "region_type" %in% names(df),
  "rent_sqm" %in% names(df),
  "wohnflaeche" %in% names(df)
)

# -------------------------------------------------------------------
# 2. Variable Construction (Analysis Specific)
# -------------------------------------------------------------------
# Define energy efficiency indicator
# Logic: "APLUS", "A", and "B" are classified as efficient (1).
# "C" through "H" are classified as standard/inefficient (0).
df <- df %>%
  mutate(
    # Standardize text format
    energy_class_std = toupper(trimws(as.character(energieeffizienzklasse))),
    
    # Generate binary efficiency indicator
    energy_efficient = if_else(
      energy_class_std %in% c("APLUS", "A", "B"), 
      1L, 
      0L
    )
  )

# -------------------------------------------------------------------
# 3. Generate Summary Table (Urban vs Rural)
# -------------------------------------------------------------------
summary_urban_rural <- df %>%
  group_by(region_type) %>%
  summarise(
    n_obs = n(),
    
    # Calculate average rent (rounded to 2 decimals)
    avg_rent_sqm = round(mean(rent_sqm, na.rm = TRUE), 2),
    
    # Calculate average size (rounded to 1 decimal)
    avg_wohnflaeche = round(mean(wohnflaeche, na.rm = TRUE), 1),
    
    # Calculate percentage of efficient housing
    share_energy_efficient_pct = round(mean(energy_efficient, na.rm = TRUE) * 100, 1),
    
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 4. Save and Print Results
# -------------------------------------------------------------------
write_csv(summary_urban_rural, "output/summary_urban_rural.csv")
saveRDS(summary_urban_rural, "output/summary_urban_rural.rds")

# Display results
print(summary_urban_rural)