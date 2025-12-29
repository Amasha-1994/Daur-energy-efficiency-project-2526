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

# Optional: Save a CSV copy for manual inspection
write_csv(df, "data/cleaned_data.csv")

# Safety Check: Ensure critical variables exist before analysis
stopifnot(
  "region_type" %in% names(df),
  "rent_sqm" %in% names(df),
  "wohnflaeche" %in% names(df)
)

# -------------------------------------------------------------------
# 2. Variable Construction (Analysis Specific)
# -------------------------------------------------------------------
# Define energy efficiency indicator
# We classify "APLUS", "A", and "B" as energy efficient (Green).
# "C" through "H" are classified as standard/inefficient.
df <- df %>%
  mutate(
    # Clean text to uppercase just in case
    energy_class_std = toupper(trimws(as.character(energieeffizienzklasse))),
    
    # Create Binary Indicator (1 = Efficient, 0 = Not Efficient)
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
    avg_rent_sqm = mean(rent_sqm, na.rm = TRUE),
    avg_wohnflaeche = mean(wohnflaeche, na.rm = TRUE),
    share_energy_efficient = mean(energy_efficient, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 4. Save and Print Results
# -------------------------------------------------------------------
write_csv(summary_urban_rural, "output/summary_urban_rural.csv")
saveRDS(summary_urban_rural, "output/summary_urban_rural.rds")

# Print to console for immediate verification
print(summary_urban_rural)