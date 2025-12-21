# 01_import.R
# Purpose: Load data, clean variable names, and create log-transformed rent
# Authors: Group 1 (Ahmed & Begüm)

# 1. Load Libraries -------------------------------------------------------
library(tidyverse)
library(readr)

# 2. Import Data ----------------------------------------------------------
# This reads the large file (241 MB).
# show_col_types = FALSE hides the long message about column types in the console.
raw_data <- read_csv("data/CampusFile_WM_2022.csv", show_col_types = FALSE)

# 3. Tidy & Transform -----------------------------------------------------
cleaned_data <- raw_data %>%
  # Select the variables mentioned in your outline
  select(
    rent_sqm,                 # Outcome Variable
    energieeffizienzklasse,   # Independent Variable (Energy Label)
    wohnflaeche,              # Control 1: Living Space (sqm)
    baujahr,                  # Control 2: Construction Year
    gid2019                   # Control 3: District ID (Fixed Effects)
  ) %>%
  
  # CRITICAL: Create the Natural Log (ln) variable for your model
  mutate(
    log_rent_sqm = log(rent_sqm)
  ) %>%
  
  # Filter out invalid data
  filter(
    rent_sqm > 0,
    !is.na(energieeffizienzklasse),
    energieeffizienzklasse != "Not specified"  # Remove rows with unknown labels
  )

# 4. Save Processed Data --------------------------------------------------
# Save as an .rds file so your partner can load it easily in the next script
saveRDS(cleaned_data, "data/cleaned_data.rds")

# 5. Verification ---------------------------------------------------------
print("Success! Data loaded, cleaned, and filtered.")

# Check the energy labels to ensure "Not specified" is gone
print("Count of Energy Labels:")
table(cleaned_data$energieeffizienzklasse)

# Check the first few rows
head(cleaned_data)