# 01_import.R
# Purpose: Import raw data, select V2 variables, and classify regions as Urban vs Rural
# Authors: Group 1 (Ahmed Amasha & Begüm Akyüz)

# -------------------------------------------------------------------
# 0. Load libraries
# -------------------------------------------------------------------
library(tidyverse)
library(readr)

# -------------------------------------------------------------------
# 1. Import raw data
# -------------------------------------------------------------------
raw_data <- readr::read_csv(
  "data/CampusFile_WM_2022.csv",
  na = c("", "NA", "Not specified"),
  # standardizing decimal format to avoid import issues
  locale = readr::locale(decimal_mark = ".", grouping_mark = ""), 
  show_col_types = FALSE
)

# -------------------------------------------------------------------
# 2. Data cleaning and variable construction
# -------------------------------------------------------------------
cleaned_data <- raw_data %>%
  
  # A. Select variables required for the V2 outline
  select(
    rent_sqm,                 # Rent per square meter (€/m²) – Dependent variable
    energieeffizienzklasse,   # Energy efficiency class (A+ to H)
    
    # Location identifiers
    kid2019,                  # District ID (Fixed Effects)
    gid2019,                  # Municipality ID
    
    # Structural characteristics
    wohnflaeche,              # Living area (m²)
    zimmeranzahl,             # Number of rooms
    baujahr,                  # Year of construction
    letzte_modernisierung,    # Year of last modernization
    
    # Amenities (binary indicators)
    balkon,                   # Balcony
    aufzug,                   # Elevator
    einbaukueche,             # Fitted kitchen
    parkplatz,                # Parking space
    keller,                   # Basement / cellar
    gaestewc,                 # Guest toilet
    garten                    # Garden
  ) %>%
  
  # B. Urban vs Rural classification
  #    Logic: Single-municipality districts are urban (Kreisfreie Städte),
  #    multi-municipality districts are rural (Landkreise).
  group_by(kid2019) %>%
  mutate(
    municipality_count = n_distinct(gid2019),
    region_type = if_else(municipality_count == 1, "urban", "rural")
  ) %>%
  ungroup() %>%
  
  # C. Set Baseline for Energy Efficiency
  #    Setting "C" as the reference category for the regression model.
  mutate(
    energieeffizienzklasse = as.factor(energieeffizienzklasse),
    energieeffizienzklasse = relevel(energieeffizienzklasse, ref = "C")
  ) %>%
  
  # D. Log transformation & Filtering
  mutate(log_rent_sqm = log(rent_sqm)) %>%
  filter(
    rent_sqm > 0, 
    !is.na(energieeffizienzklasse)
  )

# -------------------------------------------------------------------
# 3. Save cleaned dataset
# -------------------------------------------------------------------
saveRDS(cleaned_data, "data/cleaned_data.rds")

# -------------------------------------------------------------------
# 4. Verification checks
# -------------------------------------------------------------------
message("Data cleaning finished successfully.")

# Check Urban vs Rural distribution
table(cleaned_data$region_type)

# Verify factor levels (first level indicates baseline)
print(levels(cleaned_data$energieeffizienzklasse))