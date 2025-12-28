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
  locale = readr::locale(decimal_mark = ".", grouping_mark = ""),
  show_col_types = FALSE
)


# -------------------------------------------------------------------
# 2. Data cleaning and variable construction
# -------------------------------------------------------------------
cleaned_data <- raw_data %>%
  
  # A. Select only variables required for the V2 outline
  select(
    rent_sqm,
    # Rent per square meter (€/m²) – dependent variable
    energieeffizienzklasse,
    # Energy efficiency class (A+ to H)
    
    # Location identifiers
    kid2019,
    # District ID (Kreiskennung, fixed effects)
    gid2019,
    # Municipality ID (Gemeindekennung)
    
    # Structural characteristics
    wohnflaeche,
    # Living area (m²)
    zimmeranzahl,
    # Number of rooms
    baujahr,
    # Year of construction
    letzte_modernisierung,
    # Year of last modernization
    
    # Amenities (binary indicators)
    balkon,
    # Balcony
    aufzug,
    # Elevator
    einbaukueche,
    # Fitted kitchen
    parkplatz,
    # Parking space
    keller,
    # Basement / cellar
    gaestewc,
    # Guest toilet
    garten                    # Garden
  ) %>%
  
  # B. Urban vs Rural classification
  #    German administrative context:
  #    - Districts with only one municipality are typically
  #      "kreisfreie Städte" (independent cities → urban)
  #    - Districts with multiple municipalities are classified as rural
  group_by(kid2019) %>%
  mutate(
    municipality_count = n_distinct(gid2019),
    region_type = if_else(municipality_count == 1, "urban", "rural")
  ) %>%
  ungroup() %>%
  
  # C. Log transformation of rent per square meter
  mutate(log_rent_sqm = log(rent_sqm)) %>%
  
  # D. Remove invalid or unusable observations
  filter(rent_sqm > 0, !is.na(energieeffizienzklasse))

# -------------------------------------------------------------------
# 3. Save cleaned dataset
# -------------------------------------------------------------------
saveRDS(cleaned_data, "data/cleaned_data.rds")

# -------------------------------------------------------------------
# 4. Verification checks
# -------------------------------------------------------------------
message("Data cleaning finished successfully.")

# Urban vs Rural distribution
table(cleaned_data$region_type)

# Quick sanity checks
summary(cleaned_data$rent_sqm)
glimpse(cleaned_data)
