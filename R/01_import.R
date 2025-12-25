# 01_import.R
# Purpose: Load data, select V2 variables, and classify Urban vs. Rural
# Authors: Group 1 (Ahmed & Begüm)

library(tidyverse)
library(readr)

# 1. Smart Import ---------------------------------------------------------
# Load raw data and fix "Not specified" immediately
raw_data <- read_csv(
  "data/CampusFile_WM_2022.csv",
  na = c("", "NA", "Not specified"),
  show_col_types = FALSE
) 

# 2. Tidy, Transform & Classify -------------------------------------------
cleaned_data <- raw_data %>%
  # A. Select ONLY the columns for the V2 Outline
  select(
    rent_sqm,                 # Outcome
    energieeffizienzklasse,   # Interest Variable
    
    # IDs for Location & Classification
    kid2019,                  # District ID (needed for Fixed Effects)
    gid2019,                  # Municipality ID (needed to classify Urban/Rural)
    
    # Controls: Characteristics
    wohnflaeche,              # Living Space
    zimmeranzahl,             # Rooms
    baujahr,                  # Construction Year
    letzte_modernisierung,    # Modernization
    
    # Controls: Amenities
    balkon,                   # Balcony
    aufzug,                   # Elevator
    einbaukueche,             # Kitchen
    parkplatz,                # Parking
    keller,                   # Basement
    gaestewc,                 # Guest WC
    garten                    # Garden
  ) %>%
  
  # [cite_start]B. Create "Urban" vs "Rural" (The Math Trick) [cite: 31, 32]
  # Logic: If a District (kid) contains many Municipalities (gid), it is Rural.
  #        If a District contains only 1 Municipality, it is an Independent City.
  group_by(kid2019) %>%
  mutate(
    municipality_count = n_distinct(gid2019),
    region_type = if_else(municipality_count == 1, "urban", "rural")
  ) %>%
  ungroup() %>% # Always ungroup after calculations!
  
  # C. Create Log-Rent 
  mutate(
    log_rent_sqm = log(rent_sqm)
  ) %>%
  
  # D. Filter Invalid Data
  filter(
    rent_sqm > 0, 
    !is.na(energieeffizienzklasse)
  )

# 3. Save Processed Data --------------------------------------------------
saveRDS(cleaned_data, "data/cleaned_data.rds")

# 4. Verification ---------------------------------------------------------
print("Success! Data updated.")

# Check the new Urban/Rural split
print("Urban vs. Rural Count:")
table(cleaned_data$region_type)

# Check the columns to make sure amenities are there
glimpse(cleaned_data)