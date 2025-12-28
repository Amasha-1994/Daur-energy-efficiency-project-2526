# 02_descriptive.R


write.csv(readRDS("data/cleaned_data.rds"), "data/cleaned_data.csv", row.names = FALSE)

rm(list = ls())

library(dplyr)
library(readr)

# Create output folder if it does not exist
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

# Load cleaned data
df <- readRDS("data/cleaned_data.rds")

# --- SAFETY CHECKS ---
stopifnot(
  "region_type" %in% names(df),
  "rent_sqm" %in% names(df),
  "wohnflaeche" %in% names(df)
)

# Assign energieeffizienzklasse to obj
energy_col <- "energieeffizienzklasse"   

# Create energy-efficient indicator
df <- df %>%
  mutate(
    energy_class_std = toupper(trimws(as.character(.data[[energy_col]]))),
    energy_efficient = if_else(
      energy_class_std %in% c("A+", "A", "B"),
      1L,
      0L
    )
  )

# Urban vs Rural summary table
summary_urban_rural <- df %>%
  group_by(region_type) %>%
  summarise(
    n_obs = n(),
    avg_rent_sqm = mean(rent_sqm, na.rm = TRUE),
    avg_wohnflaeche = mean(wohnflaeche, na.rm = TRUE),
    share_energy_efficient = mean(energy_efficient, na.rm = TRUE),
    .groups = "drop"
  )

# Save outputs
write_csv(summary_urban_rural, "output/summary_urban_rural.csv")
saveRDS(summary_urban_rural, "output/summary_urban_rural.rds")

# Print to console
print(summary_urban_rural)
