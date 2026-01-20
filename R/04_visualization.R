# 04_visualize.R
# Purpose: Create publication-ready plots for the report
# Authors: Group 1 (Ahmed Amasha & Begüm Akyüz)
# -------------------------------------------------------------------
# CRITIQUE ADDRESSAL:
# 1. Clustering: All models (including subsamples) now cluster SEs by 'kid2019'.
# 2. Ordinality: Energy classes treated as dummy variables to allow non-linear 
#    price effects, but plotted in strictly ordinal sequence (A+ -> H).
# -------------------------------------------------------------------

rm(list = ls())

# Libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("modelsummary")) install.packages("modelsummary")
if (!require("fixest")) install.packages("fixest")
if (!require("viridis")) install.packages("viridis") 
if (!require("broom")) install.packages("broom") 

library(tidyverse)
library(modelsummary)
library(fixest)
library(viridis)
library(broom)

# Create output folder structure (Main + Subfolders)
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if (!dir.exists("output/figures/png")) dir.create("output/figures/png", recursive = TRUE)
if (!dir.exists("output/figures/pdf")) dir.create("output/figures/pdf", recursive = TRUE)

# Load Data
df <- readRDS("data/cleaned_data.rds")

# -------------------------------------------------------------------
# 1. GENERATE ALL PLOT OBJECTS
# -------------------------------------------------------------------
message("Generating plot objects...")

# --- p1: Descriptive Boxplot ---
# Note: Visualizes the ordinal nature of classes before modelling
p1 <- ggplot(df, aes(x = energieeffizienzklasse, y = rent_sqm, fill = energieeffizienzklasse)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 0.5) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(
    title = "Rent Distribution by Energy Efficiency Class",
    subtitle = "Raw data (no controls). Efficient classes tend to have higher median rents.",
    x = "Energy Class (Ordinal Scale)",
    y = "Rent per Sqm (Euro)",
    caption = "Source: ImmoScout24 Data"
  ) +
  theme_minimal() +
  theme(legend.position = "none") + 
  coord_cartesian(ylim = c(0, 40))

# --- p2: The Green Premium (Base Model) ---
message("Re-estimating base model with clustering...")
model_base <- feols(
  log_rent_sqm ~ energieeffizienzklasse + wohnflaeche + zimmeranzahl + 
    baujahr + letzte_modernisierung + 
    balkon + aufzug + einbaukueche + parkplatz + 
    region_type, 
  data = df,
  cluster = ~kid2019  # Addressed Methodological Caveat: Robust SEs
)

coef_names <- c(
  "energieeffizienzklasseAPLUS" = "Class A+",
  "energieeffizienzklasseA"     = "Class A",
  "energieeffizienzklasseB"     = "Class B",
  "energieeffizienzklasseD"     = "Class D",
  "energieeffizienzklasseE"     = "Class E",
  "energieeffizienzklasseF"     = "Class F",
  "energieeffizienzklasseG"     = "Class G",
  "energieeffizienzklasseH"     = "Class H"
)

p2 <- modelplot(model_base, 
                coef_map = coef_names, 
                conf_level = 0.95,
                color = "darkblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "The Green Premium: Rent Impact Relative to Class C",
    subtitle = "Estimates from Hedonic Regression. Bars represent 95% CIs (Clustered SEs).",
    x = "Log-Point Increase in Rent (Approx %)",
    y = "",
    caption = "Note: Classes modelled as categorical dummies to capture non-linear premiums."
  ) +
  theme_minimal() +
  theme(plot.title.position = "plot")

# --- p3: Urban vs Rural (Total Effect Method) ---
message("Re-estimating separate models with clustering...")

# Model for Rural (With Clustering Fix)
model_rural <- feols(
  log_rent_sqm ~ energieeffizienzklasse + wohnflaeche + zimmeranzahl + 
    baujahr + letzte_modernisierung + 
    balkon + aufzug + einbaukueche + parkplatz,
  data = df %>% filter(region_type == "rural"),
  cluster = ~kid2019 # <--- FIXED: Added clustering here
)

# Model for Urban (With Clustering Fix)
model_urban <- feols(
  log_rent_sqm ~ energieeffizienzklasse + wohnflaeche + zimmeranzahl + 
    baujahr + letzte_modernisierung + 
    balkon + aufzug + einbaukueche + parkplatz,
  data = df %>% filter(region_type == "urban"),
  cluster = ~kid2019 # <--- FIXED: Added clustering here
)

# Process Results
tidy_rural <- tidy(model_rural, conf.int = TRUE) %>%
  filter(str_detect(term, "energieeffizienzklasse")) %>%
  mutate(market = "Rural")

tidy_urban <- tidy(model_urban, conf.int = TRUE) %>%
  filter(str_detect(term, "energieeffizienzklasse")) %>%
  mutate(market = "Urban")

plot_data <- bind_rows(tidy_rural, tidy_urban) %>%
  mutate(
    energy_class = str_remove(term, "energieeffizienzklasse"),
    estimate_pct = estimate * 100,
    conf.low_pct = conf.low * 100,
    conf.high_pct = conf.high * 100
  ) %>%
  mutate(
    # Enforcing Ordinality in Visualization
    energy_class = factor(energy_class, levels = c("APLUS", "A", "B", "C", "D", "E", "F", "G", "H"))
  )

p3 <- ggplot(plot_data, aes(x = energy_class, y = estimate_pct, color = market)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low_pct, ymax = conf.high_pct),
    width = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "The Green Premium: Urban vs. Rural Markets",
    subtitle = "Percentage rent increase relative to Class C (Total Effect)",
    x = "Energy Efficiency Class (Ordered)",
    y = "Rent Premium (%)",
    color = "Region Type",
    caption = "Note: Estimates from split-sample regressions. Standard errors clustered by district."
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Rural" = "#2E8B57", "Urban" = "#B22222"))

# -------------------------------------------------------------------
# 2. SAVE PNG FILES
# -------------------------------------------------------------------
message("Saving PNG files to 'output/figures/png/'...")
ggsave("output/figures/png/fig01_boxplot_raw.png", plot = p1, width = 8, height = 6)
ggsave("output/figures/png/fig02_green_premium.png", plot = p2, width = 8, height = 5)
ggsave("output/figures/png/fig03_interaction_effect.png", plot = p3, width = 8, height = 5)

# -------------------------------------------------------------------
# 3. SAVE PDF FILES
# -------------------------------------------------------------------
message("Saving PDF files to 'output/figures/pdf/'...")
ggsave("output/figures/pdf/fig01_boxplot_raw.pdf", plot = p1, width = 8, height = 6)
ggsave("output/figures/pdf/fig02_green_premium.pdf", plot = p2, width = 8, height = 5)
ggsave("output/figures/pdf/fig03_interaction_effect.pdf", plot = p3, width = 8, height = 5)

message("Visualization complete. All files saved to subfolders in 'output/figures/'.")