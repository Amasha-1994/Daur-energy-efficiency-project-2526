# -------------------------------------------------------------------
# 04_visualization.R
# Purpose: Visualize energy efficiency effects by urban vs rural areas
# -------------------------------------------------------------------

rm(list = ls())

library(fixest)
library(tidyverse)
library(broom)

df <- readRDS("data/cleaned_data.rds")

model_interaction <- feols(
  log_rent_sqm ~ energieeffizienzklasse * region_type + 
    wohnflaeche + zimmeranzahl + baujahr + letzte_modernisierung + 
    balkon + aufzug + einbaukueche + parkplatz,
  data = df,
  cluster = ~kid2019
)

coef_df <- tidy(model_interaction, conf.int = TRUE) %>%
  filter(str_detect(term, "energieeffizienzklasse")) %>%
  mutate(
    market = if_else(str_detect(term, "region_typeurban"),
                     "Urban", "Rural"),
    energy_class = term %>%
      str_remove("energieeffizienzklasse") %>%
      str_remove(":region_typeurban")
  )

coef_df <- coef_df %>%
  mutate(
    estimate_pct = estimate * 100,
    conf.low_pct = conf.low * 100,
    conf.high_pct = conf.high * 100
  )

coef_df <- coef_df %>%
  mutate(
    energy_class = factor(
      energy_class,
      levels = c("APLUS", "A", "B", "C", "D", "E", "F", "G", "H")
    )
  )

ggplot(coef_df, aes(x = energy_class, y = estimate_pct, color = market)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(
    aes(ymin = conf.low_pct, ymax = conf.high_pct),
    width = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Energy Efficiency Class (Reference: C)",
    y = "Difference in Rent per m² (%)",
    color = "Market Type",
    title = "Energy Efficiency and Rental Prices",
    subtitle = "Urban vs Rural Rental Markets"
  ) +
  theme_minimal()

ggsave(
  filename = "output/energy_efficiency_urban_rural.png",
  width = 8,
  height = 5
)




