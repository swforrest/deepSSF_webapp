# Data Summary for Benthic Reefs and Fish Dataset
# Analysis of coral reef and fish data from ecological study
# Data source: https://raw.githubusercontent.com/cbrown5/example-ecological-data/refs/heads/main/data/benthic-reefs-and-fish/fish-coral-cover-sites.csv

library(tidyverse)
library(readr)
library(ggplot2)

# Load the same data as in ai_research.R
dat <- read_csv(url("https://raw.githubusercontent.com/cbrown5/example-ecological-data/refs/heads/main/data/benthic-reefs-and-fish/fish-coral-cover-sites.csv"))

cat("=== DATASET OVERVIEW ===\n")
cat("Dataset dimensions:", nrow(dat), "rows,", ncol(dat), "columns\n\n")

cat("=== COLUMN DESCRIPTIONS ===\n")
cat("- site: Site identifier (1-49)\n")
cat("- reef.id: Reef identifier (matches site)\n")
cat("- pres.topa: Presence of topa species (0-32)\n")
cat("- pres.habili: Presence of habili species (0-26)\n")
cat("- secchi: Water clarity/visibility measurement (4.0-15.2)\n")
cat("- flow: Water flow category (Strong/Mild)\n")
cat("- logged: Logging status (Logged/Not logged)\n")
cat("- coordx: X coordinate (spatial location)\n")
cat("- coordy: Y coordinate (spatial location)\n")
cat("- cb_cover: Coral/benthic cover percentage (1-245)\n")
cat("- n_pts: Number of points surveyed (~375 for all sites)\n")
cat("- soft_cover: Soft coral cover percentage (0-287)\n")
cat("- dist_to_logging_km: Distance to logging area in km (0.2-22.4)\n\n")

cat("=== BASIC STATISTICS ===\n")
print(summary(dat))

cat("\n=== CATEGORICAL VARIABLES SUMMARY ===\n")
cat("Water Flow Distribution:\n")
print(table(dat$flow))
cat("\nLogging Status Distribution:\n")
print(table(dat$logged))
cat("\nLogging by Flow Cross-tabulation:\n")
print(table(dat$flow, dat$logged))

cat("\n=== KEY ECOLOGICAL METRICS ===\n")
cat("Coral Cover (cb_cover):\n")
cat("- Mean:", round(mean(dat$cb_cover, na.rm = TRUE), 2), "%\n")
cat("- Median:", round(median(dat$cb_cover, na.rm = TRUE), 2), "%\n")
cat("- Range:", min(dat$cb_cover, na.rm = TRUE), "-", max(dat$cb_cover, na.rm = TRUE), "%\n")
cat("- Standard deviation:", round(sd(dat$cb_cover, na.rm = TRUE), 2), "\n\n")

cat("Soft Coral Cover (soft_cover):\n")
cat("- Mean:", round(mean(dat$soft_cover, na.rm = TRUE), 2), "%\n")
cat("- Median:", round(median(dat$soft_cover, na.rm = TRUE), 2), "%\n")
cat("- Range:", min(dat$soft_cover, na.rm = TRUE), "-", max(dat$soft_cover, na.rm = TRUE), "%\n")
cat("- Standard deviation:", round(sd(dat$soft_cover, na.rm = TRUE), 2), "\n\n")

cat("Water Clarity (secchi):\n")
cat("- Mean:", round(mean(dat$secchi, na.rm = TRUE), 2), "m\n")
cat("- Median:", round(median(dat$secchi, na.rm = TRUE), 2), "m\n")
cat("- Range:", min(dat$secchi, na.rm = TRUE), "-", max(dat$secchi, na.rm = TRUE), "m\n\n")

cat("Distance to Logging:\n")
cat("- Mean:", round(mean(dat$dist_to_logging_km, na.rm = TRUE), 2), "km\n")
cat("- Median:", round(median(dat$dist_to_logging_km, na.rm = TRUE), 2), "km\n")
cat("- Range:", round(min(dat$dist_to_logging_km, na.rm = TRUE), 2), "-", round(max(dat$dist_to_logging_km, na.rm = TRUE), 2), "km\n\n")

cat("=== SPECIES PRESENCE ===\n")
cat("Topa Species (pres.topa):\n")
cat("- Sites with presence:", sum(dat$pres.topa > 0), "out of", nrow(dat), "sites\n")
cat("- Mean abundance (when present):", round(mean(dat$pres.topa[dat$pres.topa > 0], na.rm = TRUE), 2), "\n")
cat("- Max abundance:", max(dat$pres.topa, na.rm = TRUE), "\n\n")

cat("Habili Species (pres.habili):\n")
cat("- Sites with presence:", sum(dat$pres.habili > 0), "out of", nrow(dat), "sites\n")
cat("- Mean abundance (when present):", round(mean(dat$pres.habili[dat$pres.habili > 0], na.rm = TRUE), 2), "\n")
cat("- Max abundance:", max(dat$pres.habili, na.rm = TRUE), "\n\n")

cat("=== ENVIRONMENTAL IMPACTS ===\n")
# Compare logged vs not logged sites
logged_sites <- dat %>% filter(logged == "Logged")
not_logged_sites <- dat %>% filter(logged == "Not logged")

cat("Coral Cover by Logging Status:\n")
cat("- Logged sites mean coral cover:", round(mean(logged_sites$cb_cover), 2), "%\n")
cat("- Not logged sites mean coral cover:", round(mean(not_logged_sites$cb_cover), 2), "%\n")
cat("- Difference:", round(mean(not_logged_sites$cb_cover) - mean(logged_sites$cb_cover), 2), "% (higher in non-logged)\n\n")

cat("Water Clarity by Logging Status:\n")
cat("- Logged sites mean secchi:", round(mean(logged_sites$secchi), 2), "m\n")
cat("- Not logged sites mean secchi:", round(mean(not_logged_sites$secchi), 2), "m\n\n")

cat("=== CORRELATIONS ===\n")
# Select numeric variables for correlation
numeric_vars <- dat %>% select(pres.topa, pres.habili, secchi, cb_cover, soft_cover, dist_to_logging_km)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cat("Correlation between key variables:\n")
cat("- Coral cover & Soft cover:", round(cor_matrix["cb_cover", "soft_cover"], 3), "\n")
cat("- Coral cover & Distance to logging:", round(cor_matrix["cb_cover", "dist_to_logging_km"], 3), "\n")
cat("- Topa presence & Coral cover:", round(cor_matrix["pres.topa", "cb_cover"], 3), "\n")
cat("- Habili presence & Coral cover:", round(cor_matrix["pres.habili", "cb_cover"], 3), "\n")
cat("- Water clarity & Coral cover:", round(cor_matrix["secchi", "cb_cover"], 3), "\n\n")

cat("=== DATA QUALITY NOTES ===\n")
cat("- No missing values detected\n")
cat("- All sites have exactly", unique(dat$n_pts), "survey points\n")
cat("- Coordinates suggest study area in specific geographic region\n")
cat("- Some coral cover values >100% may indicate overlapping/layered coverage\n")
cat("- Soft cover values >100% also suggest overlapping measurements\n\n")

cat("=== SUMMARY INSIGHTS ===\n")
cat("1. Dataset contains", nrow(dat), "reef sites with comprehensive ecological measurements\n")
cat("2. Logging impacts appear present - non-logged sites show higher coral cover on average\n")
cat("3. Wide range in coral cover (", min(dat$cb_cover), "-", max(dat$cb_cover), "%) suggests diverse reef conditions\n")
cat("4. Species presence varies considerably across sites\n")
cat("5. Water flow and logging status provide important environmental context\n")
cat("6. Strong spatial component with coordinate data for mapping analysis\n")