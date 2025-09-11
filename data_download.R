library(readr)

# URLs for datasets
benthic_cover_url <- "https://raw.githubusercontent.com/cbrown5/example-ecological-data/refs/heads/main/data/benthic-reefs-and-fish/benthic_cover.csv"
benthic_variables_url <- "https://raw.githubusercontent.com/cbrown5/example-ecological-data/refs/heads/main/data/benthic-reefs-and-fish/benthic_variables.csv"
fish_coral_cover_sites_url <- "https://raw.githubusercontent.com/cbrown5/example-ecological-data/refs/heads/main/data/benthic-reefs-and-fish/fish-coral-cover-sites.csv"

# Local file paths
benthic_cover_path <- "data/benthic_cover.csv"
benthic_variables_path <- "data/benthic_variables.csv"
fish_coral_cover_sites_path <- "data/fish-coral-cover-sites.csv"

# Download and save datasets
benthic_cover <- read_csv(benthic_cover_url)
write_csv(benthic_cover, benthic_cover_path)

benthic_variables <- read_csv(benthic_variables_url)
write_csv(benthic_variables, benthic_variables_path)

fish_coral_cover_sites <- read_csv(fish_coral_cover_sites_url)
write_csv(fish_coral_cover_sites, fish_coral_cover_sites_path)