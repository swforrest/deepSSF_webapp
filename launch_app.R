# Shiny App Launcher for Benthic Cover and Fish Analysis
# This script installs required packages and launches the Shiny app

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Function to install and load required packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Required packages for the Shiny app
required_packages <- c(
  "shiny",
  "shinydashboard", 
  "leaflet",
  "DT",
  "plotly",
  "tidyverse",
  "readr",
  "ggplot2",
  "mgcv",
  "viridis",
  "RColorBrewer",
  "patchwork",
  "scales",
  "htmlwidgets",
  "sf"
)

cat("Checking and installing required packages...\n")
install_and_load(required_packages)

cat("All packages installed successfully!\n")
cat("Launching Shiny app...\n\n")

# Launch the Shiny app
shiny::runApp("shiny_app.R", launch.browser = TRUE)
