# Simple Shiny App Test Script
# Assumes packages are already installed

# Set working directory to the app directory
setwd("/Users/scottforrest/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/PhD - Scott Forrest/GIT/deepSSF_webapp")

# Load required libraries (if available)
library(shiny)
cat("Running Shiny app...\n")

# Run the app
shiny::runApp("shiny_app.R", host = "127.0.0.1", port = 3838, launch.browser = FALSE)
