# Complete Setup and Launch Script for Benthic Cover & Fish Analysis Shiny App
# This script provides multiple options for running the Shiny dashboard

cat("=== Benthic Cover & Fish Analysis Shiny Dashboard ===\n\n")

# Check if required data files exist
data_files <- c("data/fish-coral-cover-sites.csv", 
                "data/benthic_cover.csv", 
                "data/benthic_variables.csv")

missing_files <- data_files[!file.exists(data_files)]
if (length(missing_files) > 0) {
  cat("WARNING: Missing data files:\n")
  for (file in missing_files) {
    cat("  -", file, "\n")
  }
  cat("\nPlease ensure all data files are in the correct location.\n\n")
}

# Function to check package availability
check_packages <- function(packages) {
  available <- sapply(packages, function(pkg) {
    suppressMessages(suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE)))
  })
  return(available)
}

# Check which version of the app to recommend
required_full <- c("shiny", "shinydashboard", "leaflet", "DT", "plotly", 
                   "tidyverse", "ggplot2", "mgcv", "viridis", "patchwork")
required_simple <- c("shiny", "ggplot2", "leaflet")

full_available <- all(check_packages(required_full))
simple_available <- all(check_packages(required_simple))

cat("Package availability check:\n")
cat("Full app (all features):", ifelse(full_available, "✓ Available", "✗ Missing packages"), "\n")
cat("Simple app (basic features):", ifelse(simple_available, "✓ Available", "✗ Missing packages"), "\n\n")

# Provide recommendations
if (full_available) {
  cat("RECOMMENDED: Use the full-featured app\n")
  cat("Run: shiny::runApp('shiny_app.R')\n\n")
  
  cat("Or use the automatic installer:\n")
  cat("Run: source('launch_app.R')\n\n")
  
} else if (simple_available) {
  cat("RECOMMENDED: Use the simplified app\n")
  cat("Run: shiny::runApp('simple_app.R')\n\n")
  
} else {
  cat("INSTALL PACKAGES FIRST:\n")
  cat("Run: source('launch_app.R')\n")
  cat("This will automatically install required packages.\n\n")
}

# Display available app versions
cat("=== AVAILABLE APP VERSIONS ===\n\n")

cat("1. FULL APP (shiny_app.R)\n")
cat("   Features:\n")
cat("   - Professional dashboard layout\n")
cat("   - Multiple interactive tabs\n")
cat("   - Satellite imagery maps\n")
cat("   - Advanced data tables\n")
cat("   - Publication-ready plots\n")
cat("   Requirements: All packages listed above\n\n")

cat("2. SIMPLE APP (simple_app.R)\n")
cat("   Features:\n")
cat("   - Basic tabbed interface\n")
cat("   - Essential plots and maps\n")
cat("   - Data exploration\n")
cat("   - Lightweight and fast\n")
cat("   Requirements: shiny, ggplot2, leaflet\n\n")

cat("3. INSTALLER (launch_app.R)\n")
cat("   - Automatically installs all required packages\n")
cat("   - Sets up CRAN mirror\n")
cat("   - Launches the full app\n\n")

# Instructions
cat("=== QUICK START INSTRUCTIONS ===\n\n")

cat("Option 1 - Full App (if packages are installed):\n")
cat("  library(shiny)\n")
cat("  runApp('shiny_app.R')\n\n")

cat("Option 2 - Simple App (minimal requirements):\n")
cat("  library(shiny)\n")
cat("  runApp('simple_app.R')\n\n")

cat("Option 3 - Auto-install and launch:\n")
cat("  source('launch_app.R')\n\n")

cat("Option 4 - From terminal/command line:\n")
cat("  Rscript -e \"shiny::runApp('simple_app.R', host='127.0.0.1', port=3838)\"\n\n")

cat("=== ACCESS INFORMATION ===\n\n")
cat("Once running, access the app at:\n")
cat("  Local: http://127.0.0.1:3838\n")
cat("  Local: http://localhost:3838\n\n")

cat("=== DASHBOARD FEATURES ===\n\n")
cat("Tabs available in the full app:\n")
cat("  📊 Overview - Study summary and statistics\n")
cat("  🔬 Main Research Findings - Core analysis plots\n")
cat("  🗺️  Spatial Analysis - Geographic patterns\n")
cat("  🛰️  Interactive Maps - Satellite imagery with data overlay\n")
cat("     - Coral Cover & Topa Distribution\n")
cat("     - Logging Impact Visualization\n")
cat("     - Comprehensive Multi-variable Map\n")
cat("  🌊 Environmental Context - Multi-factor analysis\n")
cat("  📋 Data Explorer - Interactive data tables and custom plots\n\n")

cat("=== TROUBLESHOOTING ===\n\n")
cat("If you encounter issues:\n")
cat("1. Check that all data files are in the 'data/' directory\n")
cat("2. Try the simple app first: runApp('simple_app.R')\n")
cat("3. Install packages manually if auto-install fails\n")
cat("4. Check R version (recommended: R >= 4.0)\n")
cat("5. Restart R session and try again\n\n")

cat("For more information, see SHINY_README.md\n\n")

cat("=== READY TO LAUNCH ===\n")
cat("Choose your preferred option above and start exploring the data!\n")
