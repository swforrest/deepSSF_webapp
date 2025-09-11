# Benthic Cover & Fish Analysis Shiny Dashboard

This interactive R Shiny dashboard presents comprehensive analysis of coral reef ecosystems in the Solomon Islands, focusing on the relationship between coral cover and Topa fish abundance, and the impact of logging activities on reef health.

## Features

### 📊 **Interactive Dashboard Tabs**

1. **Overview** - Study summary, key statistics, and dataset overview
2. **Main Research Findings** - Core research plots with statistical models
3. **Spatial Analysis** - Geographic distribution of variables
4. **Interactive Maps** - Satellite-based interactive maps with three sub-tabs:
   - Coral Cover & Topa Distribution
   - Logging Impact Visualization  
   - Comprehensive Multi-variable Map
5. **Environmental Context** - Multi-panel analysis by environmental factors
6. **Data Explorer** - Interactive data table and custom plot generation

### 🗺️ **Interactive Satellite Maps**

- **High-resolution satellite imagery** from Esri WorldImagery
- **Multiple basemap options** (Satellite, Streets, Light)
- **Interactive markers** with detailed popup information
- **Toggleable layers** for different variables
- **Zoom and pan** functionality
- **Click markers** for site-specific data

### 📈 **Static Plots**

- Coral cover vs Topa abundance (GAM with negative binomial)
- Logging impact on coral cover (GAM smooth)
- Water quality analysis by logging status
- Coral vs soft coral trade-off relationships
- Spatial distribution patterns
- Environmental context analysis

## Quick Start

### Option 1: Use the Launcher (Recommended)
```r
# Run this command in R to automatically install packages and launch the app
source("launch_app.R")
```

### Option 2: Manual Launch
```r
# Install required packages first
install.packages(c("shiny", "shinydashboard", "leaflet", "DT", "plotly", 
                   "tidyverse", "ggplot2", "mgcv", "viridis", "patchwork", 
                   "scales", "htmlwidgets", "sf"))

# Launch the app
shiny::runApp("shiny_app.R")
```

## File Structure

```
├── shiny_app.R          # Main Shiny application
├── launch_app.R         # Automatic launcher script
├── summary_plots.R      # Plot generation and data processing
├── data/
│   ├── fish-coral-cover-sites.csv
│   ├── benthic_cover.csv
│   └── benthic_variables.csv
└── outputs/             # Generated static plots and maps
```

## Data Requirements

The app requires the following data files in the `data/` directory:
- `fish-coral-cover-sites.csv` - Main dataset with site information
- `benthic_cover.csv` - Benthic cover measurements  
- `benthic_variables.csv` - Environmental variables

## Key Variables

- **Coral Cover** - Proportion of hard coral coverage
- **Topa Abundance** - Count of Topa fish at each site
- **Logging Status** - Whether site is in logged or unlogged area
- **Water Clarity** - Secchi depth measurements
- **Flow Regime** - Strong or mild water flow classification
- **Distance to Logging** - Distance from site to nearest logging activity

## Technical Details

### Statistical Models
- **GAM (Generalized Additive Models)** for non-linear relationships
- **Negative binomial family** for count data (Topa abundance)
- **Linear models** for proportional data and comparisons

### Coordinate System
- Original data in UTM coordinates (Solomon Islands)
- Converted to WGS84 lat/long for web mapping
- Automatic projection detection and conversion

### Interactive Features
- **Leaflet maps** with multiple basemap providers
- **DT tables** with filtering and sorting
- **Plotly integration** for enhanced interactivity
- **Responsive design** with shinydashboard

## Usage Tips

1. **Navigation**: Use the sidebar menu to navigate between different analysis sections
2. **Interactive Maps**: 
   - Click markers for detailed site information
   - Use layer controls to toggle different variables
   - Switch between satellite, street, and light basemaps
3. **Data Explorer**: 
   - Filter data using the search boxes above each column
   - Create custom scatter plots and histograms
   - Sort by clicking column headers
4. **Plots**: All static plots are high-resolution and publication-ready

## Research Applications

This dashboard supports research into:
- **Marine conservation** and reef health assessment
- **Environmental impact** of terrestrial activities on marine ecosystems
- **Spatial ecology** and biogeographic patterns
- **Coral-fish relationships** and ecosystem dynamics
- **Water quality** and environmental monitoring

## Technical Support

If you encounter issues:
1. Ensure all required packages are installed
2. Check that data files are in the correct `data/` directory
3. Verify R version compatibility (recommended: R >= 4.0)
4. For coordinate conversion issues, check the projection settings in `summary_plots.R`

## Citation

When using this dashboard, please cite the underlying research and acknowledge the data sources from the Solomon Islands reef surveys.

---

**Created with R Shiny** | **Interactive Maps powered by Leaflet** | **Statistical Analysis with mgcv**
