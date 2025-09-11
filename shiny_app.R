# R Shiny App for Benthic Cover and Fish Analysis
# Interactive presentation of all plots from summary_plots.R
# Features interactive satellite maps and static plot displays

library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(readr)
library(ggplot2)
library(mgcv)
library(viridis)
library(RColorBrewer)
library(patchwork)
library(scales)
library(htmlwidgets)
library(sf)

# Source the summary plots script to load data and create plots
source("summary_plots.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Benthic Cover & Fish Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Main Research Findings", tabName = "main_research", icon = icon("microscope")),
      menuItem("Spatial Analysis", tabName = "spatial", icon = icon("map")),
      menuItem("Interactive Maps", tabName = "maps", icon = icon("satellite")),
      menuSubItem("Coral Cover & Topa", tabName = "map_coral_topa"),
      menuSubItem("Logging Impact", tabName = "map_logging"),
      menuSubItem("Comprehensive Map", tabName = "map_comprehensive"),
      menuItem("Environmental Context", tabName = "environment", icon = icon("water")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .plot-container {
          background-color: white;
          padding: 15px;
          margin: 10px;
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .info-box {
          background-color: #3c8dbc;
          color: white;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Study Overview", status = "primary", solidHeader = TRUE, width = 12,
            h3("Benthic Cover and Fish Analysis: Solomon Islands"),
            p("This dashboard presents comprehensive analysis of coral reef ecosystems, focusing on:"),
            tags$ul(
              tags$li("Relationship between coral cover and Topa fish abundance"),
              tags$li("Impact of logging activities on reef health"),
              tags$li("Spatial patterns in environmental variables"),
              tags$li("Water quality and flow regime effects")
            ),
            br(),
            h4("Key Research Questions:"),
            tags$ol(
              tags$li("How does coral cover relate to Topa abundance?"),
              tags$li("What is the impact of logging on coral reef health?"),
              tags$li("How do environmental factors vary spatially across the study area?")
            )
          )
        ),
        
        fluidRow(
          valueBox(
            value = nrow(fish_data),
            subtitle = "Study Sites",
            icon = icon("map-marker"),
            color = "blue"
          ),
          valueBox(
            value = paste0(round(mean(fish_data$coral_cover_prop) * 100, 1), "%"),
            subtitle = "Mean Coral Cover",
            icon = icon("coral"),
            color = "green"
          ),
          valueBox(
            value = round(mean(fish_data$pres.topa), 1),
            subtitle = "Mean Topa Abundance",
            icon = icon("fish"),
            color = "yellow"
          )
        ),
        
        fluidRow(
          box(
            title = "Dataset Summary", status = "info", solidHeader = TRUE, width = 6,
            verbatimTextOutput("data_summary")
          ),
          box(
            title = "Key Correlations", status = "info", solidHeader = TRUE, width = 6,
            verbatimTextOutput("correlations")
          )
        )
      ),
      
      # Main Research Findings Tab
      tabItem(tabName = "main_research",
        fluidRow(
          box(
            title = "Coral Cover vs Topa Abundance", status = "primary", solidHeader = TRUE, width = 12,
            div(class = "plot-container",
                plotOutput("plot_coral_topa", height = "500px")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Logging Impact on Coral Cover", status = "primary", solidHeader = TRUE, width = 6,
            div(class = "plot-container",
                plotOutput("plot_logging_impact", height = "400px")
            )
          ),
          box(
            title = "Water Quality by Logging Status", status = "primary", solidHeader = TRUE, width = 6,
            div(class = "plot-container",
                plotOutput("plot_water_quality", height = "400px")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Coral vs Soft Coral Trade-off", status = "primary", solidHeader = TRUE, width = 12,
            div(class = "plot-container",
                plotOutput("plot_coral_tradeoff", height = "500px")
            )
          )
        )
      ),
      
      # Spatial Analysis Tab
      tabItem(tabName = "spatial",
        fluidRow(
          box(
            title = "Spatial Distribution: Coral Cover & Topa", status = "primary", solidHeader = TRUE, width = 6,
            div(class = "plot-container",
                plotOutput("plot_spatial_coral", height = "400px")
            )
          ),
          box(
            title = "Spatial Distribution: Logging Impact", status = "primary", solidHeader = TRUE, width = 6,
            div(class = "plot-container",
                plotOutput("plot_spatial_logging", height = "400px")
            )
          )
        )
      ),
      
      # Interactive Maps Tabs
      tabItem(tabName = "map_coral_topa",
        fluidRow(
          box(
            title = "Interactive Map: Coral Cover & Topa Abundance", 
            status = "primary", solidHeader = TRUE, width = 12, height = "600px",
            p("This satellite map shows the spatial distribution of coral cover (color) and Topa abundance (marker size). 
              Click on markers for detailed site information."),
            leafletOutput("coral_topa_map", height = "500px")
          )
        )
      ),
      
      tabItem(tabName = "map_logging",
        fluidRow(
          box(
            title = "Interactive Map: Logging Impact", 
            status = "primary", solidHeader = TRUE, width = 12, height = "600px",
            p("This satellite map visualizes logging status and distance to logging activities. 
              Marker size indicates distance to logging sites."),
            leafletOutput("logging_map", height = "500px")
          )
        )
      ),
      
      tabItem(tabName = "map_comprehensive",
        fluidRow(
          box(
            title = "Comprehensive Interactive Map", 
            status = "primary", solidHeader = TRUE, width = 12, height = "600px",
            p("This comprehensive map combines multiple variables with toggleable layers. 
              Switch between different basemaps and overlay combinations."),
            leafletOutput("comprehensive_map", height = "500px")
          )
        )
      ),
      
      # Environmental Context Tab
      tabItem(tabName = "environment",
        fluidRow(
          box(
            title = "Environmental Context Analysis", status = "primary", solidHeader = TRUE, width = 12,
            p("Multi-panel analysis showing relationships between Topa abundance, water clarity, 
              and environmental factors (flow regime and logging status)."),
            div(class = "plot-container",
                plotOutput("plot_environment", height = "600px")
            )
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Data Explorer", status = "primary", solidHeader = TRUE, width = 12,
            p("Explore the complete dataset with interactive filtering and sorting capabilities."),
            DT::dataTableOutput("data_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Variable Distributions", status = "info", solidHeader = TRUE, width = 6,
            selectInput("hist_variable", "Select Variable:",
                       choices = c("Coral Cover" = "coral_cover_prop",
                                 "Topa Abundance" = "pres.topa",
                                 "Water Clarity" = "secchi",
                                 "Distance to Logging" = "dist_to_logging_km")),
            plotOutput("histogram_plot", height = "300px")
          ),
          box(
            title = "Scatter Plot Explorer", status = "info", solidHeader = TRUE, width = 6,
            selectInput("scatter_x", "X Variable:",
                       choices = c("Coral Cover" = "coral_cover_prop",
                                 "Topa Abundance" = "pres.topa",
                                 "Water Clarity" = "secchi",
                                 "Distance to Logging" = "dist_to_logging_km")),
            selectInput("scatter_y", "Y Variable:",
                       choices = c("Topa Abundance" = "pres.topa",
                                 "Coral Cover" = "coral_cover_prop",
                                 "Water Clarity" = "secchi",
                                 "Distance to Logging" = "dist_to_logging_km"),
                       selected = "pres.topa"),
            plotOutput("scatter_plot", height = "300px")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Overview outputs
  output$data_summary <- renderText({
    paste0(
      "Number of sites: ", nrow(fish_data), "\n",
      "Logged sites: ", sum(fish_data$logged == "Logged"), "\n",
      "Not logged sites: ", sum(fish_data$logged == "Not logged"), "\n",
      "Strong flow sites: ", sum(fish_data$flow == "Strong"), "\n",
      "Mild flow sites: ", sum(fish_data$flow == "Mild"), "\n",
      "Mean coral cover: ", round(mean(fish_data$coral_cover_prop) * 100, 1), "%\n",
      "Mean Topa abundance: ", round(mean(fish_data$pres.topa), 1), "\n",
      "Mean water clarity: ", round(mean(fish_data$secchi), 1), "m"
    )
  })
  
  output$correlations <- renderText({
    cor_coral_topa <- cor(fish_data$coral_cover_prop, fish_data$pres.topa, use = "complete.obs")
    cor_dist_coral <- cor(fish_data$dist_to_logging_km, fish_data$coral_cover_prop, use = "complete.obs")
    cor_secchi_topa <- cor(fish_data$secchi, fish_data$pres.topa, use = "complete.obs")
    
    paste0(
      "Coral cover - Topa abundance: ", round(cor_coral_topa, 3), "\n",
      "Distance to logging - Coral cover: ", round(cor_dist_coral, 3), "\n",
      "Water clarity - Topa abundance: ", round(cor_secchi_topa, 3), "\n\n",
      "Logging impact on coral cover:\n",
      "Not logged: ", round(mean(fish_data$coral_cover_prop[fish_data$logged == "Not logged"]) * 100, 1), "%\n",
      "Logged: ", round(mean(fish_data$coral_cover_prop[fish_data$logged == "Logged"]) * 100, 1), "%"
    )
  })
  
  # Main research plots
  output$plot_coral_topa <- renderPlot({ p1 })
  output$plot_logging_impact <- renderPlot({ p2 })
  output$plot_water_quality <- renderPlot({ p3 })
  output$plot_coral_tradeoff <- renderPlot({ p4 })
  
  # Spatial plots
  output$plot_spatial_coral <- renderPlot({ p5a })
  output$plot_spatial_logging <- renderPlot({ p5b })
  
  # Environmental context
  output$plot_environment <- renderPlot({ p6 })
  
  # Interactive maps
  output$coral_topa_map <- renderLeaflet({
    coral_topa_map
  })
  
  output$logging_map <- renderLeaflet({
    logging_impact_map
  })
  
  output$comprehensive_map <- renderLeaflet({
    comprehensive_map
  })
  
  # Data explorer
  output$data_table <- DT::renderDataTable({
    display_data <- fish_data %>%
      select(site, logged, flow, coral_cover_prop, pres.topa, secchi, 
             dist_to_logging_km, latitude, longitude) %>%
      mutate(
        coral_cover_prop = round(coral_cover_prop * 100, 1),
        secchi = round(secchi, 1),
        dist_to_logging_km = round(dist_to_logging_km, 2),
        latitude = round(latitude, 4),
        longitude = round(longitude, 4)
      ) %>%
      rename(
        Site = site,
        `Logging Status` = logged,
        `Flow Regime` = flow,
        `Coral Cover (%)` = coral_cover_prop,
        `Topa Abundance` = pres.topa,
        `Water Clarity (m)` = secchi,
        `Distance to Logging (km)` = dist_to_logging_km,
        Latitude = latitude,
        Longitude = longitude
      )
    
    DT::datatable(display_data, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  filter = 'top',
                  rownames = FALSE)
  })
  
  # Interactive plots for data exploration
  output$histogram_plot <- renderPlot({
    var_name <- input$hist_variable
    var_label <- names(which(c("coral_cover_prop" = "Coral Cover",
                              "pres.topa" = "Topa Abundance", 
                              "secchi" = "Water Clarity",
                              "dist_to_logging_km" = "Distance to Logging") == var_name))
    
    ggplot(fish_data, aes_string(x = var_name)) +
      geom_histogram(bins = 15, fill = "#2E86AB", alpha = 0.7, color = "white") +
      labs(title = paste("Distribution of", var_label),
           x = var_label,
           y = "Count") +
      theme_research
  })
  
  output$scatter_plot <- renderPlot({
    x_var <- input$scatter_x
    y_var <- input$scatter_y
    
    x_label <- names(which(c("coral_cover_prop" = "Coral Cover",
                            "pres.topa" = "Topa Abundance", 
                            "secchi" = "Water Clarity",
                            "dist_to_logging_km" = "Distance to Logging") == x_var))
    y_label <- names(which(c("coral_cover_prop" = "Coral Cover",
                            "pres.topa" = "Topa Abundance", 
                            "secchi" = "Water Clarity", 
                            "dist_to_logging_km" = "Distance to Logging") == y_var))
    
    ggplot(fish_data, aes_string(x = x_var, y = y_var)) +
      geom_point(aes(color = logged), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      scale_color_manual(values = colors_logging, name = "Logging Status") +
      labs(title = paste(y_label, "vs", x_label),
           x = x_label,
           y = y_label) +
      theme_research
  })
}

# Run the application
shinyApp(ui = ui, server = server)
