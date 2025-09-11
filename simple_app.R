# Simplified R Shiny App for Benthic Cover and Fish Analysis
# Using only base shiny package for maximum compatibility

library(shiny)
library(ggplot2)
library(leaflet)

# Try to load additional packages, but continue if they're not available
tryCatch({
  library(DT)
  dt_available <- TRUE
}, error = function(e) {
  dt_available <- FALSE
  cat("DT package not available, using basic table display\n")
})

tryCatch({
  library(tidyverse)
  library(readr)
  library(mgcv)
  library(viridis)
  library(scales)
  packages_available <- TRUE
}, error = function(e) {
  packages_available <- FALSE
  cat("Some packages not available, using simplified functionality\n")
})

# Source the summary plots if possible
if (file.exists("summary_plots.R")) {
  tryCatch({
    source("summary_plots.R")
    plots_loaded <- TRUE
    cat("Plots loaded successfully\n")
  }, error = function(e) {
    plots_loaded <- FALSE
    cat("Could not load summary_plots.R:", e$message, "\n")
  })
} else {
  plots_loaded <- FALSE
  cat("summary_plots.R not found\n")
}

# Load data directly if plots weren't loaded
if (!plots_loaded) {
  cat("Loading data directly...\n")
  if (file.exists("data/fish-coral-cover-sites.csv")) {
    fish_data <- read.csv("data/fish-coral-cover-sites.csv")
    fish_data$coral_cover_prop <- fish_data$cb_cover / fish_data$n_pts
    fish_data$soft_cover_prop <- fish_data$soft_cover / fish_data$n_pts
    fish_data$logged <- factor(fish_data$logged, levels = c("Not logged", "Logged"))
    fish_data$flow <- factor(fish_data$flow)
    
    # Estimate coordinates for mapping (rough approximation)
    x_norm <- (fish_data$coordx - min(fish_data$coordx)) / (max(fish_data$coordx) - min(fish_data$coordx))
    y_norm <- (fish_data$coordy - min(fish_data$coordy)) / (max(fish_data$coordy) - min(fish_data$coordy))
    
    base_lon <- 159.5
    base_lat <- -9.0
    lon_span <- 1.5
    lat_span <- 1.0
    
    fish_data$longitude <- base_lon + (x_norm * lon_span)
    fish_data$latitude <- base_lat + (y_norm * lat_span)
    
    data_loaded <- TRUE
    cat("Data loaded successfully\n")
  } else {
    data_loaded <- FALSE
    cat("Data file not found\n")
  }
}

# Define UI
ui <- navbarPage(
  title = "Benthic Cover & Fish Analysis",
  
  # Overview Tab
  tabPanel("Overview",
    fluidRow(
      column(12,
        h2("Study Overview"),
        p("This dashboard presents analysis of coral reef ecosystems in the Solomon Islands."),
        br(),
        h4("Key Research Questions:"),
        tags$ol(
          tags$li("How does coral cover relate to Topa abundance?"),
          tags$li("What is the impact of logging on coral reef health?"),
          tags$li("How do environmental factors vary spatially?")
        )
      )
    ),
    
    if (exists("fish_data")) {
      fluidRow(
        column(4,
          h4("Dataset Summary"),
          verbatimTextOutput("data_summary")
        ),
        column(4,
          h4("Site Counts"),
          verbatimTextOutput("site_counts")
        ),
        column(4,
          h4("Mean Values"),
          verbatimTextOutput("mean_values")
        )
      )
    }
  ),
  
  # Plots Tab
  tabPanel("Research Plots",
    if (plots_loaded) {
      fluidRow(
        column(6,
          h4("Coral Cover vs Topa Abundance"),
          plotOutput("plot1", height = "400px")
        ),
        column(6,
          h4("Logging Impact on Coral Cover"),
          plotOutput("plot2", height = "400px")
        )
      )
    } else {
      fluidRow(
        column(12,
          h4("Custom Plot Generator"),
          if (data_loaded) {
            list(
              selectInput("x_var", "X Variable:",
                         choices = c("Coral Cover" = "coral_cover_prop",
                                   "Topa Abundance" = "pres.topa",
                                   "Water Clarity" = "secchi",
                                   "Distance to Logging" = "dist_to_logging_km")),
              selectInput("y_var", "Y Variable:",
                         choices = c("Topa Abundance" = "pres.topa",
                                   "Coral Cover" = "coral_cover_prop",
                                   "Water Clarity" = "secchi",
                                   "Distance to Logging" = "dist_to_logging_km")),
              plotOutput("custom_plot", height = "400px")
            )
          } else {
            p("Data not available for plotting")
          }
        )
      )
    }
  ),
  
  # Map Tab
  if (exists("fish_data")) {
    tabPanel("Interactive Map",
      fluidRow(
        column(12,
          h4("Site Locations and Variables"),
          p("Interactive map showing study site locations with coral cover and Topa abundance data."),
          leafletOutput("site_map", height = "500px")
        )
      )
    )
  },
  
  # Data Tab
  if (exists("fish_data")) {
    tabPanel("Data Explorer",
      fluidRow(
        column(12,
          h4("Dataset"),
          if (exists("dt_available") && dt_available) {
            DT::dataTableOutput("data_table")
          } else {
            tableOutput("basic_table")
          }
        )
      )
    )
  }
)

# Define Server
server <- function(input, output, session) {
  
  # Data summary outputs
  if (exists("fish_data")) {
    output$data_summary <- renderText({
      paste0(
        "Total sites: ", nrow(fish_data), "\n",
        "Variables: ", ncol(fish_data), "\n",
        "Complete cases: ", sum(complete.cases(fish_data))
      )
    })
    
    output$site_counts <- renderText({
      paste0(
        "Logged sites: ", sum(fish_data$logged == "Logged", na.rm = TRUE), "\n",
        "Not logged: ", sum(fish_data$logged == "Not logged", na.rm = TRUE), "\n",
        "Strong flow: ", sum(fish_data$flow == "Strong", na.rm = TRUE), "\n",
        "Mild flow: ", sum(fish_data$flow == "Mild", na.rm = TRUE)
      )
    })
    
    output$mean_values <- renderText({
      paste0(
        "Mean coral cover: ", round(mean(fish_data$coral_cover_prop, na.rm = TRUE) * 100, 1), "%\n",
        "Mean Topa count: ", round(mean(fish_data$pres.topa, na.rm = TRUE), 1), "\n",
        "Mean water clarity: ", round(mean(fish_data$secchi, na.rm = TRUE), 1), "m"
      )
    })
  }
  
  # Plot outputs
  if (plots_loaded && exists("p1")) {
    output$plot1 <- renderPlot({ p1 })
  }
  
  if (plots_loaded && exists("p2")) {
    output$plot2 <- renderPlot({ p2 })
  }
  
  # Custom plot for when pre-made plots aren't available
  if (exists("fish_data") && !plots_loaded) {
    output$custom_plot <- renderPlot({
      req(input$x_var, input$y_var)
      
      ggplot(fish_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(aes(color = logged), size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE) +
        labs(title = paste(input$y_var, "vs", input$x_var)) +
        theme_minimal()
    })
  }
  
  # Interactive map
  if (exists("fish_data")) {
    output$site_map <- renderLeaflet({
      leaflet(fish_data) %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = ~sqrt(pres.topa + 1) * 3 + 5,
          color = "white", weight = 2,
          fillColor = ~ifelse(logged == "Logged", "red", "blue"),
          fillOpacity = 0.7,
          popup = ~paste0(
            "<b>Site ", site, "</b><br/>",
            "Coral Cover: ", round(coral_cover_prop * 100, 1), "%<br/>",
            "Topa Abundance: ", pres.topa, "<br/>",
            "Logging Status: ", logged, "<br/>",
            "Water Clarity: ", secchi, "m"
          )
        ) %>%
        setView(lng = mean(fish_data$longitude), lat = mean(fish_data$latitude), zoom = 10)
    })
  }
  
  # Data table
  if (exists("fish_data")) {
    if (exists("dt_available") && dt_available) {
      output$data_table <- DT::renderDataTable({
        DT::datatable(fish_data, options = list(pageLength = 10, scrollX = TRUE))
      })
    } else {
      output$basic_table <- renderTable({
        head(fish_data, 20)
      }, striped = TRUE)
    }
  }
}

# Run the application
cat("Starting Shiny app on port 3838...\n")
cat("Access the app at: http://127.0.0.1:3838\n")
shinyApp(ui = ui, server = server)
