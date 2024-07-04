## Load Required Packages/Helper Files =========================================

packages <- list(
  "shiny",      # create Shiny application
  "bslib",      # formatting elements within Shiny application
  "bsicons",    # icons used within Shiny application
  "gpx",        # read .gpx file uploaded by user
  "tidyverse",  # data manipulation/visualization
  "ggmap",      # obtain map imagery from Google Static Map API
  "ggpattern"    # style sparkline plots with gradient 
)

lapply(packages, library, character.only = TRUE)

source("helpers.R")


## Create User Interface for Shiny Application =================================

ui <- page_sidebar(
  title = "Shiny Running Dashboard",
  sidebar = sidebar(
    fileInput(inputId = "file",
              label = "Upload .gpx File:",
              multiple = FALSE,
              accept = ".gpx"),
    hr(),
    radioButtons(inputId = "unit",
                 label = "Select Units:",
                 choices = c("Imperial", "Metric"),
                 selected = "Imperial"),
    hr(),
    radioButtons(inputId = "maptype",
                 label = "Select Map Type:",
                 choices = c("Terrain", "Roadmap", "Satellite", "Hybrid"),
                 selected = "Terrain")
  ),
  layout_columns(
    card(plotOutput("map")),
    card(plotOutput("summary")),
    value_box(title = "Total Distance",
              value = textOutput("total_distance")),
    value_box(title = "Average Pace",
              value = textOutput("average_pace"),
              showcase = plotOutput("sparkline_pace"),
              showcase_layout = "bottom"),
    value_box(title = "Average Heart Rate",
              value = textOutput("average_hr"),
              showcase = plotOutput("sparkline_hr"),
              showcase_layout = "bottom"),
    value_box(title = "Elevation Gain",
              value = textOutput("elevation_gain"),
              showcase = plotOutput("sparkline_elev"),
              showcase_layout = "bottom"),
    col_widths = c(6,6,3,3,3,3),
    row_heights = c(2.5,1)
  )
)

?value_box

## Define Server Logic for Shiny Application ===================================

server <- function(input, output) {
  
  output$map <- renderPlot({
    plot_route(input$file, input$maptype)
  })
  
  output$summary <- renderPlot({
    plot_summary(input$file, input$unit)
  })
  
  output$total_distance <- renderText({
    get_run_data_unit(input$file, input$unit)[1]
  })
  
  
  output$average_pace <- renderText({
    get_run_data_unit(input$file, input$unit)[2]
  })
  
  output$average_hr <- renderText({
    get_run_data_unitless(input$file)[1]
  })
  
  output$elevation_gain <- renderText({
    get_run_data_unit(input$file, input$unit)[3]
  })
  
  output$sparkline_pace <- renderPlot({
    sparkline_plot(input$file)[1]
  })
  
  output$sparkline_hr <- renderPlot({
    sparkline_plot(input$file)[2]
  })
  
  output$sparkline_elev <- renderPlot({
    sparkline_plot(input$file)[3]
  })
  
}


## Run Shiny Application =======================================================

shinyApp(ui = ui, server = server)