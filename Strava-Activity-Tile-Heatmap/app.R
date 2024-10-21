## Load Required Libraries/Helper Functions ====================================

library(shiny)
library(bslib)
library(tidyverse)

source("helpers.R")


## Create User Interface =======================================================

ui <- page_sidebar(
  
  title = "2024 Strava Activity Heatmap",
  sidebar = sidebar(
    fileInput(inputId = "file",
              label = "Upload activities.csv File:",
              multiple = FALSE,
              accept = ".csv"),
    selectInput(inputId = "activity_type",
                label = "Chose Activity",
                choices = c("Run", "Walk", "Bike", "Swim", "ALL"),
                selected = "Run"),
    selectInput(inputId = "fill_variable",
                label = "Choose Fill Variable",
                choices = c("Distance", "Time"),
                selected = "Distance"),
    selectInput(inputId = "color",
                label = "Choose Fill Color",
                choices = c("Green", "Blue", "Red", "Cyan"),
                selected = "Green"),
    sliderInput(inputId = "range",
                label = "Select Fill Gradient",
                min = 0.1,
                max = 1,
                step = 0.1,
                value = c(0.5, 1))
  ),
  card(p("1. Login into your Strava account."),
         p("2. Navigate to Settings > My Account > Download or Delete Your Account."),
         p("3. Request an archive of your account. A folder containing your data will be emailed to you."),
         p("4. Download the folder and unzip it. Upload the activities.csv file on the left.")),
  card(plotOutput("heatmap"))
  
)


## Define Server Logic  ========================================================

server <- function(input, output) {
  
  output$heatmap <- renderPlot({
    plot_heatmap(input$file, input$activity_type, input$fill_variable, input$color, input$range)
  })
  
}


## Run Shiny Application =======================================================

shinyApp(ui = ui, server = server)