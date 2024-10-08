library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Read and transform data
points_data <- st_read("cbsa_points.shp") 
tf_data <- st_transform(points_data, crs = '+proj=longlat +datum=WGS84')

# UI
ui <- fluidPage(
  titlePanel('Housing Mortgage Map'),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = 'mortagagerange', 
                  label = 'Mortgage Range', 
                  min = 0, 
                  max = max(tf_data$Mortgage, na.rm = TRUE), 
                  value = c(min(tf_data$Mortgage, na.rm = TRUE), max(tf_data$Mortgage, na.rm = TRUE))
      ),
      
      selectInput(inputId = 'stateFilter',
                  label = 'Select State',
                  choices = unique(tf_data$State),
                  selected = unique(tf_data$State)[1],
                  multiple = TRUE
      )
    ),
    mainPanel(
      leafletOutput(outputId = 'map', width = '100%', height = '500px')
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    tf_data %>% filter(
      Mortgage >= input$mortagagerange[1],
      Mortgage <= input$mortagagerange[2],
      State %in% input$stateFilter
    )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(st_coordinates(tf_data)[,1]), 
              lat = mean(st_coordinates(tf_data)[,2]), 
              zoom = 6)
  })
  
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = 5,
                       color = "blue",
                       label = ~paste("Mortgage:", Mortgage, "State:", State))
  })
}


shinyApp(ui, server)
