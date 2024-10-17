library(shiny);library(leaflet);library(sf);library(dplyr)

points_data <- st_read("cbsa_points.shp") 

tf_data = st_transform(points_data, crs = '+proj=longlat +datum=WGS84')


ui <- fluidPage(
        titlePanel('Housing Mortage Map'),
        sidebarLayout(
          sidebarPanel (
            sliderInput(inputId = 'mortgageRange', 
                        label = 'Mortgage Range', 
                        min = 0, 
                        max = max(tf_data$Mortgage, na.rm = TRUE),
                        value = c(0, max(tf_data$Mortgage, na.rm = TRUE))),
            
            radioButtons(inputId = 'filterOption',
                         label = 'Display Options',
                         choices = c('All', 'Filter'),
                         selected = 'All'), 
          
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


server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$filterOption == 'All') {
      tf_data  
    } else {
      tf_data %>% filter(
        Mortgage >= input$mortgageRange[1],
        Mortgage <= input$mortgageRange[2],
        State %in% input$stateFilter  
      )
    }
  })
  
  
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(st_coordinates(tf_data)[,1]), 
              lat = mean(st_coordinates(tf_data)[,2]), 
              zoom = 6)
    
  })
  
  #observe
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        radius = 3,  
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.5,
        label = ~paste("Mortgage:", Mortgage, "<br>", "State:", State)
      )
  })
}


shinyApp(ui, server)

