library(GeoShiny)
library(shiny)
library(googleway)
library(shinyjs)
library(httr)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("Address Locations in World Map"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 12,
      textInput("map_key", "Enter Map Key", ""),
      br(),
      fluidRow(
        column(6,
               h4("Get Geo Location"),
               br(),
               position = "center",
               textInput("Address", "Enter Address", "Sweden"),
               br(),
               actionButton("getgeolocation", "Get Geo Location",style="color: #fff;
                            background-color:#337ab7; border-color: #2e6da4")
               ),
        
        column(6, h4("Get Address"),
               br(),
               numericInput("lon", "Longitude", min = -180, max = 180, value = 18.6435),
               numericInput("lat", "Latitude", min = -90, max = 90, value = 60.1282),
               actionButton("getlatlong","Get Address", style="color: #fff; background-color:
                            #337ab7; border-color: #2e6da4")
               )
      )
      
      
      
      
    ),
    
    mainPanel(width=12,
              br(),
              verbatimTextOutput("printAddress",placeholder = FALSE),
              br(),
              google_mapOutput(outputId ="mapplot"))
  )
)



server <- function(input, output, session) {
  
  
  
  observeEvent(input$getgeolocation,{
    useShinyjs()
    cordinatematrix <- GeoShiny::geocode_response(input$Address,input$map_key)
    df = data.frame(lat = cordinatematrix[1], lon = cordinatematrix[2])
    output$printAddress = renderText(paste("lat",cordinatematrix[1],"lon",cordinatematrix[2]))
    
    output$mapplot <- renderGoogle_map({
      google_map(key = input$map_key,
                 location = c(cordinatematrix[1], cordinatematrix[2]),
                 zoom = 10,
                 split_view = "pano")%>%
        add_markers(data =df, lat = "lat", lon = "lon", marker_icon = "", update_map_view = FALSE)
      
    })
  }
  )
  
  observeEvent(input$getlatlong,{
    useShinyjs()
    cordinatematrix = c(input$lat,input$lon)
    df = data.frame(lat = input$lat, lon = input$lon)
    address =  GeoShiny::reverse_geocode_response(input$lat,input$lon, input$map_key)
    
    output$printAddress = renderText(address)
    output$mapplot = renderGoogle_map({
      google_map(key = input$map_key,
                 location = c(cordinatematrix[1], cordinatematrix[2]),
                 zoom = 10,
                 split_view = "pano")%>%
        add_markers(data =df, lat = "lat", lon = "lon", marker_icon = "", update_map_view = FALSE)
      
    })
  }
  
  
  )}
# Run the application
shinyApp(ui = ui, server = server)
