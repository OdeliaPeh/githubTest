#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



# before starting, do setwd("D:/KDE_Shiny/KDE_Shiny") and then check with getwd()




pacman::p_load(shiny, maptools, sf, raster, 
        spatstat, tmap, tidyverse, leaflet,
        rasterVis, ggplot2)


main_island <- st_read(dsn="kristendata/main_island",
                       layer = "main_island")

merged_amenities <- st_read(dsn="kristendata/merged_amenities",
                      layer = "merged_amenities")

outline <- st_read(dsn="kristendata/mpsz",
                   layer="MP14_SUBZONE_WEB_PL")


# conduct_kde <- function(points_list, multipolygon_list, kernel_type = "gaussian", sigma = NULL) {
#   tryCatch({
#     # Convert points list to SpatialPoints
#     points_sp <- SpatialPoints(points_list)
#     
#     # Convert multipolygon list to SpatialPolygons
#     multipolygon_sp <- SpatialPolygons(multipolygon_list)
#     
#     # Convert to spatstat's ppp format
#     ppp_points <- as.ppp(points_sp)
#     
#     # Create owin object
#     win <- as.owin(bbox(points_sp))
#     
#     # Perform KDE
#     if (is.null(sigma)) {
#       kde_result <- density(ppp_points, kernel = kernel_type, window = win)
#     } else {
#       kde_result <- density(ppp_points, sigma = sigma, kernel = kernel_type, window = win)
#     }
#     
#     return(kde_result)
#   }, error = function(e) {
#     return(NA)
#   })
# }


conduct_kde <- function(amenity_type, area = main_island, subzone = NULL, kernel_type, sigma = bw.diggle) {
  tryCatch({
    if (!is.null(subzone)) {
      area <- outline[outline$PLN_AREA_N == subzone, ]
      amenity <- merged_amenities[merged_amenities$PLN_ARE== subzone, ]
    }
    amenity <- merged_amenities[merged_amenities$amnty_t == amenity_type, ]
    amenity_sp <- as_Spatial(amenity) %>%
      as("SpatialPoints")

    sg_sp <- as_Spatial(area) %>%
      as("SpatialPolygons")
    amenity_ppp <- as(amenity_sp, "ppp")
    sg_owin = as(sg_sp, "owin")
    amenity_SG_ppp <- amenity_ppp[sg_owin]
    amenity_SG_ppp.km <- rescale(amenity_SG_ppp, 1000, "km")
    kde_amenity_bw.km <- density(amenity_SG_ppp.km,
                              sigma,
                              edge=TRUE,
                              kernel = kernel_type)
    
    return(kde_amenity_bw.km)
    
    
    # print("it works")
    # print(class(sg_owin))
    # print(class(amenity_ppp))
  }, error = function(e) {
    print(paste("Error in conduct_kde:", e))
    return(NULL)
  })
}



# Define UI for application that draws a histogram
# Define UI
# UI
ui <- fluidPage(
  titlePanel("Kernel Density Estimation of Amenities"),
  mainPanel(
    plotOutput("mapPlot", 
               width = "100%",
               height = 400)
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "level",
                  label = "Level:",
                  choices = c("Country-wide", "Planning Area-specific")),
      selectInput(inputId = "amenities",
                  label = "Amenities:",
                  choices = unique(merged_amenities$amnty_t) ),  # Initialize with no choices
      # selectInput(inputId = "bandwidth_type",
      #             label = "Bandwidth-type:",
      #             choices = c("Low", "Medium", "High")),
      selectInput(inputId = "kernel",
                  label = "Kernel:",
                  choices = c("gaussian", "epanechnikov","quartic", "disc")),
      selectInput(inputId = "subzone_name",
                  label = "Planning Area Name:",
                  choices = NULL)
    ),
    mainPanel(
      # Placeholder for the main panel content
      textOutput("selected_options")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  kde_result <- reactive({
    if (!is.null(input$amenities) && input$level == "Country-wide") {
      conduct_kde(amenity_type = input$amenities, kernel_type = input$kernel)
    } else if (input$level == "Planning Area-specific" && !is.null(input$subzone_name)) {
      conduct_kde(amenity_type = input$amenities, subzone = input$subzone_name, area = outline, kernel_type = input$kernel)
      
    }
  })
  
  output$mapPlot <- renderPlot({
    # Check if kde_result is not NULL
    if (!is.null(kde_result())) {
      # Plot the intensity map
      plot(kde_result())  # This will plot the image object
      
    } else {
      # If kde_result is NULL, return an empty plot with an error message
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
      par(mar = c(5,5,4,2))
      text(0.8, 0.5, "Error: Unable to generate KDE. If you are in Planning Area-specific, \n the issue is likely due to the area having 0 of this facility", 
           cex=1,
           adj = c(0.3,0.5))
    }
  })
  
  # output$map <- renderLeaflet({
  #   # Perform KDE and get result
  #   kde_result <- conduct_kde(amenity_type = input$amenities,
  #                             kernel_type = input$kernel)
  #   
  #   # Check if kde_result is not NULL
  #   if (!is.null(kde_result)) {
  #     # Get extent of the KDE raster
  #     extent <- extent(kde_result)
  #     xmin <- extent[1]
  #     xmax <- extent[2]
  #     ymin <- extent[3]
  #     ymax <- extent[4]
  #     
  #     # Create leaflet map
  #     leaflet() %>%
  #       addTiles() %>%
  #       fitBounds(lng1 = xmin, lng2 = xmax, lat1 = ymin, lat2 = ymax) %>%  # Fit map to the extent of the KDE raster
  #       addRasterImage(kde_result)  # Add KDE result as raster image layer
  #   } else {
  #     # If kde_result is NULL, return empty leaflet map
  #     leaflet() %>%
  #       addTiles() %>%
  #       addMarkers(lng = 0, lat = 0, popup = "Error: Unable to generate KDE")
  #   }
  # })
  
  
  # Output selected options
  output$selected_options <- renderText({
    paste("Selected Level:", input$level, "\n",
          "Selected Amenities:", input$amenities, "\n",
          "Selected Bandwidth-type:", input$bandwidth_type,
          "Selected Kernel:", input$kernel, "\n",
          "Selected Subzone Name:", input$subzone_name)
  })
  
  observe({
    if (input$level == "Planning Area-specific") {
      updateSelectInput(session, "subzone_name", label = "Planning Area Name:",
                        choices = unique(outline$PLN_AREA_N))
    } else {
      updateSelectInput(session, "subzone_name", label = "Planning Area Name:",
                        choices = NULL)
    }
  })
  
  observe({
    if(!is.null(input$amenities) && input$level=="Country-wide") {
      kde_result <- conduct_kde(amenity_type=input$amenities, kernel_type= input$kernel)
      print(kde_result)
    }
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

