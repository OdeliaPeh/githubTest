
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shiny, bslib, maptools, sf, raster, 
               spatstat, tmap, tidyverse, leaflet,
               rasterVis, ggplot2, tmap, SpatialAcc, sp, tidyverse)

# kristen data
main_island <- st_read(dsn="kristendata/main_island",
                       layer = "main_island")

merged_amenities <- st_read(dsn="kristendata/merged_amenities",
                            layer = "merged_amenities")

outline <- st_read(dsn="kristendata/mpsz",
                   layer="MP14_SUBZONE_WEB_PL")

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


# binhui data
hex_sf <- read_rds("data/rds/hex_sf.rds")

eldercare_distmat_km <- readRDS("data/rds/eldercare_distmat_km.rds")
garden_distmat_km <- readRDS("data/rds/garden_distmat_km.rds")
chas_distmat_km <- readRDS("data/rds/chas_distmat_km.rds")
hospital_distmat_km <- readRDS("data/rds/hospital_distmat_km.rds")
communitycentre_distmat_km <- readRDS("data/rds/communitycentre_distmat_km.rds")
park_distmat_km <- readRDS("data/rds/park_distmat_km.rds")

eldercare_cap <- read_rds("data/rds/eldercare_cap.rds")
garden_cap <- read_rds("data/rds/garden_cap.rds")
chas_cap <- read_rds("data/rds/chas_cap.rds")
hospital_cap <- read_rds("data/rds/hospital_cap.rds")
communitycentre_cap <- read_rds("data/rds/communitycentre_cap.rds")
park_cap <- read_rds("data/rds/park_cap.rds")

# Define UI
ui <- page_navbar(
  title = "ELDANALYZE: Services for the Elderly in Singapore",
  bg = "#f7ce4d",
    nav_panel("Spatial Points Pattern Analysis (KDE)", p(
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
                 textOutput("selected_options"),
                 plotOutput("mapPlot")
               )
             )
             )
  ),
  
nav_panel("Accessibility Analysis", p(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "facility",
        label = "Type of Facility:",
        choices = c("Eldercare Centres", "Community Gardens", "CHAS Clinics", "Hospitals", "Community Centres", "Parks"),
        selected = "Eldercare Centres"
      ),
      selectInput(
        inputId = "method",
        label = "Accessibility Analysis Method:",
        choices = c("Hansen", "KD2SFCA", "SAM"),
        selected = "Hansen"
      ),
      sliderInput(
        inputId = "thres_dist",
        label = "Maximum Distance (in kilometres):",
        min = 0.05,
        max = 1,
        value = 0.1
      ),
      selectInput(
        inputId = "vis_class",
        label = "Classification Method:",
        choices = c("pretty", "quantile", "fisher", "equal", "jenks"),
        selected = "pretty"
      )
    ),
    mainPanel(
      plotOutput("plot"),
      p("              "),
      p(strong("Understanding the Classification Methods:")),
      p(" - Pretty: Computes a sequence of roughly equally spaced values"),
      p(" - Quantile: Same number of features per class"),
      p(" - Fisher: Uses Fisher's discriminant analysis for dimensionality reduction and classification of spatial data."),
      p(" - Equal Interval: Divides the range of attribute values into equally sized classes"),
      p(" - Jenks: Finds natural groupings in the data"),
      p(strong("Understanding the Accessibility Analysis Methods:")),
      p(" - Hansen Accessibility Index: A simple yet effective method for measuring accessibility. It calculates accessibility by summing the opportunities (e.g., eldercare centres, parks) within a certain travel time or distance from a given origin"),
      p(" - KD2SFCA (Two-Step Floating Catchment Area): KD2SFCA is an extension of the classic Gravity Model and Floating Catchment Area (FCA) method."),
      p(" - Spatial Accessibility Model (SAM): A comprehensive framework for assessing accessibility, It considers multiple factors such as travel time, distance, and spatial distribution of both demand (population) and supply (e.g., services, resources).")
    )
  )
    )),

nav_spacer(),
nav_menu(
  title = "Other Links",
  align = "right",
  nav_item(tags$a("Project Website", href = "https://project-gaa-t4.netlify.app/")),
  nav_item(tags$a("User Guide", href = "https://shiny.posit.co"))
)

)


# Define server logic
server <- function(input, output, session) {
  
# kristen
  
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
      plot(kde_result(), main = "Kernel Density Estimation")  # This will plot the image object
      
    } else {
      # If kde_result is NULL, return an empty plot with an error message
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
      par(mar = c(5,5,4,2))
      text(0.8, 0.5, "Error: Unable to generate KDE. If you are in Planning Area-specific, \n the issue is likely due to the area having 0 of this facility", 
           cex=1,
           adj = c(0.3,0.5))
    }
  })
  
  
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
  
  
# bin hui
  output$plot <- renderPlot({
    # Read the appropriate data based on user input
    
    cap <- switch(input$facility, 
                  "Eldercare Centres" = eldercare_cap,
                  "Community Gardens" = garden_cap,
                  "CHAS Clinics" = chas_cap,
                  "Hospitals" = hospital_cap,
                  "Community Centres" = communitycentre_cap,
                  "Parks" = park_cap)
    
    distmat <- switch(input$facility,
                      "Eldercare Centres" = eldercare_distmat_km,
                      "Community Gardens" = garden_distmat_km,
                      "CHAS Clinics" = chas_distmat_km,
                      "Hospitals" = hospital_distmat_km,
                      "Community Centres" = communitycentre_distmat_km,
                      "Parks" = park_distmat_km)
  
    # ac  computation
    
    acc_result <- data.frame(ac(hex_sf$hexagon_demand,
                                           cap$capacity,
                                           distmat,
                                           d0 =input$thres_dist,
                                           power = 2,
                                           family = input$method))
    colnames(acc_result) <- "acc_value"
    acc_result <- as_tibble(acc_result)
    hexagon_result <- bind_cols(hex_sf, acc_result)

    # Create tmap plot
    tmap_mode("plot")
    tm_shape(hexagon_result) + 
      tm_fill(col = 'acc_value',
              n = 10,
              style = input$vis_class,
              na.rm = TRUE,
              border.col = "black",
              border.lwd = 1) +
      tm_shape(cap) +
      tm_symbols(size = 0.1) +
      tm_layout(main.title = paste0("Accessibility of ", input$facility, " (", input$method, " method)"),
                main.title.position = "center",
                main.title.size = 1,
                legend.outside = FALSE,
                legend.height = 0.45, 
                legend.width = 3.0,
                legend.format = list(digits = 6),
                frame = TRUE) +
      tm_scale_bar(width = 0.15) +
      tm_grid(lwd = 0.1, alpha = 0.5)
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
