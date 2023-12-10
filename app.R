library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(fastmap)
library(ggplot2)
library(tidyverse)
library(ggmap)

# Load and process the census data outside the server
census <- st_read("2020_CensusData.shp", stringsAsFactors = FALSE)
districts <- st_read("City_Council_Districts.shp", stringsAsFactors = FALSE)

# Ensure both data sets are in the same CRS and are 'sf' objects
census <- st_transform(census, crs = st_crs(districts))
census_center <- st_centroid(census)

# Spatial join - attach district information to each census tract
census_districts <- st_join(census_center, districts, join = st_intersects)

# Selecting relevant columns, removing NA values, and sorting
census_clean <- census_districts %>%
  select(A14006_1, Num, Council_Me) %>%
  filter(Num != "NA") %>%
  arrange(Num)

# Define UI
ui <- fluidPage(
  titlePanel("South Bend Revitalization"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Restaurants & Abandoned Properties", 
               leafletOutput("combinedMap", height = "800px", width = "100%")),
      
      tabPanel("Redevelopment Corridors",
               fluidPage(
                 selectInput(inputId = "district", 
                             label = "Choose a city council district:", 
                             choices = c("1", "2", "3", "4", "5", "6"),
                             selected = "1"),
                 plotOutput("redev_plot", height = "800px", width = "100%"))),
      
      tabPanel("Demographic Insights", 
               sidebarLayout(
                 sidebarPanel(
                   selectInput("mapType",
                               "Select Map Type:",
                               choices = c("Population Density" = "pop_density",
                                           "Median Household Income" = "median_income"))),
                 mainPanel(
                   plotOutput("choropleth_map", height = "800px", width = "100%")))),
      
      tabPanel("Income by City Council District",
               fluidPage(
                 checkboxGroupInput(inputId = "district2",
                                    label = "Choose city council district(s): ",
                                    choices = c("1", "2", "3", "4", "5", "6"),  # Default choices
                                    selected = "1"),
                 plotOutput("income_plot", height = "800px", width = "100%"))))
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load data sets
  businessData <- read.csv("Business_Licenses_geocoded.csv")
  abandonedProperties <- st_read("Abandoned_Property_Parcels.shp")
  
  # Filter for properties that are deconstructed or demolished, dropping NA council district properties
  open_properties <- abandonedProperties %>% 
    filter(Outcome_St == "Deconstructed" | Outcome_St == "Demolished") %>% 
    filter(Council_Di != "NA")
  
  # Filter from businessData to include all "restaurant"  records
  # Restaurants are any business that has a license to serve food
  restaurantData <- businessData %>%
    filter(Classifi_1 %in% c("RESTAURANTS A-M", "RESTAURANTS N-Z")) %>%
    arrange(desc(License_Ex)) %>%
    ungroup()
  
  output$combinedMap <- renderLeaflet({
    restaurantData <- businessData %>%
      filter(Classifi_1 %in% c("RESTAURANTS A-M", "RESTAURANTS N-Z")) %>%
      group_by(Name, Address) %>%
      arrange(desc(License_Ex)) %>%
      slice(1) %>%
      ungroup()
    
    abandonedProperties <- st_read("Abandoned_Property_Parcels.shp")
    city_districts <- st_read("City_Council_Districts.shp")
    
    # Create map
    map <- leaflet() %>% addTiles()
    
    map <- map %>% addCircleMarkers(data = restaurantData, 
                                    lng = ~X, lat = ~Y, 
                                    popup = ~paste(Name, "<br>", 
                                                   ifelse(is.na(Address), "No address", Address), 
                                                   "<br>", 
                                                   "License Expiry:", 
                                                   ifelse(is.na(License_Ex), "Not available", License_Ex)),
                                    group = 'Restaurants',
                                    clusterOptions = markerClusterOptions(),
                                    color = "black")
    
    
    # Add abandoned properties data as polygons
    map <- map %>% addPolygons(data = abandonedProperties, 
                               fillColor = "black",
                               weight = 1,
                               color = "black",
                               fillOpacity = 0.7,
                               popup = ~paste(Street_Nam, Suffix, "<br>Council District:", Council_Di, "<br>Zip Code:", Zip_Code),
                               group = 'Abandoned Properties')
    
    # Generating a vector of unique colors for each district
    district_colors <- brewer.pal(6, "Accent")
    
    # Add city council districts with unique colors
    map <- map %>% addPolygons(data = city_districts, 
                               fillColor = district_colors, 
                               color = "black", 
                               weight = 1, 
                               opacity = 0.5, 
                               fillOpacity = 0.5,
                               group = 'City Council Districts')
    
    # Add layer control to toggle between layers
    map <- map %>% addLayersControl(overlayGroups = c('Restaurants', 'Abandoned Properties', 'City Council Districts'),
                                    options = layersControlOptions(collapsed = FALSE))
    
    return(map)
  })
  
  # Logic for Redevelopment Corridors
  output$redev_plot <- renderPlot({
    ggplot(data = subset(open_properties, Council_Di == input$district), 
           aes(x = fct_rev(fct_infreq(Street_Nam)), fill = as.factor(Zip_Code))) + 
      geom_bar() + 
      coord_flip() +
      xlab("Streets for Redevelopment") + 
      ylab("Number of Open Properties") + 
      labs(fill = "Zip Code")+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14))
  })
  
  # Logic for Demographic Insights
  output$choropleth_map <- renderPlot({
    shapefile <- st_read("2020_CensusData.shp")
    
    if (input$mapType == "pop_density") {
      ggplot(data = shapefile) +
        geom_sf(aes(fill = A00002_2)) +
        scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), name = "Population Density (per sq. mile)") +
        theme_minimal() +
        ggtitle("Population Density in South Bend")+
        theme(legend.text = element_text(size = 12), 
              legend.title = element_text(size = 12),
              plot.title = element_text(size = 18, hjust = 0.5))
    } else {
      ggplot(data = shapefile) +
        geom_sf(aes(fill = A14006_1)) +
        scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), name = "Median Household Income (in dollars)") +
        theme_minimal() +
        ggtitle("Median Household Income in South Bend")+
        theme(legend.text = element_text(size = 12), 
              legend.title = element_text(size = 12),
              plot.title = element_text(size = 18, hjust = 0.5))
    }
  })
  
  # Update the choices for district2 based on census_clean
  observe({
    updateCheckboxGroupInput(session, "district2", 
                             choices = unique(census_clean$Num))
  })
  
  # Logic for Income by City Council District tab
  output$income_plot <- renderPlot({
    selected_districts <- input$district2
    temp <- filter(census_clean, Num %in% selected_districts)
    
    ggplot(data = temp, aes(x = Num, y = A14006_1, fill = Council_Me)) + 
      geom_boxplot() + 
      xlab("City Council District") + 
      ylab("Distribution of Median Income") + 
      labs(fill = "City Councilmember")+
      theme(legend.text = element_text(size = 16), 
            legend.title = element_text(size = 18), 
            axis.text = element_text(size = 14), 
            axis.title = element_text(size = 16)) 
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)