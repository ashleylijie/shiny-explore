library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(tidyverse)
library(shiny)                                                                                    
library(shinythemes)                                                                                 
library(RColorBrewer)                                                                                
library(leaflet)                                                                                 
library(plotly)

# Set main color for room type
type_cols <- brewer.pal(xiaoqus$xiaoqu_std_price_type %>% unique() %>% length(), "Set3")

# For Leaflet's markers color
pal <- colorFactor(type_cols, domain = xiaoqus$xiaoqu_std_price_type %>% unique())

# Modify ggplot theme
old <- theme_set(theme_light() + 
                     theme(legend.position = "none", 
                           axis.title = element_text(family = "Menlo", colour = "navyblue"),
                           axis.text = element_text(family = "Menlo")
                     ))

# Load Data ---------------------------------------------------------------
# source("R/get_data.R")
source("R/global.R")
# Helper Function ---------------------------------------------------------

ui <- navbarPage("Community Standard Price", id="nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Community Standard Price"),
                                            
                                            selectInput("city", "City", choices = c("", toupper(city_list))),
                                            selectInput("area", "area", character(0)),
                                            
                                            
                                            plotlyOutput("hist_plot", height = 200),
                                            plotOutput("scatter_plot", height = 250)
                              )
                          )
                 )
)






server <- function(input, output, session){
    
    # Select a city from a list of cities
    selected_city <- reactive({
        req(input$city)
        cities[[input$city]]
    })
    
    # Text for the main header
    output$h4 <- renderUI({
        text <- ifelse(isTruthy(input$city), input$city, "Select A City To Get Started.")
        h4(text, style = "text-align: center;")
    })
    
    # Geography concentration goes here
    output$map <- renderLeaflet({
        
        selected_city() %>% 
            leaflet() %>% 
            addTiles() %>%
            addProviderTiles(providers$Thunderforest.Transport) %>% 
            setView(lng = city_location[city_location$city == input$city, ]$center_longitude, 
                    lat = city_location[city_location$city == input$city, ]$center_latitude, 
                    zoom = 10) %>% 
            addCircles(~longitude, ~latitude, stroke=FALSE, 
                       fillOpacity=0.4,
                       color = "darkblue", fill = TRUE) %>% 
            # customise viewport to fit
            # fitBounds(lng1 = ~min_lon, 
            #           lat1 = ~min_lat, 
            #           lng2 = ~max_lon, 
            #           lat2 = ~max_lat) %>% 
            addLegend("bottomleft", pal = pal, values = ~xiaoqu_std_price_type, title = "Type") %>%
            # For resetting zoom
            addEasyButton(
                easyButton("fa-arrows-alt", title = "Reset Zoom",
                           onClick = JS("function(btn, map){ map.setZoom(11); }")))
        
        
    })
    
    
    # Sublevel of city
    area <- reactive({ 
        # some cities have a larger subcity cluster called neightbourhood_group
        if(sum(is.na(selected_city()[["block"]] > 10))){
            unique(selected_city()[["block"]])
        }else{
            unique(selected_city()[["block"]])
        }
    })
    
    # Generate neighbourhood selection based on selected area (dynamic UI)
    observe({
        updateSelectInput(session, "area", choices = c("", area()))
    })
    
    # A subset of city data frame based on selected area
    area_df <- reactive({
        req(input$area)
        # depends on data available
        selected_city() %>% filter(block == input$area)
        
    })
    
    # Prepare neighbourhood bounding lng and lat for Leaflet proxy
    bounds <- reactive({
        list(
            lng = range(area_df()$longitude),
            lat = range(area_df()$latitude)
        )
    })
    
    # Update data points within current bounding box
    bounded_area <- reactive({
        
        req(input$map_bounds, cancelOutput = TRUE)
        
        # Get map boundary from Leaflet
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        # Filter area given boundary
        subset(area_df(), 
               latitude >= latRng[1] & 
                   latitude <= latRng[2] & 
                   longitude >= lngRng[1] & 
                   longitude <= lngRng[2])
        
    })
    
    # Leaflet proxy to modify map aspect (add markers here)
    observeEvent(input$area, {
        leafletProxy("map", data = area_df()) %>% 
            clearMarkers() %>% 
            addCircleMarkers(lng = ~longitude,
                             lat = ~latitude,
                             color = ~pal(xiaoqu_std_price_type),
                             radius = 5,
                             stroke = FALSE,
                             popup = ~paste0(xiaoqu_id, "<br>",
                                             community, "<br>",
                                             xiaoqu_std_price),
                             fillOpacity = 0.5) %>%
            # addPopups(lng = ~longitude, lat = ~latitude, 
            #           popup = ~paste0(xiaoqu_id, "<br>",
            #                           community, "<br>",
            #                           xiaoqu_std_price),
            #           options = popupOptions(autoPan = TRUE)) %>% 
            # Fit bounding box based on neighbourhood
            fitBounds(lng1 = bounds()$lng[1], 
                      lng2 = bounds()$lng[2], 
                      lat1 = bounds()$lat[1], 
                      lat2 = bounds()$lat[2])
    })
    
    # Calculate total of respective room type (within bounding box)
    output$roomInBounds <- renderPrint({
        # Total count by room type
        df <- bounded_area() %>% group_by(xiaoqu_std_price_type) %>% summarise(n = n()) %>% as.data.frame(row.names = NULL)
        colnames(df) <- c("xiaoqu_std_price_type", "Quantity")
        df
        
    })
    
    
    # Data points for price density and listings per host analysis
    rv <- reactive({
        
        # ** Require this to trigger first selected city  **
        input$map_bounds
        
        # If subcity (area) is not selected, fall back to city data frame
        if(!input$area == ""){
            bounded_area()
        }else{
            selected_city()
        }
        
    })
    
    # Slow down reactive expression to prevent invalidation when switching city
    rv_d <- rv %>% debounce(750)
    
    # Price distribution goes here
    output$price_hist <- renderPlotly({
        
        # Prevent invalidation when switching from area to area (further investigation required)
        withProgress(message = "Rendering...", value = 0.5, {
            p <- rv_d() %>% 
                # filter right tail outlier using Tukey's IQR method
                filter(xiaoqu_std_price < (1.5 * IQR(xiaoqu_std_price) + quantile(xiaoqu_std_price, .75))) %>% 
                ggplot(aes(xiaoqu_std_price, fill = xiaoqu_std_price_type, col = xiaoqu_std_price_type, text = "")) +
                geom_density(alpha = 0.6) + 
                scale_color_manual(values = type_cols) +
                scale_fill_manual(values = type_cols) +
                labs(x = "xiaoqu_std_price", y = "Kernel Density Estimation")
            
            setProgress(1)
            
        })
        
        ggplotly(p, tooltip = c("text"))
        
    })
    
    # Listings per host goes here
    output$price_scatter <- renderPlotly({
        
        withProgress(message = "Rendering...", value = 0,5, {
            p <- rv_d() %>% 
                group_by(xiaoqu_std_price_type, xiaoqu_std_price_cut) %>% 
                summarise(n = n_distinct(xiaoqu_id)) %>% 
                # Do a count on n (how many hosts own 3, 4..n houses?)
                ungroup() %>% count(n) %>% 
                filter(n > 1) %>% 
                ggplot(aes(n, nn, text = paste("# Listings:", n, "\n# Hosts:", nn))) + 
                # geom_hline(aes(yintercept = 0), lty = 3) +
                geom_bar(stat = 'identity', width = 0.1, fill = "skyblue", alpha = 0.6) + 
                geom_point(size = 3, col = "royalblue") +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
                coord_flip() + 
                labs(x = "# Listings", y = "# Hosts with y listings")    
            
            setProgress(1)
            
        })
        
        ggplotly(p, tooltip = c("text"))
        
    })
    
}

# run App
shinyApp(ui = ui, server = server)







