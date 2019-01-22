


function(input, output, session) {
   
    USER <- reactiveValues(Logged = FALSE , session = session$user) 
    
    source("R/Login.R", local = TRUE)
    
    # Calculate
    dataoutput <- eventReactive(input$goButton, {
        
        if (!USER$Logged) {
            stop("User or Password verification failed.")
        }
        
        req(input$file)
        
        input_dat <- isolate(openxlsx::read.xlsx(input$file$datapath,
                                                 colNames = FALSE, 
                                                 skipEmptyRows = TRUE,
                                                 skipEmptyCols = TRUE))
        
        # 对输入文件进行处理，只保留文件中的code
        code_input <- tidy_input(input_dat)
        
        if(length(code_input) == 0){
            
            stop("Invalid room code input.")
            
        }else{
            
            message("Upload complete!")
            
        }
        
        # 根据code从数据库提取定价接口所需id信息(suite_id, xiaoqu_id, contract_id)
        room_data <- get_room_data(code_input)
        
        # init progress bar
        progress <- Progress$new(session, min = 1, max = nrow(code_input), style = "old")
        # close progress on exit
        on.exit(progress$close())
        
        # 是否限制回款周期
        include_cost <- isolate(input$include_cost)
        
        # 房间信息转换为定价接口所需格式并去重
        input_params <- room_data %>% 
            api_paras_prepare(include_cost)
        
        outprice <- NULL
        config <- get_api_paras(api$api_id)
        
        for (i in 1:nrow(input_params)) {
            
            progress$set(message = 'Calculation in progress',
                         detail = sprintf('Complete %d row', i))
            
            progress$set(value = i)
            
            message(sprintf("~~~~~~~~~~~~~~~~~~~~~~~Calculate %d row of data...~~~~~~~~~~~~~~~~~~~~~~~", i))
            
            # 获取接口数据，get_api_data(dat, api_name = "sublet_pricing_api")
            result <- input_params[i, ] %>% 
                get_api_data(params = config)
            
            # 成功获取定价结果数据
            if(result$status[1] == 1){
                
                result <- result %>% 
                    left_join(room_data %>% 
                                  select(-(city:community)), 
                              by = c("code", "suite_id"))
                
            }else{
                
                result <- result %>% 
                    left_join(room_data %>% filter(label == "input"), 
                              by = "suite_id")
                
            }
            
            outprice <- outprice %>% 
                bind_rows(result)
            
        }
        
        message(sprintf("~~~~~~~~~~~~~~~~~~~~~~~Output %d rows price result.~~~~~~~~~~~~~~~~~~~~~~~", nrow(outprice)))
        
        write_to_db(data.frame(outprice, USER$name))
        
        return(outprice)
            
    })
    
    
    
    # display
    output$contents <- renderDataTable({
        
        show_df <- dataoutput()
        
        if(sum(show_df$status) == 0){
            
            return(show_df)
            
        }
        
        show_df <- show_df %>% 
            select(status:suite_id, code, price_online, room_price, label)
        
        if(input$disp == "room") {
            
            show_df <- show_df %>% 
                filter(label == "input")
            
        }
        
        return(show_df)
    })
    
    
    # Download
    output$downloadData <- downloadHandler(
        
        filename = function() {
            paste0("Price_output_", Sys.Date(), ".xlsx")},
        content = function(file) {
            openxlsx::write.xlsx(dataoutput(), file, row.names = FALSE)}
    )
    
    
    
    
    
    # ######community_std_price------
    
    # Select a city from a list of cities
    selected_city <- reactive({
        req(input$city)
        
        xiaoqus <- get_xiaoqus(input$city)
    })
    
    
    # For Leaflet's markers color
    
    
    # Modify ggplot theme
    old <- theme_set(theme_light() + 
                         theme(legend.position = "none", 
                               axis.title = element_text(family = "Menlo", colour = "navyblue"),
                               axis.text = element_text(family = "Menlo")
                         ))
    
    
    # Text for the main header
    output$h4 <- renderUI({
        text <- ifelse(isTruthy(input$city), input$city, "Select A City To Get Started.")
        h4(text, style = "text-align: center;")
    })
    
    # Geography concentration goes here
    output$map <- renderLeaflet({
        
        # if (!USER$Logged) {
        #     stop("User or Password verification failed.")
        # }
        
        type_cols <- brewer.pal(selected_city()$xiaoqu_std_price_type %>% unique() %>% length(), "Set3")
        pal <- colorFactor(type_cols, domain = selected_city()$xiaoqu_std_price_type %>% unique())
        
        selected_city() %>% 
            leaflet() %>% 
            addTiles() %>%
            addProviderTiles(providers$Thunderforest.Transport) %>% 
            setView(lng = city_location[city_location$city == input$city, ]$center_longitude, 
                    lat = city_location[city_location$city == input$city, ]$center_latitude, 
                    zoom = 10) %>% 
            addCircles(~longitude, ~latitude, stroke=FALSE, 
                       fillOpacity = 0.4,
                       radius = 20,
                       color = "darkblue", fill = TRUE) %>% 
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
                             radius = 20,
                             stroke = FALSE,
                             popup = ~paste0(xiaoqu_id, "<br>",
                                             community, "<br>",
                                             xiaoqu_std_price),
                             fillOpacity = 0.5) %>%
            addMarkers(lng = ~block_longitude[1],
                             lat = ~block_latitude[1],
                             popup = ~block,
                             options = popupOptions(keepInView = TRUE)) %>% 
            addLegend("bottomleft", pal = pal, values = ~xiaoqu_std_price_type, title = "Type") %>% 
            # addPopups(lng = ~longitude, lat = ~latitude, 
            #           popup = ~paste0(xiaoqu_id, "<br>",
            #                           community, "<br>",
            #                           xiaoqu_std_price),
            #           options = popupOptions(autoPan = TRUE)) %>% 
            # Fit bounding box based on neighbourhood
            fitBounds(lng1 = bounds()$lng[1] - 0.005,
                      lng2 = bounds()$lng[2] + 0.005,
                      lat1 = bounds()$lat[1] - 0.005,
                      lat2 = bounds()$lat[2] + 0.005)
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