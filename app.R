# helpers
source("R/config.R")
source("R/header.R")
source("R/helper.R")
source("R/get_sublet_pricing_tab.R")
source("R/get_xiaoqu_data.R")
source("R/get_puzu_price.R")
source("R/tabItems.R")


# load libraries
library(shinydashboard)


# body --------------------------------------------------------------------
body <- dashboardBody(
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
    includeScript("gomap.js")
  ),
  tabItems(
    user_verify_tab,
    sublet_pricing_tab,
    community_std_price_tab,
    puzu_price_tab
  )
  # tabItems ends here
)


# sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("登录", tabName = "user_verify"),
    menuItem("出房定价", tabName = "sublet_pricing", icon = icon("jpy")),
    menuItem("小区间均价", tabName = "community_std_price", icon = icon("indent")),
    menuItem("普租市场价", tabName = "puzu_price", icon = icon("signal"))
  )
)



# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DKBI Dashboard",
                  dropdownMenu(type = "messages",
                               headerText = NULL,
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = Sys.Date()
                               )
                  )),
  sidebar,
  body
  
)



server <- function(input, output, session) {
  
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
      
      # 获取接口数据，get_sublet_price(dat, api_name = "sublet_pricing_api")
      result <- input_params[i, ] %>% 
        get_sublet_price(params = config)
      
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
    
    write_to_db(db$mg_web, data.frame(outprice, USER$name))
    
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
    
    if (!USER$Logged) {
      stop("User or Password verification failed.")
    }
    
    type_cols <- brewer.pal(selected_city()$xiaoqu_std_price_type %>% unique() %>% length(), "Paired")
    pal <- colorFactor(type_cols, domain = selected_city()$xiaoqu_std_price_type %>% unique())
    
    selected_city() %>% 
      leaflet() %>% 
      addTiles() %>%
      addProviderTiles(providers$Wikimedia) %>% 
      setView(lng = city_location[city_location$city == input$city, ]$center_longitude, 
              lat = city_location[city_location$city == input$city, ]$center_latitude, 
              zoom = 11) %>% 
      addCircles(~longitude, ~latitude, stroke=FALSE, 
                 # fillOpacity = 0.4,
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
    
    type_cols <- brewer.pal(selected_city()$xiaoqu_std_price_type %>% unique() %>% length(), "Paired")
    pal <- colorFactor(type_cols, domain = selected_city()$xiaoqu_std_price_type %>% unique())
    leafletProxy("map", data = area_df()) %>% 
      clearMarkers() %>% 
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       color = ~pal(xiaoqu_std_price_type),
                       radius = 20,
                       stroke = FALSE,
                       popup = ~paste0("小区ID : ", xiaoqu_id, "<br>",
                                       "小区 : ", community, "<br>",
                                       "小区间均价 : ", xiaoqu_std_price, "<br>",
                                       "小区间均价类型 : ", xiaoqu_std_price_type, "<br>",
                                       "商圈距离/米 : ", subway_distance),
                       fillOpacity = 0.5) %>%
      addPopups(lng = ~block_longitude[1],
                lat = ~block_latitude[1],
                popup = ~block[1],
                options = popupOptions(keepInView = TRUE)) %>% 
      addMarkers(lng = ~block_longitude[1],
                 lat = ~block_latitude[1],
                 popup = ~block[1]) %>% 
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
  
  
  # puzu price --------------------------------------------------------------
  puzu_input <- eventReactive(input$goButton_puzu, ({
    
    if (!USER$Logged) {
      stop("User or Password verification failed.")
    }
    
    puzu_input <- data.frame(city = isolate(input$city_puzu),
                             xiaoqu_id = isolate(input$xiaoqu_id),
                             bedroom_num = isolate(input$bedroom_num),
                             hall_num = isolate(input$hall_num),
                             toilet_num = isolate(input$toilet_num),
                             building_area = isolate(input$building_area),
                             has_floor = isolate(input$has_floor),
                             decoration = isolate(input$decoration))
    
  })
  
  )
  
  
  
  output$puzu_price <- renderDataTable({
    
    # init progress bar
    progress <- Progress$new(session, min = 1, max = nrow(puzu_input()), style = "old")
    # close progress on exit
    on.exit(progress$close())
    
    for( i in 1:nrow(puzu_input())){
      
      progress$set(message = 'Calculation in progress',
                   detail = sprintf('Complete %d row', i))
      
      progress$set(value = i)
      
      config <- get_api_paras(api$puzu_api_id)
      # Any sort of expression can go in isolate()
      puzu_price <- puzu_input() %>% 
        get_puzu_price(params = config)
      
      write_to_db(db$mg_puzu, data.frame(puzu_price, USER$name))
      
      puzu_price <- puzu_price %>% 
        DT::datatable(colnames = c("城市", "小区ID", "室数", "客厅数", "卫生间数", "建筑面积", "楼层类型", "装修水平",
                                   "预估房态", "普租价", "价格类型", "小区间均价", "Status", "Remark"))
      
      return(puzu_price)
      
    }
    
  })
  
}

shinyApp(ui, server = server)
