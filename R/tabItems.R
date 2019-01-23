user_verify_tab <- tabItem(
    tabName = "user_verify",
    div(class = "login",
        uiOutput("uiLogin"),
        textOutput("pass"),
        tags$head(tags$style("#pass{color: red;"))
    ),    
    
    fluidRow(
        column(6,
               div(class = "span1", 
                   top = "auto", left = "auto", right = "auto", bottom = "auto",
                   width = "auto", height = "auto",
                   uiOutput("obs")
               )
        ),
        column(8,
               div(class = "logininfo",
                   uiOutput("userPanel")
               ),
               hr(),
               div(class = "DataTable",      
                   uiOutput('dataTable')
               )     
        )      
    )
)

# sublet_pricing
sublet_pricing_tab <- tabItem(
    tabName = "sublet_pricing",
    fluidPage(
        
        # App title ----
        titlePanel("出房定价"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
            
            # Sidebar panel for inputs ----
            sidebarPanel(width = 3,
                         
                         # Input: Select a file ----
                         fileInput("file", "选择上传文件 ( *.xlsx ) :",
                                   multiple = FALSE,
                                   accept = c(".xlsx")),
                         
                         # other para ----
                         h5("定价限制条件 :"),
                         checkboxInput("include_cost", "控制回款周期", FALSE),
                         actionButton("goButton", "计算价格"),
                         
                         # Horizontal line ----
                         tags$hr(),
                         
                         radioButtons("disp", h5("预览 :"),
                                      choices = c(Room = "room",
                                                  Suite = "suite"),
                                      selected = "room"),
                         
                         tags$hr(),
                         
                         h5("下载 :"),
                         downloadButton("downloadData", "Download")
                         
            ),
            
            # Main panel for displaying outputs ----
            mainPanel(width = 9,
                      
                      verbatimTextOutput("value"),
                      tags$hr(),
                      
                      # Output: Data file ----
                      DT::dataTableOutput("contents")
                      
            )
            
        )
    )
)


# community_std_price
community_std_price_tab <- tabItem(
    tabName = "community_std_price",
    fluidPage(class="outer",
        
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
                      selectInput("area", "area", character(0))
        )
    )
)


# puzu_price
puzu_price_tab <- tabItem(
    tabName = "puzu_price",
    fluidPage(
      titlePanel("普租价计算"),
      fluidRow(
        column(width = 3, wellPanel(
          selectInput("city_puzu", "城市", choices = c("", toupper(city_list))),
          
          numericInput("xiaoqu_id", "小区ID", 
                       min = 1, max = Inf, value = NA, step = 1),
          
          sliderInput("bedroom_num", "室数 : ",
                      min = 1, max = 10, value = 2, step = 1),
          sliderInput("hall_num", "客厅数 : ",
                      min = 0, max = 10, value = 1, step = 1),
          sliderInput("toilet_num", "卫生间数 : ",
                      min = 1, max = 10, value = 1, step = 1),
          
          numericInput("building_area", "房间面积", 80),
          
          # room_status : R1，精装；R2，简装，R3，毛坯'
          # floor : L1，爬楼5层及以上；L2，爬楼一层，L3，正常楼层'
          # enviorment_level : N1，安静卫生；N2，吵闹脏乱',
          selectInput("has_floor", "楼层类型", choices = c("", "L1 : 爬楼5层及以上", "L2 : 爬楼一层", "L3 : 正常楼层")),
          selectInput("decoration", "装修水平", choices = c("" ,"R1 : 精装", "R2 : 简装", "R3 : 毛坯")),
          
          br(),
          actionButton("goButton_puzu", "计算价格")
        )),
        column(9,
               h4("普租价"),
               DT::dataTableOutput("puzu_price")
        )
      )
    )
)

