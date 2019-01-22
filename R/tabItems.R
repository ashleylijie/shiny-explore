user_verify_tab <- tabItem(
    tabName = "user_verify",
    div(class = "login",
        uiOutput("uiLogin"),
        textOutput("pass"),
        tags$head(tags$style("#pass{color: red;"))
    ),    
    
    fluidRow(
        column(3,
               div(class = "span1",      
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
            sidebarPanel(width = 2,
                         
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
            mainPanel(width = 10,
                      
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
                      selectInput("area", "area", character(0)),
                      
                      
                      plotlyOutput("hist_plot", height = 200),
                      plotOutput("scatter_plot", height = 250)
        )
    )
)


# puzu_price
puzu_price_tab <- tabItem(
    tabName = "puzu_price"
)

