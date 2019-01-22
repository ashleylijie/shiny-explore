# helpers
source("R/config.R")
source("R/header.R")
# source("R/user_verify.R")
source("R/get_sublet_pricing_tab.R")
source("R/get_xiaoqu_data.R")
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



