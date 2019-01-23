# Consul Configuration ----------------------------------------------------
library(curl)

# init
consul <- new.env()

# default read from .Renviron. If null, switch to machine hostname
if (is.null(nslookup(Sys.getenv("consul.host"), error = FALSE))) {
  consul$host <- Sys.info()[["nodename"]]
  
} else {
  consul$host <- Sys.getenv("consul.host")
  
}

# not changing port and swagger from .Renviron
consul$port = Sys.getenv("consul.port")
consul$swagger = Sys.getenv("consul.swagger")


# Databases Declaration ---------------------------------------------------

# init
db <- new.env()
db$Laputa <- "Laputa"
db$Forecast <- "Forecast"
db$Laputa <- "Laputa"
db$DW <- "DW"
db$Monitor <- "Monitor"
db$DW_Temp <- "DW_Temp"
db$Fico <- "Fico"
db$mg_user <- "sublet_pricing_user"
db$mg_web <- "sublet_pricing_web"
db$mg_puzu <- "sublet_pricing_web_puzu_price"

# api_id ------------------------------------------------------------------
api <- new.env()
api$api_id <- "sublet_pricing_api_bizops"
api$puzu_api_id <- "external_sublet_market_pricing_api_bizops"

# Validation Token If Needed ----------------------------------------------

# function as a service
validate_token <- function(token) {
  # return true/false
  token == Sys.getenv("common.secret")
  
}
