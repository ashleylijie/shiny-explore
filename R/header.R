library(tidyverse)
library(mongolite)
library(DBI)
library(httr)
library(RMySQL)
library(shiny)
library(openxlsx)
library(DT)
library(base64enc)
library(dkbi)
library(shinydashboard)
library(scales)
library(lattice)
library(dplyr)
library(tidyverse)
library(shiny)                                                                                    
library(shinythemes)                                                                                 
library(RColorBrewer)                                                                                
library(leaflet)                                                                                 
library(plotly)


# get token & http url
get_api_paras <- function(api_id, params = c("token", "host", "port", "swagger")){
  
  api_config <- get_batch_kv(api_id, params)
  
  # check if the credentials are specified
  if(!all(c("token", "host", "port", "swagger") %in% names(api_config))){
    stop(paste("One or more parameter in", which_api, "is missing."))
  }
  
  # return a list of token and endpoint
  list(
    token = api_config$token,
    endpoint = with(api_config, sprintf("%s:%s/%s", host, port, swagger))
  )
  
}
