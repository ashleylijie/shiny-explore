user_verify <- function(user, password){
  
  mg <- est_mongo_conn(db$mg_user)
  
  user_info <- mg$find(sprintf('{"user":"%s", "password":"%s"}', user, password)) %>% 
    as.data.frame()
  
  rm(mg)
  
  return(user_info)
  
}

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


# establish mongo connection

write_to_db <- function(db_id, dat){
  
  mg <- est_mongo_conn(db_id)
  
  mg$insert(data.frame(dat, insert_time = Sys.time()))
  
  rm(mg)
}

# a safer post
# safely for any situation
get_api_safely <- purrr::safely(function(url, paras){
  
  resp = POST(url, body = paras, encode = "form", timeout(30))
  
  # if response is not an JSON object
  if(http_type(resp) != "application/json"){
    return(NULL)
  }
  
  # only accept 200 (success)
  if(status_code(resp) != 200){
    return(NULL)
  }
  
  # parse response
  content(resp) %>% bind_rows()
})

