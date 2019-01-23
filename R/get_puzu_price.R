

puzu_input_check <- function(puzu_input){
  
  puzu_input
}



get_puzu_price <- function(dat, params = config){
  
  # 市场价价格获取接口地址
  safe_res <- get_api_safely(url = params$endpoint,
                             paras = data.frame(token = params$token,
                                                dat))
  
  if(is.null(safe_res$result)){
    
    res <- data.frame(status = 0,
                      remark = paste("External API :", api$api_id, "error. ", safe_res$error))
    
    message(res$remark)
    
  }else{
    
    res <- safe_res$result
    
  }
  
  if (res$status[1] == 1) {
    
    result <- cbind(dat,
                    res %>% 
                      mutate(market_price = as.numeric(market_price),
                             estimate_decoration = decoration) %>% 
                      select(estimate_decoration, market_price, price_type, community_std_price,
                             status, remark))
    
    message("get market price")
    
  }else{
    
    result <- cbind(dat,
                    estimate_decoration = NA, 
                    market_price = NA,
                    price_type = NA,
                    community_std_price = NA,
                    status = res$status,
                    remark = res$remark)
    
    message("no market price")
    
  }
  
  result
  
}