# Wed Jul  4 20:09:28 2018 ------------------------------


tidy_input <- function(input_dat){
  
  sample <- input_dat %>% 
    sample_n(size = min(10, nrow(input_dat)))
  
  weight <- map(sample, function(x){mean(grepl(pattern = "[0-9]+[-][A-Z]", x, ignore.case = NA))})
  
  code_input <- input_dat[, which.max(weight)] %>% 
    str_extract(pattern = "[0-9]+[-][A-Z]{1}$") %>% 
    na.omit() %>% 
    as.character()
  
  return(code_input)
  
}

# 获取ID及原始线上价格 -------------------------------------------------------------

get_room_data <- function(code_input){
  
  suite_list <- code_input %>% 
    str_extract(pattern = "[0-9]+") %>% 
    as.numeric()
  
  Laputa <- est_mysql_conn(db$Laputa)
  DBI::dbSendQuery(Laputa, statement = "SET NAMES utf8")
  
  rooms <- tbl(Laputa, "rooms") %>% 
    filter(suite_id %in% suite_list) %>% 
    filter(is.na(deleted_at)) %>% 
    filter(!(rent_type %in% c("整租", "蛋壳租房_整租") & room_number != 'A')) %>% 
    select(code, suite_id, price_online = price) %>% 
    inner_join(tbl(Laputa, "suites") %>% 
                 select(suite_id = id, xiaoqu_id),
               by = "suite_id") %>%
    inner_join(tbl(Laputa, "xiaoqus") %>% 
                 select(xiaoqu_id = id, city, block, community = name),
               by = "xiaoqu_id") %>% 
    left_join(tbl(Laputa, "contract_with_landlords") %>% 
                # 房东端合同租期有可能不足一年或者6个月，但房东端合同有可能提前续约，因此只要不是执行结束的合同即可
                filter(status == '已签约' | stage == '执行中') %>%
                filter(stage != "执行结束") %>% 
                filter(sign_date <= CURDATE()) %>% 
                filter(end_date >= CURDATE()) %>% 
                select(suite_id, contract_id = id),
              by = "suite_id") %>% 
    collect() %>% 
    distinct() %>% 
    mutate(price_online = as.integer(price_online)) %>% 
    left_join(data.frame(code = code_input,
                         label = "input",
                         stringsAsFactors = FALSE), by = "code")
  
  lapply(DBI::dbListConnections(RMySQL::MySQL()), DBI::dbDisconnect)
  
  rooms
}



# api parameters prepare --------------------------------------------------

api_paras_prepare <- function(dat, include_cost){
  
  api_paras <- dat %>% 
    mutate_at(vars(contains("id")), as.numeric) %>% 
    mutate(include_cost = include_cost) %>% 
    select(suite_id, xiaoqu_id, contract_id, include_cost) %>%
    distinct()
  
  # api_paras <- apply(api_paras, MARGIN = 1, as.list)
  
  api_paras
  
}




# post request ------------------------------------------------------------

get_sublet_price <- function(dat, params){
  
  # 市场价价格获取接口地址
  safe_res <- get_api_safely(url = params$endpoint,
                             paras = data.frame(token = params$token, dat))
  
  if(is.null(safe_res$result) & is.null(safe_res$error)){
    
    res <- data.frame(status = 0,
                      remark = "Pricing API Error",
                      suite_id = dat$suite_id,
                      stringsAsFactors = FALSE)
    
  }else if(is.null(safe_res$result) & !is.null(safe_res$error)){
    
    res <- data.frame(status = 0,
                      remark = safe_res$error %>% as.character(),
                      suite_id = dat$suite_id,
                      stringsAsFactors = FALSE)
    
  }else{
    
    res <- safe_res$result
    
    if(res$status[1] == 0){
      
      res <- data.frame(status = 0,
                        remark = res$remark,
                        suite_id = dat$suite_id,
                        stringsAsFactors = FALSE)
    }
  }
  
  res
  
}

