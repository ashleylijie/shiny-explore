source("R/header.R")
source("R/config.R")
source("R/trans_coordinate.R")
options(digits=10)

city_location <- read_rds("Data/city_location.rds") %>% 
    mutate(longitude = bd_to_wgs84(longitude, latitude)$lng,
           latitude = bd_to_wgs84(longitude, latitude)$lat,
           center_longitude = bd_to_wgs84(center_longitude, center_latitude)$lng,
           center_latitude = bd_to_wgs84(center_longitude, center_latitude)$lat) %>% 
    group_by(city) %>% 
    mutate(max_lon = max(longitude),
           min_lon = min(longitude),
           max_lat = max(latitude),
           min_lat = min(latitude)) %>% 
    select(city, matches("max|min|center")) %>% 
    distinct()

city_list <- c("北京市", "上海市", "深圳市", "杭州市", "天津市",
               "武汉市", "南京市", "广州市", "成都市", "苏州市")




get_xiaoqus <- function(input_city){
 
    lapply(DBI::dbListConnections(RMySQL::MySQL()), DBI::dbDisconnect)
    Laputa <- est_mysql_conn(db$Laputa)
    DW <- est_mysql_conn(db$DW)
    Monitor <- est_mysql_conn(db$Monitor)
    Forecast <- est_mysql_conn(db$Forecast)
    DW_Temp <- est_mysql_conn(db$DW_Temp)
    Fico <- est_mysql_conn(db$Fico)
    lapply(DBI::dbListConnections(RMySQL::MySQL()), function(x) DBI::dbSendQuery(x, statement = "SET NAMES utf8"))
    
    
    xiaoqus <- tbl(Laputa, "xiaoqus") %>% 
        filter(city == input_city) %>% 
        select(city, district, block, community = name, xiaoqu_id = id, longitude, latitude, subway_distance) %>% 
        inner_join(tbl(Laputa, "areas") %>% 
                       filter(level == "城市") %>% 
                       select(city_id = id, city = name) ,
                   by = "city") %>% 
        inner_join(tbl(Laputa, "areas") %>% 
                       filter(level == "行政区") %>% 
                       select(district_id = id, district = name, city_id = "parent_id"),
                   by = c("city_id", "district")) %>% 
        inner_join(tbl(Laputa, "areas") %>% 
                       filter(level == "商圈") %>% 
                       select(block_id = id, block = name, district_id = "parent_id", 
                              block_longitude = longitude, block_latitude = latitude),
                   by = c("district_id", "block")) %>% 
        distinct() %>% 
        collect() %>% 
        left_join(tbl(Forecast, "community_std_price") %>% 
                      select(xiaoqu_id, xiaoqu_std_price, 
                             block_std_price, xiaoqu_std_price_type) %>% 
                      distinct() %>% 
                      collect(),
                  by = "xiaoqu_id") %>% 
        left_join(tbl(DW_Temp, "community_block_distance") %>% 
                      select(xiaoqu_id, distance.value = subway_distance) %>% 
                      distinct() %>%
                      collect(),
                  by = "xiaoqu_id") %>% 
        mutate(subway_distance = ifelse(is.na(distance.value), subway_distance, distance.value) %>% as.numeric()) %>%
        select(-distance.value) %>% 
        filter(xiaoqu_std_price > 500) %>% 
        filter_all(all_vars(!is.na(.))) %>% 
        distinct() %>% 
        mutate(longitude = bd_to_wgs84(longitude, latitude)$lng,
               latitude = bd_to_wgs84(longitude, latitude)$lat,
               block_longitude = bd_to_wgs84(block_longitude, block_latitude)$lng,
               block_latitude = bd_to_wgs84(block_longitude, block_latitude)$lat) %>% 
        inner_join(city_location, by = 'city') %>%
        group_by(city) %>% 
        filter(longitude <= max_lon, longitude >= min_lon, latitude <= max_lat, latitude >= min_lat) %>% 
        ungroup() %>% 
        mutate(xiaoqu_std_price_cut = floor(xiaoqu_std_price / 500) * 500)
    
    xiaoqus
}


