library(jsonlite)

beijing <- fromJSON("location/beijing.json") %>% `[[`(2)

temp <- beijing$geometry$coordinates %>% `[[`(1)

data.frame(lon = temp[1:(length(temp)/2)], lat = temp[(length(temp)/2 + 1):length(temp)])

all_json <- lapply(paste0("location/", list.files("location/")), function(x) fromJSON(x) %>% `[[`(2))

names(all_json) <- list.files("location/") %>% gsub(pattern = "*\\.json$", replacement = "")

all_json$beijing$properties

all_json$beijing$geometry$coordinates

all_json$china$properties[["name"]]

all_json$china$geometry$coordinates[30]

grep(all_json$china$properties[["name"]], pattern = "北京市|天津市|上海市")
grep(all_json$guangdong$properties[["name"]], pattern = "广州市|深圳市")
grep(all_json$hubei$properties[["name"]], pattern = "武汉市")
grep(all_json$jiangsu$properties[["name"]], pattern = "苏州市|南京市")
grep(all_json$sichuan$properties[["name"]], pattern = "成都市")
grep(all_json$zhejiang$properties[["name"]], pattern = "杭州市")

location <- NULL
location$北京市 <- all_json$china$geometry$coordinates[30]
location$天津市 <- all_json$china$geometry$coordinates[31]
location$上海市 <- all_json$china$geometry$coordinates[32]
location$广州市 <- all_json$guangdong$geometry$coordinates[12]
location$深圳市 <- all_json$guangdong$geometry$coordinates[19]
location$武汉市 <- all_json$hubei$geometry$coordinates[11]
location$苏州市 <- all_json$jiangsu$geometry$coordinates[5]
location$南京市 <- all_json$jiangsu$geometry$coordinates[9]
location$成都市 <- all_json$sichuan$geometry$coordinates[13]
location$杭州市 <- all_json$zhejiang$geometry$coordinates[2]

result <- NULL
for(i in 1:length(location)){
    
    temp <- location[[i]] %>% `[[`(1)
    
    result <- rbind(result,
                    data.frame(city = names(location[i]), 
                               longitude = temp[1:(length(temp)/2)], 
                               latitude = temp[(length(temp)/2 + 1):length(temp)]))
}
