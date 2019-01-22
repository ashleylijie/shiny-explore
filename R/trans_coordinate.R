# 初始参数设置
pi <-  3.1415926535897932384626
x_pi <-  3.14159265358979324 * 3000 / 180
ee <- 0.00669342162296594323
cbz <- 6378245.0


transformlat <- function(lng, lat){
  ret <-  -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat^2 + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
  ret  <- ret + (20.0 * sin(6.0 * lng * pi) + 20.0 *sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret <-  ret + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) * 2.0 / 3.0
  ret
}

transformlng <- function(lng, lat){
  ret <-  300.0 + lng + 2.0 * lat + 0.1 * lng^2 + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
  ret <- ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(lng * pi) + 40.0 * sin(lng / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (150.0 * sin(lng / 12.0 * pi) + 300.0 * sin(lng / 30.0 * pi)) * 2.0 / 3.0
  ret
}

# baidu->火星坐标系
bd09_to_gcj02 <- function(bd_lon, bd_lat) {
  x <-  bd_lon - 0.0065
  y <-  bd_lat - 0.006
  sqrt(x ^ 2 + y ^ 2) - 0.00002 * sin(y * x_pi)
  z <-  sqrt(x ^ 2 + y ^ 2) - 0.00002 * sin(y * x_pi)
  theta <-  atan2(y, x) - 0.000003 * cos(x * x_pi)
  gg_lng <-  z * cos(theta)
  gg_lat <-  z * sin(theta)
  list(lng = gg_lng, lat = gg_lat)
}

# 火星坐标系->wgs1984
gcj02_to_wgs84 <- function(lng, lat){
  dlat <- transformlat(lng - 105.0, lat - 35.0)
  dlng <- transformlng(lng - 105.0, lat - 35.0)
  radlat <- lat / 180.0 * pi
  magic <- sin(radlat)
  magic <- 1 - ee * magic * magic
  sqrtmagic <- sqrt(magic)
  dlat <- (dlat * 180.0) / ((cbz * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng <- (dlng * 180.0) / (cbz / sqrtmagic * cos(radlat) * pi)
  mglat <- lat + dlat
  mglng <- lng + dlng
  list(lng = lng * 2 - mglng, lat = lat * 2 - mglat)
}

# baidu->wgs1984
bd_to_wgs84 <- function(lng_bd, lat_bd) {
  res <- bd09_to_gcj02(lng_bd, lat_bd)
  gcj02_to_wgs84(res$lng, res$lat)
}