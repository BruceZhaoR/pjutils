#' Coordinate transform
#'
#' WGS84 to GCJ02, GCJ02 to BD09 and vice versa.
#' WG84 is used by Google maps and hardware,
#' GCJ02 is used by Gaode and Tencent,
#' BD09 is used by Baidu.
#'
#' @param wgs_lon WGS84 longitude
#' @param wgs_lat WGS84 latitude
#' @param gcj_lon GCJ02 longitude, gaode/tencent
#' @param gcj_lat GCJ02 latitude, gaode/tencent
#' @param bd_lon BD09 longitude, baidu map
#' @param bd_lat BD09 latitude, baidu map
#' @return Transformed vector of  longitude and latitude.
#' @name coordinate_transform
#' @examples
#' test <-
#'   structure(
#'     list(
#'       startLon = c(
#'         113.58850000000001,
#'         113.54700000000003,
#'         113.54050000000001,
#'         113.52866660000001,
#'         113.53083329999998,
#'         113.54083329999997
#'       ),
#'       startLat = c(
#'         33.43299999999999,
#'         33.466666599999996,
#'         33.49250000000001,
#'         33.4581666,
#'         33.45766660000001,
#'         33.49250000000001
#'       ),
#'       endLon = c(
#'         113.5468333,
#'         113.54050000000001,
#'         113.52866660000001,
#'         113.53083329999998,
#'         113.54083329999997,
#'         113.52249999999998
#'       ),
#'       endLat = c(
#'         33.465,
#'         33.49250000000001,
#'         33.4581666,
#'         33.457166599999994,
#'         33.49233330000001,
#'         33.49250000000001
#'       )
#'     ),
#'     row.names = c(NA, -6L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   )
#'
#' wgs_lon <- test$startLon[[1]]
#' wgs_lat <- test$startLat[[1]]
#' wgs2gcj(wgs_lon, wgs_lat)# gaode
#' wgs2bd(wgs_lon, wgs_lat)#baidu
#'
#' t(apply(test, 1, function(x) {
#'   start_gps <- wgs2bd(x[1], x[2])
#'   end_gps <- wgs2bd(x[3], x[4])
#'   c(start_gps, end_gps)
#' }))
#'
NULL


# long semidiameter
a <- 6378245.0
# oblateness
ee <- 0.00669342162296594323

# Check whether the coordinate in China.
out_of_china <- function(lon, lat) {
  if(lon < 72.004 || lon > 137.8347) {
    return(TRUE)
  } else if(lat < 0.8293 || lat > 55.8271) {
    return(TRUE)
  } else {
    FALSE
  }
}


transform_lat <- function(lon, lat) {
  ret <- -100.0 + 2.0 * lon + 3.0 * lat + 0.2 * lat * lat + 0.1 * lon * lat + 0.2 * sqrt(abs(lon))
  ret <- ret + (20.0 * sin(6.0 * lon * pi) + 20.0 * sin(2.0 * lon * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) * 2.0 / 3.0
  ret
}

transform_lon <- function(lon, lat) {
  ret <- 300.0 + lon + 2.0 * lat + 0.1 * lon * lon + 0.1 * lon * lat + 0.1 * sqrt(abs(lon))
  ret <- ret + (20.0 * sin(6.0 * lon * pi) + 20.0 * sin(2.0 * lon * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(lon * pi) + 40.0 * sin(lon / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (150.0 * sin(lon / 12.0 * pi) + 300.0 * sin(lon / 30.0 * pi)) * 2.0 / 3.0
  ret
}


#' @export
#' @rdname coordinate_transform
wgs2gcj <- function(wgs_lon, wgs_lat) {
  if(out_of_china(wgs_lon, wgs_lat)){
    warning("gps out of China, set to 0.",call. = FALSE)
    return(c(0.0, 0.0))
  }
  dlat <- transform_lat(wgs_lon - 105.0, wgs_lat - 35.0)
  dlon <- transform_lon(wgs_lon - 105.0, wgs_lat - 35.0)
  rad_lat <- wgs_lat / 180.0 * pi
  magic <- 1 - ee * (sin(rad_lat))^2
  dlat <- (dlat * 180.0) / ((a * (1-ee)) / (magic * sqrt(magic)) * pi)
  dlon <- (dlon * 180.0) / (a / sqrt(magic) * cos(rad_lat) * pi)
  gd_lat <- wgs_lat + dlat
  gd_lon <- wgs_lon + dlon
  c(gd_lon, gd_lat)
}



#' @export
#' @rdname coordinate_transform
gcj2wgs <- function(gcj_lon, gcj_lat) {
  if(out_of_china(gcj_lon, gcj_lat)){
    warning("gps out of China, set to 0.",call. = FALSE)
    return(c(0.0, 0.0))
  }
  dlat <- transform_lat(gcj_lon - 105.0, gcj_lat - 35.0)
  dlon <- transform_lon(gcj_lon - 105.0, gcj_lat - 35.0)
  rad_lat <- gcj_lat/ 180.0 * pi
  magic <- 1 - ee * (sin(rad_lat))^2
  dlat <- (dlat * 180.0) / ((a * (1- ee)) / (magic * sqrt(magic)) * pi)
  dlon <- (dlon * 180.0) / (a / sqrt(magic) * cos(rad_lat) * pi)
  wgs_lat <- gcj_lat * 2 - (gcj_lat + dlat)
  wgs_lon <- gcj_lon * 2 - (gcj_lon + dlon)
  c(wgs_lon, wgs_lat)
}


#' @export
#' @rdname coordinate_transform
gcj2bd <- function(gcj_lon, gcj_lat) {
  if(out_of_china(gcj_lon, gcj_lat)){
    warning("gps out of China, set to 0.",call. = FALSE)
    return(c(0.0, 0.0))
  }
  x_pi <- pi * 3000.0 / 180.0
  z <- sqrt(gcj_lon^2 + gcj_lat^2) + 0.00002 * sin(gcj_lat * x_pi)
  theta <- atan2(gcj_lat, gcj_lon) + 0.000003 * cos(gcj_lon * x_pi)
  bd_lon <- z * cos(theta) + 0.0065
  bd_lat <- z * sin(theta) + 0.006
  c(bd_lon, bd_lat)
}


#' @export
#' @rdname coordinate_transform
bd2gcj <- function(bd_lon, bd_lat) {
  if(out_of_china(bd_lon, bd_lat)){
    warning("gps out of China, set to 0.",call. = FALSE)
    return(c(0.0, 0.0))
  }
  x_pi <- pi * 3000.0 / 180.0
  x <- bd_lon - 0.0065
  y <- bd_lat - 0.006
  z <- sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
  theta <- atan2(y, x) - 0.000003 * cos(x * x_pi)
  gd_lon <- z * cos(theta)
  gd_lat <- z * sin(theta)
  c(gd_lon, gd_lat)
}


#' @export
#' @rdname coordinate_transform
wgs2bd <- function(wgs_lon, wgs_lat){
  gcj <- wgs2gcj(wgs_lon,wgs_lat)
  bd <- gcj2bd(gcj[[1]],gcj[[2]])
  bd_lon <- bd[[1]]
  bd_lat <- bd[[2]]
  c(bd_lon, bd_lat)
}


#' @export
#' @rdname coordinate_transform
bd2wgs <- function(bd_lon, bd_lat){
  gcj <- bd2gcj(bd_lon, bd_lat)
  wgs <- gcj2wgs(gcj[[1]], gcj[[2]])
  wgs_lon <- wgs[[1]]
  wgs_lat <- wgs[[2]]
  c(wgs_lon, wgs_lat)
}

#' Generate GPS lines data for echarts
#'
#' Your can see the package/inst/misc files.
#'
#' @param df data.frame with starLon, starLat, endLon, endLat.
#'
#' @return json txt
#' @export
#'
#' @examples
#' \dontrun{
#' gps_trans <- function(df) {
#'   test <- select(df, startLon, startLat, endLon, endLat)
#'   as_tibble(t(apply(test, 1, function(x) {
#'     x <- as.numeric(x)
#'     start_gps <- wgs2bd(x[1], x[2])
#'     end_gps <- wgs2bd(x[3], x[4])
#'     c(start_gps[1], start_gps[2], end_gps[1], end_gps[2])
#'   })))
#' }
#' usr_data <- purrr::map(usr_ids, ~ filter(pam_data, userId == .x))
#' all <- map(lapply(usr_data, gps_trans), ~ fmt_gps_json(.x))
#'
#'}
fmt_gps_json <- function(df) {
  list_mat <- df %>%
    purrr::pmap( ~ matrix(c(..1, ..2, ..3, ..4), byrow = TRUE, ncol = 2))
  tibble::tibble(coords = list_mat) %>%
    jsonlite::toJSON()
}
