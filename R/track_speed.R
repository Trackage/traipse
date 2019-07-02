#' Track speed
#'
#' Calculate speed (m/s) based on geodesic distance with longitude, latitude, date-time
#' input vectors. The unit of speed is metres per second.
#'
#' By convention the first value is set to `NA` missing value, because the difference
#' applies to each sequential pair of locations.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse code like
#' `data %>% group_by(id) %>% mutate(speed = track_speed(lon, lat, date))`
#' @param x longitude
#' @param y latitude
#' @param date date-time in POSIXct
#'
#' @return numeric vector of sequential distances in metres per second, see Details
#' @export
#'
#' @examples
#' track_speed(trips0$x, trips0$y, trips0$date)[1:10]
track_speed <- function(x, y, date) {
  track_distance(x, y)/track_time(date)
}
