
#' Track distance
#'
#' Calculate geodesic distance on longitude, latitude input vectors.
#'
#' By convention the first value is set to `NA` missing value, because the distance
#' applies to each sequential pair of locations.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse code like
#' `data %>% group_by(id) %>% mutate(distance = track_distance(lon, lat))`
#' @param x longitude
#' @param y latitude
#'
#' @return distance in metres
#' @export
#'
#' @examples
#' track_distance(trips0$x, trips0$y)[1:10]
track_distance <- function(x, y) {
  c(NA_real_, geodist::geodist(cbind(x, y), sequential = TRUE, measure = "geodesic"))
}



