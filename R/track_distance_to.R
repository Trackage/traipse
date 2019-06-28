#' Track distance to location/s
#'
#' Calculate geodesic distance to a location or locations based on longitude, latitude (from)
#' input vectors and longitude, latitude (to) input vectors. The *to* values may be a
#' single value or individual to each *from* location.
#'
#' No missing values are required as padding, but input data with `NA`s will incur an
#' `NA` in the output.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse code like
#' `data %>% group_by(id) %>% mutate(distance = track_distance_to(lon, lat, to_lon, to_lat))`
#'
#' @param x longitude
#' @param y latitude
#' @param to_x longitude vector of *to* location/s
#' @param to_y latitude vector of *to* locations/s
#' @export
#' @return a numeric vector of distance-to values in metres
#' @examples
#' track_distance_to(trips0$x, trips0$y, to_x = 147, to_y = -42)[1:10]
track_distance_to <- function(x, y, to_x, to_y){
  xy <- cbind(x, y)
  to_xy <- cbind(to_x, to_y)
  if (nrow(xy) > nrow(to_xy) && nrow(to_xy) == 1L) {
    to_xy <- to_xy[rep(1L, nrow(xy)), ]
  }
  geodist::geodist(xy, to_xy, paired = TRUE, measure = "geodesic")
}
