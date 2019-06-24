#' Track bearing to location/s
#'
#' Calculate geodesic bearing to a location or locations based on longitude, latitude (from)
#' input vectors and longitude, latitude (to) input vectors. The *to* values may be a
#' single value or individual to each *from* location.
#'
#' No missing values are required as padding, but input data with `NA`s will incur an
#' `NA` in the output.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse code like
#' `data %>% group_by(id) %>% mutate(bearing_to = track_bearing_to(lon, lat, to_lon, to_lat))`.
#'
#' Absolute bearing is relative to North (0), and proceeds clockwise positive and anti-clockwise
#' negative `N = 0, E = 90, S = +/-180, W = -90`.
#' @param x longitude
#' @param y latitude
#' @param to_x longitude vector of *to* location/s
#' @param to_y latitude vector of *to* locations/s
#' @export
#'
#' @examples
#' track_bearing_to(trips0$x, trips0$y, to_x = 147, to_y = -42)[1:10]
#' # N E S W
#' track_bearing_to(0,0, c(0, 10, 0, -10), c(5, 0, -5, 0))
#'
#' # maximum and minimum value are the same direction (due south)
#' track_bearing(c(0, -0.00001), c(0, -1))
#' track_bearing(c(0,  0.00001), c(0, -1))
#'
#' # the absolute minimum is north
#' track_bearing(c(0, 0), c(0, 1))
track_bearing_to <- function(x, y, to_x, to_y){
  xy <- cbind(x, y)
  to_xy <- cbind(to_x, to_y)
  if (nrow(xy) > nrow(to_xy) && nrow(to_xy) == 1L) {
    to_xy <- to_xy[rep(1L, nrow(xy)), ]
  }
  geosphere::bearing(xy, to_xy)
}
