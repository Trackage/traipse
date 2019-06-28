#' Track bearing
#'
#' Calculate sequential bearing on longitude, latitude input vectors.
#'
#' By convention the last value is set to `NA` missing value, because the bearing
#' applies to the segment extending from the current location.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse code like
#' `data %>% group_by(id) %>% mutate(turn = track_bearing(lon, lat))`.
#'
#' Absolute bearing is relative to North (0), and proceeds clockwise positive and anti-clockwise
#' negative `N = 0, E = 90, S = +/-180, W = -90`.
#'
#' The last value will be `NA` as the bearing is relative to the first point of each segment.
#' @param x longitude
#' @param y latitude
#' @return a numeric vector of absolute bearing in degrees, see Details
#' @export
#' @examples
#' track_bearing(trips0$x, trips0$y)[1:10]
track_bearing <- function(x, y) {
  xy <- cbind(x, y)
  n <- nrow(xy)
  c(geosphere::bearing(xy[-nrow(xy), , drop = FALSE],
                       xy[-1L, , drop = FALSE]), NA_real_)

}
