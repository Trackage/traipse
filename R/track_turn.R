#' Track turn angle
#'
#' Calculate relative track turning angle on longitude, latitude input vectors.
#'
#' By convention the last value is set to `NA` missing value, because the angle
#' applies to the relative turn from the current location.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse code like
#' `data %>% group_by(id) %>% mutate(turn = track_turn(lon, lat))`.
#' @param x longitude
#' @param y latitude
#'
#' @export
#' @examples
#' track_turn(trips0$x, trips0$y)[1:10]
track_turn <- function(x, y) {
  xy <- cbind(x, y)
  n <- nrow(xy)
  angle <- c(geosphere::bearing(xy[-nrow(xy), , drop = FALSE],
                                xy[-1L, , drop = FALSE]) * pi/180, NA_real_)
  angle <- ifelse(angle < 0, 2 * pi + angle, angle)

  angle <- ifelse(angle > pi, (2 * pi - angle) * -1, angle)
  p <- c(NA_real_, diff(angle)) %% (2 * pi)
  ifelse(p > pi, p - 2 * pi, p) * 180/pi
}

