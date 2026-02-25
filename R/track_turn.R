#' Track turn angle
#'
#' Calculate relative track turning angle on longitude, latitude input vectors.
#' The unit of turn angle is degrees.
#'
#' By convention the last value is set to `NA` missing value, because the angle
#' applies to the relative turn from the current location.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse
#' code like `data %>% group_by(id) %>% mutate(turn = track_turn(lon, lat))`.
#'
#' The maximum possible value is 180 degrees and the minimum is -180, although
#' these particular values are a special case and will probably always be
#' positive. Turn angle is a signed quantity with negative values for a left
#' turn and positive values for a right turn.
#' @param x longitude
#' @param y latitude
#' @return a numeric vector of absolute turn angles, in degrees
#' @export
#' @examples
#' track_turn(trips0$x, trips0$y)[1:10]
#'
#' ## maximum turn angle
#' track_turn(c(0, 0, 0), c(0, 1, 0))
#' ## minimum turn angle
#' track_turn(c(0, 0, 0), c(0, 1, 2))
track_turn <- function(x, y) {

  xy <- cbind(x, y)
  n <- nrow(xy)
  if (n < 3L) return(rep(NA_real_, n))

  angle <- c(geographiclib::geodesic_inverse(xy[-nrow(xy), , drop = FALSE],
                                xy[-1L, , drop = FALSE])[["azi1"]] * pi/180, NA_real_)
  angle <- ifelse(angle < 0, 2 * pi + angle, angle)

  angle <- ifelse(angle > pi, (2 * pi - angle) * -1, angle)
  p <- c(NA_real_, diff(angle)) %% (2 * pi)
  ifelse(p > pi, p - 2 * pi, p) * 180/pi
}

