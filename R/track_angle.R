#' Track angle
#'
#' Calculate internal track angle on longitude, latitude input vectors. The unit of angle is degrees.
#'
#' By convention the first and last values are set to `NA` missing value, because the angle
#' applies to the location between each previous and next location.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse code like
#' `data %>% group_by(id) %>% mutate(angle = track_angle(lon, lat))`.
#'
#' The maximum possible value is 180 and the minimum is 0.
#'
#' @param x longitude
#' @param y latitude
#'
#' @return a numeric vector of the relative internal angle between sequential locations
#' in degrees, see Details
#' @export
#' @examples
#' track_angle(trips0$x, trips0$y)[1:10]
#'
#' ## maximum value
#' track_angle(c(0, 0, 0), c(0, 1, 2))
#' ## minimum value
#' track_angle(c(0, 0, 0), c(0, 1, 0))
track_angle <- function(x, y) {
  xy <- cbind(x, y)
  n <- nrow(xy)
  if (n < 3L) return(rep(NA_real_, n))
  # angle <- .abdali(xy[2:(n - 1), , drop = FALSE], xy[3:n, , drop = FALSE]) -
  #          .abdali(xy[2:(n - 1), , drop = FALSE], xy[1:(n - 2), , drop = FALSE])
  angle <- geographiclib::geodesic_inverse(xy[2:(n - 1), , drop = FALSE], xy[3:n, , drop = FALSE])[["azi1"]] -
    geographiclib::geodesic_inverse(xy[2:(n - 1), , drop = FALSE], xy[1:(n - 2), , drop = FALSE])[["azi1"]]
  angle <- abs((angle + 180) %% 360 - 180)
  c(NA_real_, angle, NA_real_)
}
