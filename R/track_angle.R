#' Track angle
#'
#' Calculate internal track angle on longitude, latitude input vectors.
#'
#' By convention the first and last values are set to `NA` missing value, because the angle
#' applies to the location between each previous and next location.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse code like
#' `data %>% group_by(id) %>% mutate(angle = track_angle(lon, lat))`.
#'
#' @param x longitude
#' @param y latitude
#'
#' @return angle in degrees
#' @export
#' @examples
#' track_angle(trips0$x, trips0$y)[1:10]
track_angle <- function(x, y) {
  xy <- cbind(x, y)
  n <- nrow(xy)
  # angle <- .abdali(xy[2:(n - 1), , drop = FALSE], xy[3:n, , drop = FALSE]) -
  #          .abdali(xy[2:(n - 1), , drop = FALSE], xy[1:(n - 2), , drop = FALSE])
  angle <- geosphere::bearing(xy[2:(n - 1), , drop = FALSE], xy[3:n, , drop = FALSE]) -
           geosphere::bearing(xy[2:(n - 1), , drop = FALSE], xy[1:(n - 2), , drop = FALSE])
  angle <- abs((angle + 180) %% 360 - 180)
  c(NA_real_, angle, NA_real_)
}
