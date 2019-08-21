#' Track intermediate points
#'
#' Calculate great circle intermediate points on longitude, latitude input vectors. A
#' spherical model is used, from the geosphere package.
#'
#' This function returns a list of data frames, with a data frame of interpolated locations
#' for every interval between input locations. There is a final empty data frame to ensure
#' the list is the same length as the inputs. See embedded usage of the tidyr function 'unnest()'
#' for ease of use.
#'
#' To use on multiple track ids, use a grouped data frame with tidyverse code like
#' `inter <- data %>% group_by(id) %>%
#'                    mutate(inter = track_intermediate(lon, lat, date = , distance = )`.
#'
#'
#' Then, un-nest this result for further use (the 'inter' above retains the information
#' about the parent locations for custom usage if needed), so the final location of each
#' group has invalid intermediates:
#' `dd <- inter %>% slice(-1) %>% unnest()`
#' @param x longitude
#' @param y latitude
#' @param date optional input date-time in POSIXct
#' @param distance optional minimum distance between input points
#' @return a list of data frames of intermediate points (for use with `unnest()` from tidyr)
#' @export
#'
#' @examples
#' track_intermediate(trips0$x[1:10], trips0$y[1:10], distance = 15000)
#'
#' track_intermediate(trips0$x[1:10], trips0$y[1:10], date = trips0$date, distance = 1500)
#'
#' \dontrun{
#' ## run with full workflow to expand into a new data frame with
#' ## `int_x`, `int_y`, and (optional) `int_date`
#'  if (requireNamespace("tidyr") && requireNamespace("dplyr")) {
#'    dd <- trips0 %>% group_by(id) %>%
#'      mutate(inter = track_intermediate(x, y, date = date, distance = 50000)) %>%
#'      tidyr::unnest()
#'    plot(dd$int_date, dd$int_y)
#'    abline(v = trips0$date)
#'  }
#' }
track_intermediate <- function(x, y, date = NULL, distance = NULL) {
  n <- length(x)
  if (is.null(distance)) {
    npoints <- rep(15, n - 1)
  } else {
    npoints <- pmax(3, ceiling(track_distance(x, y) / distance))[-1]
  }
  listm <- geosphere::gcIntermediate(cbind(x[-n], y[-n]), cbind(x[-1], y[-1]),
                                     n = npoints, addStartEnd = TRUE, sp = FALSE)

  if (n == 2)   listm <- list(listm)
 listm <- lapply(listm, as.data.frame)
 npoints <- npoints + 2 ## because addStartEnd = TRUE
 funa <- function(a) data.frame(int_x = a[["lon"]], int_y = a[["lat"]],
                                int_date = a[["int_date"]])
 runfun <- function(a) data.frame(int_x = a[["lon"]], int_y = a[["lat"]])



  if (!is.null(date)) {
    runfun <- funa
    for (i in seq_along(listm)) {

      listm[[i]]$int_date <- seq(date[i], date[i +1], length.out = npoints[i])
    }
  }


 c(lapply(listm, runfun), list(data.frame()))
}
