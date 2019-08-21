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
#' @param distance optional minimum distance (metres) between interpolated points
#' @param duration optional minimum duration (seconds) between interpolated point,
#'  if set then `distance` must be `NULL` and `date` must be input
#' @return a list of data frames of intermediate points (for use with `unnest()` from tidyr)
#' @export
#'
#' @examples
#' track_intermediate(trips0$x[1:10], trips0$y[1:10], distance = 15000)
#'
#' track_intermediate(trips0$x[1:10], trips0$y[1:10], date = trips0$date,
#'                                  distance = 1500)
#'
#' inter_time <- track_intermediate(trips0$x[1:10], trips0$y[1:10],
#'                             date = trips0$date, duration = 1800)
#' \dontrun{
#' ## run with full workflow to expand into a new data frame with
#' ## `int_x`, `int_y`, and (optional) `int_date`
#'  if (requireNamespace("tidyr") && requireNamespace("dplyr")) {
#'  tr1 <- trips0[seq(1, nrow(trips0), by = 30), ]
#'    dd <- tr1 %>% group_by(id) %>%
#'      mutate(inter = track_intermediate(x, y, date = date, distance = 150000)) %>%
#'      tidyr::unnest()
#'    plot(dd$int_date, dd$int_y, pch = ".", cex = 2, main = "equidistant in space")
#'    abline(v = tr1$date)
#'
#'    dd1 <- tr1 %>% group_by(id) %>%
#'      mutate(inter = track_intermediate(x, y, date = date, duration = 3600 * 12)) %>%
#'      tidyr::unnest()
#'    plot(dd1$int_date, dd1$int_y, pch = ".", cex = 2, main = "equispaced in time")
#'    abline(v = tr1$date)
#'  }
#' }
track_intermediate <- function(x, y, date = NULL, distance = NULL, duration = NULL) {
  n <- length(x)
  if (!is.null(distance) && !is.null(duration)) stop("'distance' or 'duration' (or both) must be NULL")
  if (is.null(distance)) {
    npoints <- rep(15, n - 1)
  } else {
    npoints <- pmax(3, ceiling(track_distance(x, y) / distance))[-1L]
  }

  if (!is.null(duration)) {
    if (is.null(date)) stop("if 'duration' is not NULL, 'date' must also be given/n")
    npoints <- pmax(3, ceiling(track_time(date) / duration))[-1L]
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
