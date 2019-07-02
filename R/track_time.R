#' Track time duration
#'
#' Calculate time duration based on sequential difference of date-time input.
#' The unit of time duration is seconds.
#'
#' By convention the first value is set to `NA` missing value, because the
#' difference applies to each sequential pair of locations.
#'
#' To use this on multiple track ids, use a grouped data frame with tidyverse
#' code like `data %>% group_by(id) %>% mutate(duration = track_time(date))`
#' @param date date-time in POSIXct
#'
#' @return numeric vector of duration between sequential date-time values in
#'   seconds, see Details
#' @export
#'
#' @examples
#' track_time(trips0$date)[1:10]
track_time<- function(date) {
  cls <- class(date)
  if (!inherits(date, "POSIXct")) {
    date <- try(as.POSIXct(date), silent = TRUE)
    if (inherits(date, "try-error")) {

      stop(sprintf("Cannot convert 'date' of class '%s' to POSIXct", paste(cls, collapse = ",")))
    }
  }
  c(NA_real_, diff(unclass(date)))
}
