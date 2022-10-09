#' Query track data for arbitrary locations
#'
#' Latent positions may be queried using arbitrary date-time values. The only method
#' (for now) is 'linear', but default should be 'geodesic'. In time we include more methods
#' to match the GeoPandas implementation.
#'
#' If `date` is not included, time itself is treated as the obvious index on n-locations so
#' simple relative time, and `query` is expected to match this.
#'
#' We use group_modify to keep the id groups:
#' `trips0 %>% group_by(id) %>% group_modify(~track_query(.x$x, .x$y, query = c(4.5, 6.7)))`
#' @param x longitude
#' @param y latitude
#' @param date date-time in POSIXct (or can be ignore, for relative index-time)
#' @param query required argument, date-time values to return inferred x, y positions for
#' @param type linear, geodesic, rhumb, forward, backward, nearest (also need open/closed intervals)
#' @export
#' @importFrom stats approxfun
#' @return data frame of 'x,y,date' of inferred positions
#' @examples
#' track_query(trips0$x[1:10], trips0$y[1:10], query = c(4.5, 5.5, 6.5))
#' track_query(trips0$x[1:10], trips0$y[1:10], trips0$date[1:10], query = trips0$date[1:10] + 10)
#' s <- seq(min(trips0$date), max(trips0$date), by = "1 hour")
track_query <- function(x, y, date = NULL, query, type = "linear") {
  type <- match.arg(type)  ## only linear for now
  if (is.null(date)) {
    warning("date is null, so assuming linear relative movement in time")
    date <- seq_along(x)
  }
  x_t <- approxfun(date, x)
  y_t <- approxfun(date, y)
  if (inherits(query, "Date")) query <- as.POSIXct(query)
  tibble::tibble(x = x_t(query),
                 y = y_t(query),
                 date = query)  ## here need to insert the rule-based date, not the query
}
