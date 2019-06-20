make_fake <- function(ngroup = 1, lon = 120, lat = -40, time = 0, plot = TRUE) {
  ## Brownian motion tethered at each end
  brownian.bridge <- function(n, r) {
    x <- cumsum(rnorm(n, 0, 1))
    x <- x - (x[1] + seq(0, 1, length=n) * (x[n] - x[1]))
    r * x
  }

  d <- purrr::map_dfr(seq_len(ngroup), ~{
    ## Number of days and number of obs
    days <- 80
    n <- 500
    ## Make separation between obs gamma distributed
    x <- rgamma(n, 3)
    x <- cumsum(x)
    x <- x/x[n]
    ## Track is lissajous + brownian bridge
    b.scale <- 0.6
    r.scale <- sample(c(0.1, 2, 10.2), n, replace=TRUE,
                      prob=c(0.8, 0.18, 0.02))
    set.seed(26 + .x -1)

    lon <- (lon + -5 * .x) + 20 * sin(2 * pi * x) +
      brownian.bridge(n, b.scale) + rnorm(n, 0, r.scale)
    lat <- (lat + -3 * .x) + 10 *(sin(3 * 2 * pi * x) + cos(2 * pi * x) - 1) +
      brownian.bridge(n, b.scale) + rnorm(n, 0, r.scale)
    tms <- ISOdate(2001, 1, 1) + trunc(days * 24 * 60 * 60 *x) + time * 3600 * 24 * 7 *  sign(rnorm(1)) * .x

    tibble::tibble(
      x =  lon,
      y =  lat,
      date = tms)


  }, .id = "id")
  if (plot) {
    plot(d$x, d$y, asp = 1/cos(mean(d$y) * pi/180))
    lines(d$x, d$y)
    maps::map(add = TRUE)
  }
  d %>% arrange(id, date)
}

library(magrittr)
trips0 <- make_fake(3) %>% dplyr::select(x, y, date, id)

usethis::use_data(trips0)
