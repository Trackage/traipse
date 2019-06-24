d <- tibble::tibble(x = c(0, 20, 50, 80, 80, 50, 170, -170, -100, -100),
                    y = c(0, 10, 20, 30, -30, -30, -65, 0, 10, 20), id = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2))

d <- trips0
idx <- 1:3

local_proj <- function(x) {
  sprintf("+proj=laea +lon_=%f +lat_0=%f +datum=WGS84", x$x[2], x$y[2])
}

par(xpd = NA)
data(wrld_simpl, package = "maptools")
m <- wrld_simpl
for (ii in seq(0, nrow(d) - 2)) {
  xx <- d[idx + ii, c("x", "y")]
  lproj <- local_proj(xx)
  xy <- rgdal::project(cbind(xx$x, xx$y), lproj)

 plot(extent(xy) + 3e5, asp= 1)
 lines(xy)
 text(xy, lab = 1:3, pos = 4)
 text(xy[2, , drop = FALSE], lab = round(track_angle(xx$x, xx$y)[2]), pos = 2, col = "firebrick")
 plot(spTransform(m, lproj), add = TRUE)
}




for (ii in seq(0, nrow(d) - 2)) {
  xx <- d[idx + ii, c("x", "y")]
  lproj <- local_proj(xx)
  xy <- rgdal::project(cbind(xx$x, xx$y), lproj)
  grat <- graticule::graticule(pretty(range(xx$x)), pretty(range(xx$y)), proj = lproj)
  plot(extent(xy) + 3e5, asp= 1)
  lines(xy)
  text(xy, lab = 1:3, pos = 4)
  text(xy, lab = round(track_bearing(xx$x, xx$y)), pos = 2, col = "firebrick")
#  plot(spTransform(m, lproj), add = TRUE)
  plot(grat, add = TRUE)
}

i <- 1; j <- i + 1
i <- i + 1;j<- j + 1
maptools::gzAzimuth(cbind(trips0$x[i], trips0$y[i]), cbind(trips0$x[j], trips0$y[j]))
track_bearing(trips0$x[i:j], trips0$y[i:j])
track_distance(trips0$x[i:j], trips0$y[i:j])[2]
track_distance_to(trips0$x[i], trips0$y[i],
                  trips0$x[j], trips0$y[j])

