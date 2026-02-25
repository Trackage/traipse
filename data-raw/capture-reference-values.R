## capture-reference-values.R
##
## Run this ONCE against current traipse (geosphere-backed) to capture
## reference values. Output is copy-pasteable R code for the test file.
##
## Usage:
##   source("data-raw/capture-reference-values.R")
##
## The output goes to "reference-values.R" which you can inspect and
## paste into the test file.

library(traipse)

trips0_id1 <- trips0[trips0$id == "1", ]
n <- 20  # first 20 points for detailed reference

x <- trips0_id1$x[1:n]
y <- trips0_id1$y[1:n]
dt <- trips0_id1$date[1:n]

## ── Capture all geosphere-dependent outputs ──

b  <- track_bearing(x, y)
a  <- track_angle(x, y)
tu <- track_turn(x, y)
bt <- track_bearing_to(x, y, 147, -42)

# track_intermediate by distance
inter_d <- track_intermediate(x[1:5], y[1:5], date = dt[1:5], distance = 50000)
# track_intermediate by duration
inter_t <- track_intermediate(x[1:5], y[1:5], date = dt[1:5], duration = 3600)

## Also capture on tricky geometries
# antimeridian
ax <- c(170, 175, 179, -179, -175, -170, -165, -160)
ay <- c(-40, -41, -42, -43, -42, -41, -40, -39)
b_anti  <- track_bearing(ax, ay)
a_anti  <- track_angle(ax, ay)
tu_anti <- track_turn(ax, ay)

# polar
px <- seq(-180, 170, by = 10)
py <- rep(-75, length(px))
b_polar <- track_bearing(px, py)

# equatorial cardinal check
bt_card <- track_bearing_to(0, 0, c(0, 10, 0, -10), c(5, 0, -5, 0))

## ── Write out ──

sink("data-raw/reference-values.R")

cat("## Reference values captured from traipse", as.character(packageVersion("traipse")),
    "with geosphere", as.character(packageVersion("geosphere")), "\n")
cat("## Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n")

fmt <- function(x, name) {
  vals <- paste(sprintf("%.10f", x), collapse = ", ")
  # Replace NaN/NA properly
  vals <- gsub("NA", "NA_real_", vals)
  vals <- gsub("NaN", "NaN", vals)
  cat(name, "<- c(", vals, ")\n\n")
}

cat("# ── trips0 id1 first 20 points ──\n\n")
fmt(b, "ref_bearing")
fmt(a, "ref_angle")
fmt(tu, "ref_turn")
fmt(bt, "ref_bearing_to")

cat("# ── antimeridian track ──\n\n")
fmt(b_anti, "ref_bearing_anti")
fmt(a_anti, "ref_angle_anti")
fmt(tu_anti, "ref_turn_anti")

cat("# ── polar track (bearing only, latitude = -75) ──\n\n")
fmt(b_polar, "ref_bearing_polar")

cat("# ── cardinal bearing-to from origin ──\n\n")
fmt(bt_card, "ref_bearing_to_cardinal")

cat("# ── track_intermediate by distance (first interval) ──\n")
cat("# Interval 1: (x[1],y[1]) -> (x[2],y[2]), 50km spacing\n\n")
if (nrow(inter_d[[1]]) > 0) {
  fmt(inter_d[[1]]$int_x, "ref_inter_d_x")
  fmt(inter_d[[1]]$int_y, "ref_inter_d_y")
} else {
  cat("ref_inter_d_x <- numeric(0)\n")
  cat("ref_inter_d_y <- numeric(0)\n\n")
}
cat("ref_inter_d_npts <- c(",
    paste(vapply(inter_d, nrow, integer(1)), collapse = ", "), ")\n\n")

cat("# ── track_intermediate by duration (first interval) ──\n")
cat("# Interval 1: 1-hour spacing\n\n")
if (nrow(inter_t[[1]]) > 0) {
  fmt(inter_t[[1]]$int_x, "ref_inter_t_x")
  fmt(inter_t[[1]]$int_y, "ref_inter_t_y")
} else {
  cat("ref_inter_t_x <- numeric(0)\n")
  cat("ref_inter_t_y <- numeric(0)\n\n")
}
cat("ref_inter_t_npts <- c(",
    paste(vapply(inter_t, nrow, integer(1)), collapse = ", "), ")\n\n")

sink()

cat("Reference values written to reference-values.R\n")
cat("Inspect, then paste into the test file.\n")
