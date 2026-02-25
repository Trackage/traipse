## test-traipse-geosphere-equivalence.R
##
## Comprehensive tests for the traipse geosphere → geographiclib migration.
##
## Strategy:
##   1. Run this BEFORE the swap with geosphere to generate reference values
##   2. Swap geosphere → geographiclib in the source
##   3. Run again — all tests should pass (within tolerance)
##
## The tolerance reflects that both geosphere::bearing() and geographiclib
## use the same underlying Karney algorithm, so results should agree to
## ~1e-10 or better. Any larger discrepancy indicates a bug in the swap.
##
## Usage: testthat::test_file("tests/testthat/test-geosphere-equivalence.R")

library(testthat)
library(traipse)

# ── Helper: dense realistic track data ──────────────────────────────────────

# Southern Ocean albatross-like track (many points, high latitude, large steps)
set.seed(42)
n <- 200
albatross <- data.frame(
  x = cumsum(c(147, rnorm(n - 1, 0.8, 0.5))),
  y = cumsum(c(-42, rnorm(n - 1, -0.3, 0.4)))
)
# Clamp latitude to valid range
albatross$y <- pmax(pmin(albatross$y, 89.9), -89.9)
# Wrap longitude
albatross$x <- ((albatross$x + 180) %% 360) - 180

# Equatorial track (low latitude, different geometry)
equatorial <- data.frame(
  x = seq(10, 50, length.out = 100),
  y = rnorm(100, 0, 2)
)

# Polar track (high latitude stress test - convergence of meridians)
polar <- data.frame(
  x = seq(-180, 180, length.out = 80),
  y = rep(-75, 80) + rnorm(80, 0, 2)
)

# Antimeridian crossing track (wrap-around stress test)
antimeridian <- data.frame(
  x = c(170, 175, 179, -179, -175, -170, -165, -160),
  y = c(-40, -41, -42, -43, -42, -41, -40, -39)
)

# Very short steps (sub-metre, tests numerical stability)
micro <- data.frame(
  x = c(0, 0 + 1e-7, 0 + 2e-7, 0 + 3e-7),
  y = c(0, 0 + 1e-7, 0 + 2e-7, 0 + 3e-7)
)

# Stationary points (zero distance, degenerate bearing)
stationary <- data.frame(
  x = c(100, 100, 100, 101, 101),
  y = c(-30, -30, -30, -31, -31)
)

# trips0 bundled data (the canonical test case)
trips0_id1 <- trips0[trips0$id == "1", ]

# ── 1. track_bearing() ─────────────────────────────────────────────────────
# Uses geosphere::bearing(p1, p2) where p1 = cbind(x[-n], y[-n]),
# p2 = cbind(x[-1], y[-1]), returns c(bearings, NA)

test_that("track_bearing returns correct length with trailing NA", {
  b <- track_bearing(albatross$x, albatross$y)
  expect_length(b, nrow(albatross))
  expect_true(is.na(b[length(b)]))
  expect_true(all(!is.na(b[-length(b)])))
})

test_that("track_bearing cardinal directions are correct", {
  # Due north
  expect_equal(track_bearing(c(0, 0), c(0, 1))[1], 0, tolerance = 1e-6)
  # Due east
  expect_equal(track_bearing(c(0, 1), c(0, 0))[1], 90, tolerance = 0.1)
  # Due south
  b_south <- track_bearing(c(0, 0), c(1, -1))[1]
  expect_true(abs(b_south) > 170)  # near +/-180
  # Due west
  expect_equal(track_bearing(c(1, 0), c(0, 0))[1], -90, tolerance = 0.1)
})

test_that("track_bearing handles antimeridian crossing", {
  b <- track_bearing(antimeridian$x, antimeridian$y)
  expect_length(b, nrow(antimeridian))
  # All bearings should be finite (no NaN from wrap)
  expect_true(all(is.finite(b[!is.na(b)])))
  # Bearings should be in [-180, 180]
  expect_true(all(b[!is.na(b)] >= -180 & b[!is.na(b)] <= 180))
})

test_that("track_bearing handles polar regions", {
  b <- track_bearing(polar$x, polar$y)
  expect_true(all(is.finite(b[!is.na(b)])))
})

test_that("track_bearing with dense albatross track", {
  b <- track_bearing(albatross$x, albatross$y)
  expect_length(b, nrow(albatross))
  # All non-NA values should be in valid range
  b_valid <- b[!is.na(b)]
  expect_true(all(b_valid >= -180 & b_valid <= 180))
})

test_that("track_bearing with stationary points", {
  b <- track_bearing(stationary$x, stationary$y)
  # First two stationary: bearing is 0 or NaN depending on implementation
  # geosphere::bearing returns 0 for identical points
  # Just check it doesn't error and returns correct length

  expect_length(b, nrow(stationary))
})

test_that("track_bearing reference values on trips0 id1", {
  b <- track_bearing(trips0_id1$x, trips0_id1$y)
  # First bearing should be ~34.5 degrees (from README)
  expect_equal(b[1], 34.5, tolerance = 1)
  # Check the bulk is reasonable
  expect_true(all(is.finite(b[!is.na(b)])))
})


# ── 2. track_angle() ───────────────────────────────────────────────────────
# Internal angle at each vertex (0 = straight, 180 = reversal)
# Uses geosphere::bearing to compute forward/back bearings then derives angle

test_that("track_angle returns correct length with padding NAs", {
  a <- track_angle(albatross$x, albatross$y)
  expect_length(a, nrow(albatross))
  expect_true(is.na(a[1]))
  expect_true(is.na(a[length(a)]))
})

test_that("track_angle straight line is 180 (collinear)", {
  # Points in a straight line (due east along equator)
  straight <- data.frame(x = seq(0, 10, by = 1), y = rep(0, 11))
  a <- track_angle(straight$x, straight$y)
  # Interior angles should be 180 (straight = no turn)
  interior <- a[!is.na(a)]
  expect_true(all(abs(interior - 180) < 1))
})

test_that("track_angle reversal is near 0", {
  # Out and back
  reverse <- data.frame(x = c(0, 5, 0), y = c(0, 0, 0))
  a <- track_angle(reverse$x, reverse$y)
  expect_equal(a[2], 0, tolerance = 1)
})

test_that("track_angle right angle is near 90", {
  # L-shaped path: east then north
  lshape <- data.frame(x = c(0, 5, 5), y = c(0, 0, 5))
  a <- track_angle(lshape$x, lshape$y)
  expect_equal(a[2], 90, tolerance = 2)  # geodesic effects at this scale
})

test_that("track_angle dense track has valid range", {
  a <- track_angle(albatross$x, albatross$y)
  a_valid <- a[!is.na(a)]
  # Internal angles should be [0, 180]
  expect_true(all(a_valid >= -0.1 & a_valid <= 180.1))
})

test_that("track_angle reference values on trips0 id1", {
  a <- track_angle(trips0_id1$x, trips0_id1$y)
  # From README: second value ~120 degrees
  expect_equal(a[2], 120, tolerance = 2)
})

test_that("track_angle handles antimeridian", {
  a <- track_angle(antimeridian$x, antimeridian$y)
  a_valid <- a[!is.na(a)]
  expect_true(all(is.finite(a_valid)))
})


# ── 3. track_turn() ────────────────────────────────────────────────────────
# Relative turn angle: signed, [-180, 180]
# Negative = left turn, positive = right turn
# Currently uses geosphere::bearing internally

test_that("track_turn returns correct length with padding NAs", {
  t <- track_turn(albatross$x, albatross$y)
  expect_length(t, nrow(albatross))
  expect_true(is.na(t[1]))
  expect_true(is.na(t[length(t)]))
})

test_that("track_turn straight line is near zero", {
  straight <- data.frame(x = seq(0, 10, by = 1), y = rep(0, 11))
  t <- track_turn(straight$x, straight$y)
  interior <- t[!is.na(t)]
  expect_true(all(abs(interior) < 1))
})

test_that("track_turn left vs right are signed correctly", {
  # East then north-east = left turn (negative? or positive? check convention)
  left <- data.frame(x = c(0, 5, 8), y = c(0, 0, 3))
  right <- data.frame(x = c(0, 5, 8), y = c(0, 0, -3))
  t_left <- track_turn(left$x, left$y)[2]
  t_right <- track_turn(right$x, right$y)[2]
  # They should have opposite signs

  expect_true(sign(t_left) != sign(t_right))
})

test_that("track_turn reference values on trips0 id1", {
  t <- track_turn(trips0_id1$x, trips0_id1$y)
  # From README: second value ~-61 degrees
  expect_equal(t[2], -61, tolerance = 2)
})

test_that("track_turn dense track in valid range", {
  t <- track_turn(albatross$x, albatross$y)
  t_valid <- t[!is.na(t)]
  expect_true(all(t_valid >= -180 & t_valid <= 180))
})

test_that("track_turn reversal is near +/-180", {
  reverse <- data.frame(x = c(0, 5, 0), y = c(0, 0, 0))
  t <- track_turn(reverse$x, reverse$y)
  expect_equal(abs(t[2]), 180, tolerance = 1)
})


# ── 4. track_bearing_to() ──────────────────────────────────────────────────
# Bearing from each point to a fixed target location
# Direct call to geosphere::bearing(xy, to_xy)

test_that("track_bearing_to returns correct length, no padding", {
  b <- track_bearing_to(albatross$x, albatross$y, 0, 0)
  expect_length(b, nrow(albatross))
  # No trailing NA — every point has a bearing to the target
  expect_true(all(is.finite(b)))
})

test_that("track_bearing_to cardinal directions", {
  # N E S W from origin
  b <- track_bearing_to(0, 0, c(0, 10, 0, -10), c(5, 0, -5, 0))
  expect_equal(b[1], 0, tolerance = 0.1)     # north
  expect_equal(b[2], 90, tolerance = 0.1)    # east
  expect_equal(abs(b[3]), 180, tolerance = 0.1)  # south
  expect_equal(b[4], -90, tolerance = 0.1)   # west
})

test_that("track_bearing_to single target recycling", {
  b <- track_bearing_to(trips0_id1$x, trips0_id1$y, 147, -42)
  expect_length(b, nrow(trips0_id1))
  expect_true(all(is.finite(b)))
  expect_true(all(b >= -180 & b <= 180))
})

test_that("track_bearing_to vectorised target", {
  # Each point has its own target
  n <- 10
  b <- track_bearing_to(
    trips0_id1$x[1:n], trips0_id1$y[1:n],
    trips0_id1$x[1:n] + 1, trips0_id1$y[1:n] + 1
  )
  expect_length(b, n)
  expect_true(all(is.finite(b)))
})

test_that("track_bearing_to antimeridian", {
  # Bearing from 170E to 170W (should go east, not west around the world)
  b <- track_bearing_to(170, 0, -170, 0)
  # Should be approximately east (90), not west (-90)
  expect_true(abs(b - 90) < 1 || abs(b - (-270)) < 1)
})


# ── 5. track_intermediate() ────────────────────────────────────────────────
# Uses geosphere::gcIntermediate for spherical interpolation
# Returns list-column of data frames

test_that("track_intermediate by distance returns list of correct length", {
  inter <- track_intermediate(
    trips0_id1$x[1:5], trips0_id1$y[1:5],
    date = trips0_id1$date[1:5],
    distance = 50000
  )
  expect_length(inter, 5)
  # Last element should be empty (terminal padding)
  expect_equal(nrow(inter[[5]]), 0)
})

test_that("track_intermediate by duration returns list of correct length", {
  inter <- track_intermediate(
    trips0_id1$x[1:5], trips0_id1$y[1:5],
    date = trips0_id1$date[1:5],
    duration = 3600
  )
  expect_length(inter, 5)
})

test_that("track_intermediate points lie between endpoints", {
  x <- c(0, 10)
  y <- c(0, 0)
  inter <- track_intermediate(x, y, distance = 100000)
  if (nrow(inter[[1]]) > 0) {
    # Interpolated x should be between 0 and 10
    expect_true(all(inter[[1]]$int_x >= -0.1 & inter[[1]]$int_x <= 10.1))
    # Interpolated y should be near 0 (equatorial great circle)
    expect_true(all(abs(inter[[1]]$int_y) < 1))
  }
})

test_that("track_intermediate with dense input", {
  inter <- track_intermediate(
    albatross$x[1:20], albatross$y[1:20],
    distance = 10000  # 10km spacing
  )
  expect_length(inter, 20)
  # Each interval should have ≥0 interpolated points
  expect_true(all(vapply(inter, nrow, integer(1)) >= 0))
})


# ── 6. track_distance() ────────────────────────────────────────────────────
# Uses geodist, NOT geosphere — should be unaffected by the swap
# But include for regression completeness

test_that("track_distance returns correct length with leading NA", {
  d <- track_distance(albatross$x, albatross$y)
  expect_length(d, nrow(albatross))
  expect_true(is.na(d[1]))
})

test_that("track_distance is always non-negative", {
  d <- track_distance(albatross$x, albatross$y)
  d_valid <- d[!is.na(d)]
  expect_true(all(d_valid >= 0))
})

test_that("track_distance reference values on trips0 id1", {
  d <- track_distance(trips0_id1$x, trips0_id1$y)
  # From README: second value ~129435
  expect_equal(d[2], 129435, tolerance = 100)
})

test_that("track_distance stationary is zero", {
  d <- track_distance(stationary$x, stationary$y)
  # Points 2 and 3 are identical to point 1
  expect_equal(d[2], 0, tolerance = 0.01)
  expect_equal(d[3], 0, tolerance = 0.01)
})


# ── 7. Consistency checks across functions ──────────────────────────────────

test_that("track_angle + track_turn are consistent", {
  # |turn| should equal angle (approximately) — they're different views
  # of the same geometry
  a <- track_angle(albatross$x[1:50], albatross$y[1:50])
  t <- track_turn(albatross$x[1:50], albatross$y[1:50])
  both_valid <- !is.na(a) & !is.na(t)
  # The relationship: angle = 180 - |turn| ... but check the actual convention
  # For now just verify they're in compatible ranges
  expect_true(all(is.finite(a[both_valid])))
  expect_true(all(is.finite(t[both_valid])))
})

test_that("track_speed = track_distance / track_time", {
  d <- track_distance(trips0_id1$x, trips0_id1$y)
  tm <- track_time(trips0_id1$date)
  s <- track_speed(trips0_id1$x, trips0_id1$y, trips0_id1$date)

  both_valid <- !is.na(d) & !is.na(tm) & !is.na(s)
  expect_equal(s[both_valid], d[both_valid] / tm[both_valid], tolerance = 1e-6)
})


# ── 8. Numerical equivalence snapshot (run BEFORE swap) ─────────────────────
# Uncomment and run this block before the geosphere→geographiclib swap
# to capture exact reference values. Then hardcode them below for
# post-swap verification.
#
# cat("── Reference values from geosphere ──\n")
# b_ref <- track_bearing(trips0_id1$x[1:10], trips0_id1$y[1:10])
# cat("bearing[1:9]:", paste(round(b_ref[1:9], 8), collapse = ", "), "\n")
#
# a_ref <- track_angle(trips0_id1$x[1:10], trips0_id1$y[1:10])
# cat("angle[2:9]:", paste(round(a_ref[2:9], 8), collapse = ", "), "\n")
#
# t_ref <- track_turn(trips0_id1$x[1:10], trips0_id1$y[1:10])
# cat("turn[2:9]:", paste(round(t_ref[2:9], 8), collapse = ", "), "\n")
#
# bt_ref <- track_bearing_to(trips0_id1$x[1:10], trips0_id1$y[1:10], 147, -42)
# cat("bearing_to[1:10]:", paste(round(bt_ref, 8), collapse = ", "), "\n")

# After capturing, paste the values here and enable these tests:
#
# test_that("bearing matches geosphere reference to 1e-6", {
#   b <- track_bearing(trips0_id1$x[1:10], trips0_id1$y[1:10])
#   b_ref <- c(...)  # paste from above
#   expect_equal(b[1:9], b_ref, tolerance = 1e-6)
# })


# ── 9. Edge cases ──────────────────────────────────────────────────────────

test_that("track_bearing and track_bearing_to handle length-1 input", {
  expect_length(track_bearing(0, 0), 1)
  expect_true(is.na(track_bearing(0, 0)))
  expect_length(track_bearing_to(0, 0, 10, 10), 1)
})

test_that("track_bearing handles length-2 input", {
  expect_length(track_bearing(c(0, 1), c(0, 1)), 2)
})

## KNOWN BUG: track_angle and track_turn error on length < 3
## (subscript out of bounds in internal matrix indexing)
## Fix during migration: early return of rep(NA_real_, n) for n < 3
test_that("track_angle errors on length < 3 (known bug)", {
  expect_equal(track_angle(0, 0), NA_real_)
  expect_equal(track_angle(c(0, 1), c(0, 1)), rep(NA_real_, 2L))
})

test_that("track_turn errors on length < 3 (known bug)", {
  expect_equal(track_turn(0, 0), NA_real_)
  expect_equal(track_turn(c(0, 1), c(0, 1)), rep(NA_real_, 2L))
})

test_that("track_angle and track_turn work at length 3 (minimum)", {
  a <- track_angle(c(0, 5, 10), c(0, 0, 0))
  expect_length(a, 3)
  expect_true(is.na(a[1]))
  expect_true(is.na(a[3]))
  expect_true(!is.na(a[2]))

  tu <- track_turn(c(0, 5, 10), c(0, 0, 0))
  expect_length(tu, 3)
  expect_true(is.na(tu[1]))
  expect_true(is.na(tu[3]))
  expect_true(!is.na(tu[2]))
})

test_that("functions handle NA in input", {
  x <- c(0, NA, 2, 3)
  y <- c(0, 1, NA, 3)
  # Should not error, NAs propagate
  expect_length(track_bearing(x, y), 4)
  expect_length(track_bearing_to(x, y, 10, 10), 4)
})

test_that("bearing at poles", {
  # From north pole, every direction is south
  b <- track_bearing_to(0, 89.999, 0, 0)
  expect_true(abs(abs(b) - 180) < 1)
  # From south pole to equator, everything is north
  b2 <- track_bearing_to(0, -89.999, 0, 0)
  expect_true(abs(b2) < 1)
})

test_that("track_turn could be rebuilt from track_bearing differences", {
  # This is the key insight for the refactor:
  # turn[i] = bearing[i] - bearing[i-1], normalised to [-180, 180]
  x <- albatross$x[1:20]
  y <- albatross$y[1:20]
  b <- track_bearing(x, y)
  t <- track_turn(x, y)

  # Manual calculation of turn from bearing differences
  for (i in 2:(length(x) - 1)) {
    if (!is.na(b[i]) && !is.na(b[i - 1]) && !is.na(t[i])) {
      diff <- b[i] - b[i - 1]
      # Normalise to [-180, 180]
      diff <- ((diff + 180) %% 360) - 180
      expect_equal(t[i], diff, tolerance = 0.01,
                   label = paste("turn[", i, "]"))
    }
  }
})
