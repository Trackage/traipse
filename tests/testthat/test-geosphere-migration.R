## test-geosphere-migration.R
##
## Two-phase test suite for traipse geosphere -> geographiclib migration.
##
## Phase 1 (BEFORE swap):
##   - Run capture-reference-values.R to dump current geosphere outputs
##   - Run this file — all structural/behavioural tests pass,
##     snapshot tests are skipped until reference values are pasted in
##
## Phase 2 (AFTER swap):
##   - Paste geosphere reference values into the snapshot section below
##   - Run this file — structural tests still pass
##   - Snapshot tests compare new geographiclib values against old geosphere values
##   - For bearing/angle/turn: expect agreement to ~1e-6 (same Karney algorithm)
##   - For track_intermediate: values WILL differ (spherical -> ellipsoidal),
##     document the magnitude of change, then update reference values to
##     the new geographiclib ground truth
##
## Phase 3 (NEW ground truth):
##   - Replace geosphere reference values with geographiclib values
##   - All tests pass with tight tolerance — this is the permanent test suite

library(testthat)
library(traipse)

# ── Test data ───────────────────────────────────────────────────────────────

trips0_id1 <- trips0[trips0$id == "1", ]

# Dense realistic track (~200 pts, southern ocean)
set.seed(42)
n_dense <- 200
dense <- local({
  x <- cumsum(c(147, rnorm(n_dense - 1, 0.8, 0.5)))
  y <- cumsum(c(-42, rnorm(n_dense - 1, -0.3, 0.4)))
  y <- pmax(pmin(y, 89.9), -89.9)
  x <- ((x + 180) %% 360) - 180
  data.frame(x = x, y = y)
})

# Antimeridian crossing
anti <- data.frame(
  x = c(170, 175, 179, -179, -175, -170, -165, -160),
  y = c(-40, -41, -42, -43, -42, -41, -40, -39)
)

# Polar
polar <- data.frame(
  x = seq(-180, 170, by = 10),
  y = rep(-75, 36)
)

# ════════════════════════════════════════════════════════════════════════════
# A. STRUCTURAL TESTS — these are invariant across implementations
# ════════════════════════════════════════════════════════════════════════════

# ── track_bearing ──

test_that("track_bearing: length and NA padding", {
  b <- track_bearing(dense$x, dense$y)
  expect_length(b, nrow(dense))
  expect_true(is.na(b[length(b)]))
  expect_true(all(!is.na(b[-length(b)])))
})

test_that("track_bearing: values in [-180, 180]", {
  b <- track_bearing(dense$x, dense$y)
  bv <- b[!is.na(b)]
  expect_true(all(bv >= -180 & bv <= 180))
})

test_that("track_bearing: cardinal directions", {
  # Due north from origin
  expect_equal(track_bearing(c(0, 0), c(0, 1))[1], 0, tolerance = 1e-4)
  # Due east (approximately)
  b_east <- track_bearing(c(0, 1), c(0, 0))[1]
  expect_equal(b_east, 90, tolerance = 0.5)
  # Due west
  b_west <- track_bearing(c(1, 0), c(0, 0))[1]
  expect_equal(b_west, -90, tolerance = 0.5)
  # Due south
  b_south <- track_bearing(c(0, 0), c(0, -1))[1]
  expect_true(abs(b_south) > 179)
})

test_that("track_bearing: antimeridian is finite", {
  b <- track_bearing(anti$x, anti$y)
  expect_true(all(is.finite(b[!is.na(b)])))
})

test_that("track_bearing: polar is finite", {
  b <- track_bearing(polar$x, polar$y)
  expect_true(all(is.finite(b[!is.na(b)])))
})

# ── Edge cases: short input ──

test_that("track_bearing: length 1 returns NA", {
  expect_length(track_bearing(0, 0), 1)
  expect_true(is.na(track_bearing(0, 0)))
})

test_that("track_bearing: length 2 has one value + NA", {
  b <- track_bearing(c(0, 1), c(0, 1))
  expect_length(b, 2)
  expect_true(!is.na(b[1]))
  expect_true(is.na(b[2]))
})

## KNOWN BUG: track_angle and track_turn error on length < 3
## (subscript out of bounds in internal matrix indexing)
## TODO: fix during migration — early return of rep(NA_real_, n) for n < 3
test_that("track_angle: errors on length < 3 (known bug, fix in migration)", {
  expect_equal(track_angle(0, 0), NA_real_)
  expect_equal(track_angle(c(0, 1), c(0, 1)), rep(NA_real_, 2L))
})

test_that("track_turn: errors on length < 3 (known bug, fix in migration)", {
  expect_equal(track_turn(0, 0), NA_real_)
  expect_equal(track_turn(c(0, 1), c(0, 1)), rep(NA_real_, 2L))
})

test_that("track_angle: minimum length 3 works", {
  a <- track_angle(c(0, 5, 10), c(0, 0, 0))
  expect_length(a, 3)
  expect_true(is.na(a[1]))
  expect_true(is.na(a[3]))
  expect_equal(a[2], 180, tolerance = 1)  # straight line
})

test_that("track_turn: minimum length 3 works", {
  tu <- track_turn(c(0, 5, 10), c(0, 0, 0))
  expect_length(tu, 3)
  expect_true(is.na(tu[1]))
  expect_true(is.na(tu[3]))
  expect_equal(tu[2], 0, tolerance = 1)  # straight = no turn
})

# ── track_angle ──

test_that("track_angle: length and NA padding", {
  a <- track_angle(dense$x, dense$y)
  expect_length(a, nrow(dense))
  expect_true(is.na(a[1]))
  expect_true(is.na(a[length(a)]))
})

test_that("track_angle: straight line ~= 180", {
  straight <- data.frame(x = seq(0, 10, by = 1), y = rep(0, 11))
  a <- track_angle(straight$x, straight$y)
  interior <- a[!is.na(a)]
  expect_true(all(abs(interior - 180) < 2))
})

test_that("track_angle: reversal ~= 0", {
  rev <- data.frame(x = c(0, 5, 0), y = c(0, 0, 0))
  a <- track_angle(rev$x, rev$y)
  expect_equal(a[2], 0, tolerance = 1)
})

test_that("track_angle: right angle ~= 90", {
  lshape <- data.frame(x = c(0, 5, 5), y = c(0, 0, 5))
  a <- track_angle(lshape$x, lshape$y)
  expect_equal(a[2], 90, tolerance = 3)
})

test_that("track_angle: values in [0, 180]", {
  a <- track_angle(dense$x, dense$y)
  av <- a[!is.na(a)]
  expect_true(all(av >= -0.1 & av <= 180.1))
})

# ── track_turn ──

test_that("track_turn: length and NA padding", {
  tu <- track_turn(dense$x, dense$y)
  expect_length(tu, nrow(dense))
  expect_true(is.na(tu[1]))
  expect_true(is.na(tu[length(tu)]))
})

test_that("track_turn: values in [-180, 180]", {
  tu <- track_turn(dense$x, dense$y)
  tv <- tu[!is.na(tu)]
  expect_true(all(tv >= -180 & tv <= 180))
})

test_that("track_turn: straight ~= 0", {
  straight <- data.frame(x = seq(0, 10, by = 1), y = rep(0, 11))
  tu <- track_turn(straight$x, straight$y)
  interior <- tu[!is.na(tu)]
  expect_true(all(abs(interior) < 2))
})

test_that("track_turn: left and right have opposite signs", {
  left  <- data.frame(x = c(0, 5, 8), y = c(0, 0, 3))
  right <- data.frame(x = c(0, 5, 8), y = c(0, 0, -3))
  tl <- track_turn(left$x, left$y)[2]
  tr <- track_turn(right$x, right$y)[2]
  expect_true(sign(tl) != sign(tr))
})

test_that("track_turn: reversal ~= ±180", {
  rev <- data.frame(x = c(0, 5, 0), y = c(0, 0, 0))
  tu <- track_turn(rev$x, rev$y)
  expect_equal(abs(tu[2]), 180, tolerance = 1)
})

test_that("track_turn ~= diff(track_bearing) normalised", {
  x <- dense$x[1:30]
  y <- dense$y[1:30]
  b  <- track_bearing(x, y)
  tu <- track_turn(x, y)
  for (i in 2:(length(x) - 1)) {
    if (!is.na(b[i]) && !is.na(b[i - 1]) && !is.na(tu[i])) {
      diff_b <- b[i] - b[i - 1]
      diff_b <- ((diff_b + 180) %% 360) - 180
      expect_equal(tu[i], diff_b, tolerance = 0.1,
                   label = sprintf("turn[%d]", i))
    }
  }
})

# ── track_bearing_to ──

test_that("track_bearing_to: no NA padding, correct length", {
  bt <- track_bearing_to(dense$x, dense$y, 0, 0)
  expect_length(bt, nrow(dense))
  expect_true(all(is.finite(bt)))
})

test_that("track_bearing_to: cardinal from origin", {
  bt <- track_bearing_to(0, 0, c(0, 10, 0, -10), c(5, 0, -5, 0))
  expect_equal(bt[1], 0, tolerance = 0.1)        # N
  expect_equal(bt[2], 90, tolerance = 0.1)       # E
  expect_equal(abs(bt[3]), 180, tolerance = 0.1) # S
  expect_equal(bt[4], -90, tolerance = 0.1)      # W
})

test_that("track_bearing_to: single target recycled", {
  bt <- track_bearing_to(trips0_id1$x, trips0_id1$y, 147, -42)
  expect_length(bt, nrow(trips0_id1))
  expect_true(all(is.finite(bt)))
})

test_that("track_bearing_to: vectorised targets", {
  nn <- 10
  bt <- track_bearing_to(
    trips0_id1$x[1:nn], trips0_id1$y[1:nn],
    trips0_id1$x[1:nn] + 1, trips0_id1$y[1:nn] + 1
  )
  expect_length(bt, nn)
  expect_true(all(is.finite(bt)))
})

test_that("track_bearing_to: from poles", {
  # North pole -> equator = due south
  b <- track_bearing_to(0, 89.999, 0, 0)
  expect_true(abs(abs(b) - 180) < 1)
  # South pole -> equator = due north
  b2 <- track_bearing_to(0, -89.999, 0, 0)
  expect_true(abs(b2) < 1)
})

# ── track_intermediate ──

test_that("track_intermediate: list of correct length", {
  inter <- track_intermediate(
    trips0_id1$x[1:5], trips0_id1$y[1:5],
    date = trips0_id1$date[1:5], distance = 50000
  )
  expect_length(inter, 5)
  expect_equal(nrow(inter[[5]]), 0)  # terminal padding
})

test_that("track_intermediate: points between endpoints", {
  inter <- track_intermediate(c(0, 10), c(0, 0), distance = 100000)
  if (nrow(inter[[1]]) > 0) {
    expect_true(all(inter[[1]]$int_x >= -0.5 & inter[[1]]$int_x <= 10.5))
    expect_true(all(abs(inter[[1]]$int_y) < 1))
  }
})

test_that("track_intermediate: more points with finer spacing", {
  coarse <- track_intermediate(
    trips0_id1$x[1:3], trips0_id1$y[1:3],
    date = trips0_id1$date[1:3], distance = 100000
  )
  fine <- track_intermediate(
    trips0_id1$x[1:3], trips0_id1$y[1:3],
    date = trips0_id1$date[1:3], distance = 10000
  )
  expect_true(nrow(fine[[1]]) >= nrow(coarse[[1]]))
})

# ── track_distance (geodist-based, NOT geosphere — regression guard) ──

test_that("track_distance: length and leading NA", {
  d <- track_distance(dense$x, dense$y)
  expect_length(d, nrow(dense))
  expect_true(is.na(d[1]))
})

test_that("track_distance: non-negative", {
  d <- track_distance(dense$x, dense$y)
  expect_true(all(d[!is.na(d)] >= 0))
})

test_that("track_distance: stationary = 0", {
  d <- track_distance(c(100, 100, 100), c(-30, -30, -30))
  expect_equal(d[2], 0, tolerance = 0.01)
  expect_equal(d[3], 0, tolerance = 0.01)
})

test_that("track_speed = track_distance / track_time", {
  d  <- track_distance(trips0_id1$x, trips0_id1$y)
  tm <- track_time(trips0_id1$date)
  s  <- track_speed(trips0_id1$x, trips0_id1$y, trips0_id1$date)
  ok <- !is.na(d) & !is.na(tm) & !is.na(s) & tm > 0
  expect_equal(s[ok], d[ok] / tm[ok], tolerance = 1e-6)
})

# ── NA propagation ──

test_that("NA in input propagates correctly", {
  x <- c(0, NA, 2, 3, 4)
  y <- c(0, 1, NA, 3, 4)
  expect_length(track_bearing(x, y), 5)
  expect_length(track_angle(x, y), 5)
  expect_length(track_turn(x, y), 5)
  expect_length(track_bearing_to(x, y, 10, 10), 5)
  # Bearings touching an NA input should be NA
  b <- track_bearing(x, y)
  expect_true(is.na(b[1]))  # (0,0) -> (NA,1)
  expect_true(is.na(b[2]))  # (NA,1) -> (2,NA)
})


# ════════════════════════════════════════════════════════════════════════════
# B. SNAPSHOT TESTS — capture then verify
# ════════════════════════════════════════════════════════════════════════════
#
# INSTRUCTIONS:
#   1. Run capture-reference-values.R against current geosphere-backed traipse
#   2. Paste the output reference-values.R content below (replace the NULLs)
#   3. Uncomment the test blocks
#   4. Run after the geographiclib swap
#
# For bearing/angle/turn: use tight tolerance (1e-6) — same Karney algorithm
# For track_intermediate: values WILL change (spherical -> ellipsoidal).
#   First run with loose tolerance to see the magnitude, then tighten.

## Paste reference-values.R content here:
# source("reference-values.R")  # or inline below

## Reference values captured from traipse 0.3.0.9001 with geosphere 1.5.20
## Date: 2026-02-25 13:41:37 UTC

# ── trips0 id1 first 20 points ──

ref_bearing <- c( 34.5207685919, -26.4750016024, 158.0749290697, -9.1990066797, 98.4537393364, -56.7290950868, 34.1307276223, 12.8844075884, 168.8959701847, 53.0686621582, -34.2576755138, 62.0681521435, 56.2023163169, 80.9624056812, 54.2562389001, 20.6505619041, -60.2760682233, -111.5174736839, 46.4798942180, NA_real_ )

ref_angle <- c( NA_real_, 119.5900724486, 4.4286404381, 13.0878908080, 72.3866315292, 25.5956429237, 89.5222366524, 159.2318550528, 23.7686009703, 64.2987302946, 93.3147204731, 84.1268457769, 175.3726865205, 154.6829639546, 153.6247107474, 146.8644512727, 99.1826941893, 128.3205742823, 22.5009314406, NA_real_ )

ref_turn <- c( NA_real_, -60.9957701943, -175.4500693278, -167.2739357494, 107.6527460161, -155.1828344232, 90.8598227091, -21.2463200339, 156.0115625963, -115.8273080265, -87.3263376720, 96.3258276573, -5.8658358266, 24.7600893643, -26.7061667811, -33.6056769959, -80.9266301275, -51.2414054606, 157.9973679020, NA_real_ )

ref_bearing_to <- c( 99.9145104064, 101.8223154084, 102.5153399694, 100.0057199620, 100.6670450269, 99.9731303702, 100.8515638911, 102.6089720435, 105.3108761782, 103.4334943286, 104.7066499770, 106.7829259231, 108.8517471490, 110.1745081061, 110.4369747690, 111.7749922279, 112.8479818304, 113.5112288013, 112.6387490279, 114.7053479132 )

# ── antimeridian track ──

ref_bearing_anti <- c( 106.2964798290, 109.7124790916, 124.7193393335, 72.6913925820, 76.7689311981, 76.9520809062, 77.1245646383, NA_real_ )

ref_angle_anti <- c( NA_real_, 173.3354400022, 162.3419521641, 129.3233600914, 173.2193985189, 176.5024394394, 176.5789555327, NA_real_ )

ref_turn_anti <- c( NA_real_, 3.4159992625, 15.0068602420, -52.0279467515, 4.0775386161, 0.1831497081, 0.1724837320, NA_real_ )

# ── polar track (bearing only, latitude = -75) ──

ref_bearing_polar <- c( 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, 94.8304497586, NA_real_ )

# ── cardinal bearing-to from origin ──

ref_bearing_to_cardinal <- c( 0.0000000000, 90.0000000000, 180.0000000000, -90.0000000000 )

# ── track_intermediate by distance (first interval) ──
# Interval 1: (x[1],y[1]) -> (x[2],y[2]), 50km spacing

ref_inter_d_x <- c( 115.2766532944, 115.4984694774, 115.7186075881, 115.9370913990, 116.1539443500 )

ref_inter_d_y <- c( -42.3757967249, -42.1355439728, -41.8948672243, -41.6537732160, -41.4122685656 )

ref_inter_d_npts <- c( 5, 5, 5, 5, 0 )

# ── track_intermediate by duration (first interval) ──
# Interval 1: 1-hour spacing

ref_inter_t_x <- c( 115.2766532944, 115.4984694774, 115.7186075881, 115.9370913990, 116.1539443500 )

ref_inter_t_y <- c( -42.3757967249, -42.1355439728, -41.8948672243, -41.6537732160, -41.4122685656 )

ref_inter_t_npts <- c( 5, 5, 5, 5, 0 )


test_that("SNAPSHOT: track_bearing matches reference", {
  skip_if(is.null(ref_bearing), "Reference values not yet captured")
  x <- trips0_id1$x[1:20]
  y <- trips0_id1$y[1:20]
  b <- track_bearing(x, y)
  expect_equal(b, ref_bearing, tolerance = 1e-6)
})

test_that("SNAPSHOT: track_angle matches reference", {
  skip_if(is.null(ref_angle), "Reference values not yet captured")
  x <- trips0_id1$x[1:20]
  y <- trips0_id1$y[1:20]
  a <- track_angle(x, y)
  expect_equal(a, ref_angle, tolerance = 1e-6)
})

test_that("SNAPSHOT: track_turn matches reference", {
  skip_if(is.null(ref_turn), "Reference values not yet captured")
  x <- trips0_id1$x[1:20]
  y <- trips0_id1$y[1:20]
  tu <- track_turn(x, y)
  expect_equal(tu, ref_turn, tolerance = 1e-6)
})

test_that("SNAPSHOT: track_bearing_to matches reference", {
  skip_if(is.null(ref_bearing_to), "Reference values not yet captured")
  x <- trips0_id1$x[1:20]
  y <- trips0_id1$y[1:20]
  bt <- track_bearing_to(x, y, 147, -42)
  expect_equal(bt, ref_bearing_to, tolerance = 1e-6)
})

test_that("SNAPSHOT: antimeridian bearing matches reference", {
  skip_if(is.null(ref_bearing_anti), "Reference values not yet captured")
  ax <- c(170, 175, 179, -179, -175, -170, -165, -160)
  ay <- c(-40, -41, -42, -43, -42, -41, -40, -39)
  b <- track_bearing(ax, ay)
  expect_equal(b, ref_bearing_anti, tolerance = 1e-6)
})

test_that("SNAPSHOT: polar bearing matches reference", {
  skip_if(is.null(ref_bearing_polar), "Reference values not yet captured")
  px <- seq(-180, 170, by = 10)
  py <- rep(-75, 36)
  b <- track_bearing(px, py)
  expect_equal(b, ref_bearing_polar, tolerance = 1e-6)
})

test_that("SNAPSHOT: track_intermediate point count matches reference", {
  skip_if(is.null(ref_inter_d_npts), "Reference values not yet captured")
  inter <- track_intermediate(
    trips0_id1$x[1:5], trips0_id1$y[1:5],
    date = trips0_id1$date[1:5], distance = 50000
  )
  npts <- vapply(inter, nrow, integer(1))
  # Point counts may differ slightly with ellipsoidal intermediate
  # (slightly different total distance -> different number of steps)
  # Allow ±1 point difference per interval
  expect_true(all(abs(npts - ref_inter_d_npts) <= 1),
    info = paste("Got:", paste(npts, collapse = ","),
                 "Ref:", paste(ref_inter_d_npts, collapse = ",")))
})


#════════════════════════════════════════════════════════════════════════════
#C. DIAGNOSTIC COMPARISON (run manually during migration)
#════════════════════════════════════════════════════════════════════════════

#After pasting reference values above, uncomment this to see HOW MUCH
#values change (especially for track_intermediate ellipsoidal vs spherical):

test_that("DIAGNOSTIC: quantify intermediate point drift", {
  skip_if(is.null(ref_inter_d_x), "Reference values not yet captured")
  inter <- track_intermediate(
    trips0_id1$x[1:5], trips0_id1$y[1:5],
    date = trips0_id1$date[1:5], distance = 50000
  )
  if (nrow(inter[[1]]) > 0 && length(ref_inter_d_x) > 0) {
    nn <- min(nrow(inter[[1]]), length(ref_inter_d_x))
    dx <- inter[[1]]$int_x[1:nn] - ref_inter_d_x[1:nn]
    dy <- inter[[1]]$int_y[1:nn] - ref_inter_d_y[1:nn]
    cat("\nIntermediate point drift (spherical -> ellipsoidal):\n")
    cat("  max |dlon|:", max(abs(dx)), "degrees\n")
    cat("  max |dlat|:", max(abs(dy)), "degrees\n")
    cat("  mean |dlon|:", mean(abs(dx)), "degrees\n")
    cat("  mean |dlat|:", mean(abs(dy)), "degrees\n")
    # At southern ocean latitudes, expect ~0.1-0.3% difference
    # between spherical and ellipsoidal intermediate points
  }
  succeed()  # diagnostic only, always passes
})
