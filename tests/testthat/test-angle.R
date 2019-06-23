test_that("angle works", {
  expect_equivalent(round(track_angle(c(0, 0, 1, 0), c(0, 1, 1, 0)), digits = 2L),
                    c(NA, 90.01, 44.81, NA))
})


