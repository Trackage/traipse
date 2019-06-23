test_that("bearing works", {
  expect_equivalent(round(track_bearing(c(0, 0, 1, 0), c(0, 1, 1, 0)), digits = 2L),
                    c(0.0, 89.99, -134.8, NA))
})
