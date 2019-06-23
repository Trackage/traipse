test_that("turn works", {
  expect_equivalent(round(track_turn(c(0, 0, 1, 0), c(0, 1, 1, 0)), digits = 2L),
                    c(NA, 89.99, 135.21, NA))
})
