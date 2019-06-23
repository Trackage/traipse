test_that("distance works", {
  expect_equivalent(round(track_distance(c(0, 0, 1, 0), c(0, 1, 1, 0)), digits = 2L),
                    c(NA, 110574.39, 111302.65, 156899.57))
})


