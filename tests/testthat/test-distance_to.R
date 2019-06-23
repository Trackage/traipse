test_that("distance to works", {
  expect_equivalent(round(track_distance_to(c(0, 0, 1, 0), c(0, 1, 1, 0),
                                            0, 0), digits = 2L),
                    c(0.0, 110574.39, 156899.57, 0.0))
})
