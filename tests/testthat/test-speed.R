test_that("speed works", {
  expect_equivalent(round(track_speed(c(0, 0, 1, 0), c(0, 1, 1, 0),
                                      ISOdate(2001, 1, 1) + c(1:4)), digits = 2L),
                    c(NA, 110574.39, 111302.65, 156899.57))
})
