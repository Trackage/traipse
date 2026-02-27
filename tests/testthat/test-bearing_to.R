test_that("bearing to works", {
    expect_equivalent(round(track_bearing_to(c(0, 0, 1, 0), c(0, 1, 1, 0),
                                             0, 0), digits = 2L),
                      c(180.0, 180.0, -134.8, 180.0))

})
