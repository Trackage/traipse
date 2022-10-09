test_that("time works", {
  expect_equivalent(round(track_time(ISOdate(2001, 1, 1) + c(1:4)), digits = 2L),
                    c(NA, 1, 1, 1))

  expect_equivalent(round(track_time(as.POSIXlt(ISOdate(2001, 1, 1) + c(1:4))), digits = 2L),
                    c(NA, 1, 1, 1))


  ## doesn't error in R-devel 2022-01-09
  if (packageVersion("base") <= "4.2.1") {
  expect_error(track_time(1:4),
               "Cannot convert 'date' of class 'integer' to POSIXct")


  }
})


test_that("coercion from date works", {
  expect_equivalent(round(track_time(as.Date(ISOdate(2001, 1, 1)) + c(1:4)), digits = 2L),
                    c(NA, rep(24 * 3600, 3L)))
})
