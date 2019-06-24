test_that("angle works", {
  expect_equivalent(round(track_angle(c(0, 0, 1, 0), c(0, 1, 1, 0)), digits = 2L),
                    c(NA, 90.01, 44.81, NA))


  expect_equivalent(round(track_angle(c(147, 140, 140, 100), c(-42, -30, 10, -42)), digits = 2L),
                    c(NA, 156.64, 32.38, NA))

  library(dplyr)
  d <- tibble::tibble(x = c(0, 20, 50, 80, 80, 50, 170, -170, -100, -100),
                      y = c(0, 10, 20, 30, -30, -30, -65, 0, 10, 20), id = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2))
  expect_equal(d %>%
                group_by(id) %>% mutate(angle = track_angle(x, y)) %>% pull(angle) %>% is.na() %>% which(),
               c(1, 4, 5, 10))

  expect_equal(d %>%
                 group_by(id) %>% mutate(angle = round(track_angle(x, y))) %>% pull(angle),
               c(NA, 177, 168, NA, NA, 60, 151, 110, 94, NA))

})


