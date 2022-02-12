test_that("col_spec() works", {
  tb <- tibble::tibble(a = c(TRUE, FALSE, NA),
                       b = 1:3,
                       c = c(1, 2, NA),
                       d = lubridate::today() + 1:3,
                       e = lubridate::now() + 1:3,
                       f = rep("2022-02-12 00:00:00", 3),
                       g = c("10:07:30", "12:07:30", "14:07:30"),
                       h = factor(1:3))
  expect_identical(col_spec(tb), "lidDTccf")
})
