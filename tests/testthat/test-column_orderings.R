test_that("column_orderings() works", {
  dt <- tibble(a = rep(0, 3),
               b = a + 1,
               c = rep(9, 3),
               d = c(-1, 9, 9),
               e = c(5, 9, 9),
               f = c(-1, 10, 10),
               g = c(0, 0, NA))
  expect_snapshot(column_orderings(dt))
})
