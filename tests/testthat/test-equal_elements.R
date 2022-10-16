test_that("equal_elements() works", {
  dt <- tibble::tibble(a = 1:3, b = a, c = 3:1, d = a, e = 4:6, f = e)
  expect_equal(equal_elements(dt), list(c("a", "b", "d"), "c", c("e", "f")))
  expect_equal(equal_elements(dt, keep_names = FALSE),
               list(c(1, 2, 4), 3, 5:6))
  expect_warning(out <- equal_elements(c(1:3, 2, 2, 1)), "no names")
  expect_equal(out, equal_elements(c(1:3, 2, 2, 1), keep_names = FALSE))
  expect_equal(out, list(c(1, 6), c(2, 4, 5), 3))
})
