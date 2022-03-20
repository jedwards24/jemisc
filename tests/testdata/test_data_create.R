tb <- tibble::tibble(a = c(TRUE, FALSE, NA),
                     b = 1:3,
                     c = c(1, 2, NA),
                     d = c("2021-01-01", "B2021-01-01", "2021-01-01"),
                     e = as.character(lubridate::now() + 1:3),
                     f = rep("2022-02-12 00:00:00", 3),
                     g = c("10:07:30", "12:07:30", "14:07:30"),
                     h = factor(1:3))
tb
readr::write_csv(tb, "tests/testdata/data_col_spec.csv")
tb2 <- readr::read_csv("tests/testdata/data_col_spec.csv")
spec(tb2)
