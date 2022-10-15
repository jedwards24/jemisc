# Methods to select values from a set of columns using a numeric index column
# Name??: case_by_index(), lookup?

library(tidyverse)
library(glue)

# Data -----
nn <- 1e6
dt <- tibble(ind = sample(1:3, nn, TRUE),
             x1 = sample(11:12, nn, TRUE),
             x2 = sample(13:14, nn, TRUE),
             x3 = sample(15:16, nn, TRUE))
summarise_all(dt, mean)

# 0.1s
system.time(dt0 <- mutate_from_index(dt, "ind", glue("x{1:3}"), into = "new"))

# test output
dt0 %>%
  group_by(ind) %>%
  summarise_all(mean)

mutate_from_index <- function(data, index_col, ..., into) {
  dt_list <- data %>%
    select(...) %>%
    mutate(index_col = data[[index_col]]) %>%
    mutate(row_id = row_number()) %>%
    group_split(index_col)

  f <- function(x, i) {
    mutate(x,  temp_name := x[[i]])
  }
  new_col <- map2_dfr(dt_list, seq_along(dt_list), f) %>%
    arrange(row_id) %>%
    select(temp_name) %>%
    rename({{ into }} := temp_name)
  bind_cols(data, new_col)
}

# takes 116s
system.time(
  dt1 <- dt %>%
    rowwise() %>%
    mutate(new = c_across(glue("x{1:3}"))[ind]) %>%
    ungroup()
)
identical(dt0, dt1)
#waldo::compare(dt0, dt1)

# takes 6s
system.time(
  dt2 <- dt %>%
    rowwise() %>%
    mutate(new =
             list(x1, x2, x3)[[ind]]) %>%
    ungroup()
)
identical(dt0, dt2)

# takes 7s
system.time({
  dtsub <- select(dt, x1, x2, x3)
  new <- integer(nrow(dt))
  for (i in seq_along(new)){
    new[i] <- dtsub[[dt$ind[i]]][i]
  }
  dt3 <- mutate(dt, new)
})
identical(dt0, dt3)
#waldo::compare(dt0, dt3)

# 12s
system.time({
  dtsub <- select(dt, x1, x2, x3) %>%
    {as_tibble(cbind(t(.)))}
  new <- map2_int(dtsub, dt$ind, ~.x[[.y]])
  dt4 <- mutate(dt, new = unname(new))
})
identical(dt0, dt4)

#0.2s
system.time({
  dt5 <- mutate(dt,
                new = case_when(
                  ind == 1L ~ x1,
                  ind == 2L ~ x2,
                  ind == 3L ~ x3
                ))
})
identical(dt0, dt5)



#0.1s
system.time(dt6 <- case_by_index(dt, ind, glue("x{1:3}"), into = "new"))
system.time(dt6 <- case_by_index(dt, ind, c(x1, x2, x3), into = "new"))

identical(dt0, dt6)
system.time(dt6 <- case_by_index(dt, "ind", glue("x{1:3}"), into = "new"))

# profiling --------
library(profvis)
profvis(case_by_index(dt, ind, glue("x{1:3}"), into = "new"))
profvis(mutate_from_index(dt, "ind", glue("x{1:3}"), into = "new"))

# col_spec------------

library(tidyverse)
x <- parse_guess("12:05:30")
parse_guess("12:05:30T000")
parse_time("12:05:30") %>% class()
?is.numeric.difftime
typeof(x)
class(x)
hms::hms("12:05:30")

tb2 <- readr::read_csv("tests/testdata/data_col_spec.csv")
spec(tb2)
map(tb2, class)
col_spec(tb)
col_spec(tb2)
?read_csv

readr::read_csv("tests/testdata/data_col_spec.csv", col_types = "li-_Tt")
vignette("readr")
spec_csv("tests/testdata/data_col_spec.csv")
tb3 <- map_df(tb, as.character)
type_convert(tb3, guess_integer = T)
type_convert(1)

factor(letters[1:3]) %>% typeof()
