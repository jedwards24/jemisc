# Find sets of columns which are equal

dt <- tibble(a = 1:3, b = a, c = 3:1, d = a, e = 4:6, f = e)
map_lgl(dt, ~identical(., dt$a))

equal_col_sets <- function(df) {
  n <- ncol(df)
  sets <- vector("list", n)
  for (i in 1:n){
    sets[[i]] <- (1:n)[map_lgl(df, ~identical(., df[[i]]))]
  }
  map(unique(sets), ~names(df)[.])
}
equal_col_sets(dt)

equal_col_sets2 <- function(df) {
  done <- c()
  n <- ncol(df)
  sets <- vector("list", n)
  for (i in 1:n){
    if (!i %in% done){
      sets[[i]] <- (1:n)[map_lgl(df, ~identical(., df[[i]]))]
      done <- c(done, sets[[i]])
    }
  }
  map(purrr::compact(sets), ~names(df)[.])
}
equal_col_sets2(dt)

equal_col_sets3 <- function(df) {
  done <- c()
  n <- ncol(df)
  sets <- vector("list", n)
  for (i in 1:n){
    if (i %in% done) next
    sets[[i]] <- (1:n)[map_lgl(df, ~identical(., df[[i]]))]
    done <- c(done, sets[[i]])
  }
  map(purrr::compact(sets), ~names(df)[.])
}
equal_col_sets3(dt)
bench::mark(equal_col_sets(dt), equal_col_sets2(dt), equal_col_sets3(dt), iterations = 5000)

# Handling NAs
equal_col_sets4 <- function(x, na.rm = TRUE) {
  done <- c()
  n <- length(x)
  sets <- vector("list", n)
  for (i in 1:n){
    if (i %in% done) next
    sets[[i]] <- (1:n)[map_lgl(x, ~all(. == x[[i]], na.rm = na.rm))]
    done <- c(done, sets[[i]])
  }
  map(purrr::compact(sets), ~names(x)[.])
}


equal_col_sets5 <- function(x, na.rm = TRUE) {
  done <- c()
  n <- length(x)
  sets <- vector("list", n)
  for (i in 1:n){
    if (i %in% done) next
    sets[[i]] <- (1:n)[map_lgl(x, ~all(. == x[[i]], na.rm = na.rm))]
    if (!is.null(sets[[i]])) done <- c(done, sets[[i]])
  }
  map(purrr::compact(sets), ~names(x)[.])
}


dt <- 1:10
bench::mark(equal_col_sets4(dt), equal_col_sets5(dt), iterations = 1000) #5 is slower

equal_col_sets4(dt)
dt <- tibble(a = 1:3, b = a, c = 3:1, d = c(1, 2, NA), e = 4:6, f = e)
equal_elements(dt, F)
equal_col_sets4(dt)
equal_col_sets3(dt)
equal_col_sets4(c(a = 1, b = 2, c = 1, d = 2, e = 5))
(1:3)[c(F, F, F)]

# TODO: decide on returning names or indices
x <- list(a = 1, 2, 3, d = 4)
names(x)
?all.equal.list
x <- c(1:2, NA)
y <- 1:3
vctrs::vec_equal(x, x, na_equal = T)
vctrs::vec_equal(x, y, na_equal = F)
vctrs::vec_equal(1, 1:3)

equal_elements(dt)
equal_elements(dt, F)
equal_elements(list(a = list(1, 2), b = 1:3))
list(1, 2) == 1:3
equal_elements2(dt, T)
equal_elements2(dt, F)
# I will need a different method for lists to dfs. Comparison of elements is different from
# what I intend for dfs. For now just write for dfs.
dt
equal_elements2 <- function(x, na.rm = TRUE, use_names = TRUE) {
  done <- c()
  n <- length(x)
  sets <- vector("list", n)
  for (i in 1:n){
    if (i %in% done) next
    sets[[i]] <- (1:n)[map_lgl(x, ~all(vctrs::vec_equal(., x[[i]], na_equal = !na.rm), na.rm = na.rm))]
    done <- c(done, sets[[i]])
  }
  sets <- purrr::compact(sets)
  if (use_names && !is.null(names(x))){
    return(map(sets, ~names(x)[.]))
  }
  sets
}

