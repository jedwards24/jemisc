#' Test several objects are all identical
#'
#' @return Logical. `TRUE` if and only if all objects are `identical()`.
#' @param ... Objects to compare.
#' @export
# A loop is a bit faster than vapply even without early stopping.
all_identical <- function(...) {
  test <- list(...)[-1]
  x <- list(...)[[1]]
  for (i in seq_along(test)){
    if (!identical(test[[i]], x)) return(FALSE)
  }
  TRUE
}
