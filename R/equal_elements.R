#' Get sets of equal elements
#'
#' Divides `x` into subsets based on equality between elements. Equality between pairs of elements
#' is determined by `all(a == b, na.rm = TRUE)`.
#'
#' @param x A data frame
#' @param use_names Logical. If `TRUE` and `x` has names then returned sets will be names from `x`,
#' otherwise indices.
#' @export
equal_elements <- function(x, use_names = TRUE) {
  stopifnot(is.data.frame(x))
  done <- c()
  n <- length(x)
  sets <- vector("list", n)
  for (i in 1:n){
    if (i %in% done) next
    sets[[i]] <- (1:n)[map_lgl(x, ~all(. == x[[i]], na.rm = TRUE))]
    done <- c(done, sets[[i]])
  }
  sets <- purrr::compact(sets)
  if (use_names && !is.null(names(x))){
    return(map(sets, ~names(x)[.]))
  }
  sets
}
