#' Get sets of equal elements
#'
#' Divides `x` into disjoint subsets based on equality between elements. Equality between
#' pairs of elements is determined by `all.equal()`.
#'
#' @return A list of either names or indices of `x` (see `keep_names`).
#' @param x A list or atomic vector.
#' @param keep_names Logical. If `TRUE` and `x` has names then returned sets will be names from `x`,
#' otherwise indices.
#' @export
equal_elements <- function(x, keep_names = TRUE) {
  if (!is.list(x) && !is.atomic(x)){
    stop("`x` must be atomic or a list.", call. = FALSE)
  }
  sets <- if (is.atomic(x)){
    equal_elements_atomic(x)
  }else{
    equal_elements_list(x)
  }
  if (keep_names && !is.null(names(x))){
    return(purrr::map(sets, ~names(x)[.]))
  }
  if (keep_names) warning("`x` has no names. Returning indices instead.", call. = FALSE)
  sets
}

#' `equal_elements()` helper for lists
#'
#' @param x A vector.
#' @noRd
equal_elements_list <- function(x) {
  done <- c()
  n <- length(x)
  sets <- vector("list", n)
  for (i in 1:n){
    if (i %in% done) next
    sets[[i]] <- (1:n)[purrr::map_lgl(x, ~isTRUE(all.equal(., x[[i]])))]
    done <- c(done, sets[[i]])
  }
  purrr::compact(sets)
}

#' `equal_elements()` helper for atomic vectors
#'
#' @param x A vector.
#' @noRd
equal_elements_atomic <- function(x) {
  elems <- unique(x)
  n <- length(elems)
  sets <- vector("list", n)
  for (i in seq_len(n)){
    sets[[i]] <- seq_along(x)[x == elems[i]]
  }
  sets
}
