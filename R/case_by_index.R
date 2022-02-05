#' Choose values row-wise from a set of columns according to an index column
#'
#' Adds a new column to the input `data`. The new column takes values row-wise from a supplied
#' subset of columns in `data`. Which column's value is used for each row is determined by the
#' corresponding integer in the column in the data given by `index_column`.
#'
#' @param data A data frame.
#' @param index_column A column of integers indicating which column to select from for each row.
#' @param ... Columns to select values from. Passed to `dplyr::select` so can use tidy-select.
#' @param into String giving the name of the new column.
#' @return A data frame which matches the input `data` with a new column.
#' @examples
#' x <- tibble::tibble(a = 11:13, b = 21:23, index = c(1, 2, 1))
#' case_by_index(x, index, a, b, into = "new")
#' @export
case_by_index <- function(data, index_column, ..., into = "output") {
  input_data <- dplyr::select(data, ...)
  index <- dplyr::pull(dplyr::select(data, {{ index_column }}))

  out <- rep(NA, nrow(data))
  if (!rlang::is_integerish(index) || !all(stats::na.omit(index) >= 0.9)){
    stop("All values in `index_column` must be positive integers.", call. = FALSE)
  }
  max_index <- max(index, na.rm = TRUE)
  if (max_index > ncol(input_data)){
    stop("Index values must not be greater than the number of look-up columns", call. = FALSE)
  }
  for (i in 1 : max_index){
    query <- index == i
    out[query] <- input_data[[i]][query]
  }
  dplyr::mutate(data, {{ into }} := out)
}
