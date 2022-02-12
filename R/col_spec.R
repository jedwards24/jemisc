#' Get column types for use as `col_types` in `readr::read_csv()`
#'
#' @description
#' Returns the compact string representation for the `col_types` argument in
#' `readr::read_csv()` and related functions of the supplied `data`. The intended
#' use is to record the types after an initial load (with any further processing), so it
#' can be used for future reads of the same data.
#'
#' @details
#' The column types will be one of:
#'
#' - c = character
#' - i = integer
#' - d = double
#' - l = logical
#' - f = factor
#' - D = date
#' - T = date time
#' - t = time
#'
#' These types are read directly from `data` - there is no guessing or coercion.
#' The first four are identified using `typeof()`. The remainder overwrite the type if
#' one of the following is true for the column: `is.factor()`, `lubridate::is.Date()`,
#' `lubridate::is.POSIXt()`, or `"hms" %in% class(.)`.
#'
#' @param data A data frame.
#' @export
col_spec <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  types <- purrr::map_chr(data, typeof) %>%
    purrr::map_chr(~stringr::str_sub(., 1, 1)) %>%
    unname()
  date_cols <- purrr::map_lgl(data, lubridate::is.Date)
  dtime_cols <- purrr::map_lgl(data, lubridate::is.POSIXt)
  time_cols <- purrr::map_lgl(data, ~"hms" %in% class(.))
  types[date_cols] <- "D"
  types[dtime_cols] <- "T"
  types[time_cols] <- "t"
  types[purrr::map_lgl(data, is.factor)] <- "f"
  paste(types, collapse = "")
}
