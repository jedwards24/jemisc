#' Get sets of column orderings
#'
#' Column A is ordered ahead of column B if `A[i]>=B[i]` for all rows i in which neither A or B has
#' missing values. A column ordering is a sequence of column names which are in order e.g. A>=B>=C.
#' This function returns a list of all _possible_ orderings which are not a subset of another
#' ordering. The orderings are possible not definite where there are `NA`s (treated as missing values).
#'
#' @return A list of vectors of column names. Each name vector gives an ordering of columns
#' largest to smallest.
#' @param df A data frame with only atomic columns (no list or data frame columns).
#' @export
column_orderings <- function(df) {
  if (!all(purrr::map_lgl(df, is.atomic))){
    stop("All column of `df` must be atomic.", call. = FALSE)
  }
  check_package("igraph")
  column_orderings_matrix(df) %>%
    adjacency_to_paths()
}

#' Adjacency matrix recording orderings between all pairs of columns in data frame `df`.
#' A TRUE at (i,j) in the returned matrix values in column are >= to corresponding
#' values in column j (ignoring rows where either column has an `NA`).
#' @param df A data frame.
#' @keywords internal
column_orderings_matrix <- function(df) {
  n <- ncol(df)
  dom_mat <- matrix(NA, n, n)
  for (i in 1 : n){
    for (j in 1 : n){
      dom_mat[i, j] <- all(df[, i, drop = TRUE] >= df[, j, drop = TRUE], na.rm = TRUE)
    }
  }
  colnames(dom_mat) <- names(df)
  rownames(dom_mat) <- names(df)
  dom_mat
}

#'
#' Given adjacency matrix input,
#' returns a list of simple paths that are not a strict subset of another simple path.
#' @param adj_mat Adjacency matrix output from `column_ordering_matrix()`.
#' @keywords internal
adjacency_to_paths <- function(adj_mat) {
  check_package("igraph")
  roots <- roots(adj_mat)
  all_paths <- vector("list", length(roots))
  net <- igraph::graph_from_adjacency_matrix(adj_mat, diag = F)
  for(i in seq_along(roots)){
    paths <- igraph::all_simple_paths(net, roots[i], mode = "out")
    if (length(paths) == 0) paths <- list(roots[i])
    all_paths[[i]] <- reduce_simple_paths(paths)
  }
  flat <- unlist(all_paths, recursive = FALSE)
  for (i in seq_along(flat)){
    if (is.integer(flat[[i]])){
      flat[[i]] <- names(flat[[i]])
      attributes(flat[[i]]) <- NULL
    }
  }
  flat
}

#' Get all vertices with indegree zero from an adjacency matrix.
#' Used in `adjacency_to_paths()`.
#' @noRd
roots <- function(adj_mat) {
  sums <- colSums(adj_mat)
  names(sums[sums == 1])
}

#' Remove paths that are subset of other paths
#'
#' Used in `adjacency_to_paths()`.
#'
#' @param paths List of paths output from `igraph::all_simple_paths()`.
#' @noRd
reduce_simple_paths<- function(paths) {
  n <- length(paths)
  keep <- 1:n
  for (i in seq_along(paths)){
    len <- length(paths[[i]])
    for(j in seq_along(paths)[keep]){
      if (len < length(paths[[j]]) && length(setdiff(paths[[i]], paths[[j]])) == 0){
        keep <- keep[which(keep != i)]
        break()
      }
    }
  }
  paths[keep]
}
