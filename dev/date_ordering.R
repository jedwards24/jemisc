# Function to get date orderings
#Name: column_ordering()? order_cols()?
#  This could also be used to find orderings of columns based on any relation e.g. are missing
# subsets of another column’s missings?

#  Based on igraph but could be converted.
library(tidyverse)
#library(edwards)
library(lubridate)
library(igraph)


# Exxample data
dt <- tibble(a = rep(ymd("1900-01-01"), 5),
             b = a + 1,
             c = rep(ymd("2000-01-01"), 5),
             d = c,
             e = c,
             f = c + 1,
             g = a)

dt[1, 4:6] <- list(ymd("1899-01-01"), ymd("1999-01-01"), ymd("1899-01-01"))
dt

#dt <- tibble(a = rep(ymd("1900-01-01"), 5), b = a)
adj_mat <- date_ordering(dt)
pp <- adjacency_to_paths(adj_mat)
net <- igraph::graph_from_adjacency_matrix(adj_mat, diag = F)
plot(net)
paths <- igraph::all_simple_paths(net, "c", mode = "out")

ff <- function(x, y) {
  x == y
}
sweep(dt[, -1], 1, pull(dt, 1), "==") %>%
  apply(2, all)

# Input df is just datetimes/dates. Check funcs for this
# This returns an adjacency matrix of all date/datetime columns
# A TRUE in the matrix says the that i >= j in all data rows without NAs.
date_ordering <- function(df) {
  dates <- map_lgl(df, ~all(is.Date(.) | is.POSIXt(.))) %>%
    .[.] %>%
    names()
  df <- select(df, all_of(dates)) %>%
    mutate_all(as_date)
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


# Run code
adj_mat <- date_ordering(dt)
pp <- adjacency_to_paths(adj_mat)
pp
tibble(paths = pp) %>%
  rowwise() %>%
  mutate(len = length(paths)) %>%
  mutate(root = paths[1])


net <- graph_from_adjacency_matrix(mat, diag = F) # igraph graph from matrix
plot(net)

ego_size(net)
rownames(mat)
# get all paths from non-dominated column (no other column is >= to it)
# There may be multiple of these
paths <- all_simple_paths(net, "c", mode = "out")
paths[[1]] %>% str #named integer with graph attributes

edg <- as_data_frame(net, what="edges")


paths[[1]] %>% str()
list(1, "b") %>% .[c(T, F)]
reduce_simple_paths(paths)


as_edgelist(net, names=T)

as_adjacency_matrix(net)

edg <- as_data_frame(net, what="edges")
count(edg, from)
count(edg, to)
setdiff(names(dt3), edg$to)
setdiff(names(dt3), edg$from)

as_data_frame(net, what="vertices")
