# Function to get date orderings
#Name: column_ordering()? order_cols()?
#  This could also be used to find orderings of columns based on any relation e.g. are missing
# subsets of another column’s missings?

#  Based on igraph but could be converted.
library(tidyverse)
#library(edwards)
library(lubridate)
library(igraph)
library(profvis)

# Example data
dt <- tibble(a = rep(ymd("1900-01-01"), 5),
             b = a + 1,
             c = rep(ymd("2000-01-01"), 5),
             d = c,
             e = c,
             f = c + 1,
             g = a)

dt[1, 4:6] <- list(ymd("1899-01-01"), ymd("1999-01-01"), ymd("1899-01-01"))
dt

# Run code
column_orderings(dt)

adj_mat <- date_ordering(dt)
pp <- adjacency_to_paths(adj_mat)
pp
tibble(paths = pp) %>%
  rowwise() %>%
  mutate(len = length(paths)) %>%
  mutate(root = paths[1])

dt <- tibble(a = 1, b = 2, c = 2, d = NA, e = 3, f = 3, g = 1)
mat <- column_orderings_matrix(dt)
net <- graph_from_adjacency_matrix(mat, diag = F) # igraph graph from matrix
paths <- all_simple_paths(net, "c", mode = "out")
adjacency_to_paths(mat)
plot(net)

ego_size(net)
rownames(mat)
edg <- as_data_frame(net, what="edges")

as_edgelist(net, names=T)

as_adjacency_matrix(net)

edg <- as_data_frame(net, what="edges")
count(edg, from)
count(edg, to)
as_data_frame(net, what="vertices")
