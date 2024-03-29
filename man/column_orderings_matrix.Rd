% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/column_orderings.R
\name{column_orderings_matrix}
\alias{column_orderings_matrix}
\title{Adjacency matrix recording orderings between all pairs of columns in data frame \code{df}.
A TRUE at (i,j) in the returned matrix values in column are >= to corresponding
values in column j (ignoring rows where either column has an \code{NA}).}
\usage{
column_orderings_matrix(df)
}
\arguments{
\item{df}{A data frame.}
}
\description{
Adjacency matrix recording orderings between all pairs of columns in data frame \code{df}.
A TRUE at (i,j) in the returned matrix values in column are >= to corresponding
values in column j (ignoring rows where either column has an \code{NA}).
}
\keyword{internal}
