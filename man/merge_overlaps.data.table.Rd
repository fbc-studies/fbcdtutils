% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_overlaps.R
\name{merge_overlaps.data.table}
\alias{merge_overlaps.data.table}
\title{Merge overlapping intervals}
\usage{
merge_overlaps.data.table(.data, .start, .end, ..., .max_gap = 0,
  .by = key(.data))
}
\arguments{
\item{.by}{A character vector of variables to group intervals by.}
}
\description{
Merge overlapping intervals
}
