% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/affine_projection.R
\name{affine_projection}
\alias{affine_projection}
\title{Orthogonal projection to an affine subspace.}
\usage{
affine_projection(cloud, subspace)
}
\arguments{
\item{cloud}{An m-by-n matrix of coordinates for m points in n-dimensional space.}

\item{subspace}{An k-by-n matrix of coordinates spanning the (k - 1)-dimensional
affine subspace.}
}
\description{
This function calculates the orthogonal projection of a point (cloud) in a
Euclidean space to a affine subspace.
}

