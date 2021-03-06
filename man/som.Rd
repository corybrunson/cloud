% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/som.R
\name{som}
\alias{som}
\title{Vector space endomorphism}
\usage{
som(cloud, point, dir_vec)
}
\arguments{
\item{cloud}{An m-by-n matrix of coordinates for m points in n-dimensional
space.}

\item{point}{An n-coordinate point in the same space as the cloud.}

\item{dir_vec}{An n-dimensional vector giving the direction of the axis
through the cloud.}
}
\description{
This function encodes the endomorphism $\mathrm{Som}$ on the vector space
containing a point cloud (with respect to a given point) obtained by
composing $\mathcal{Eff}$ with $\mathcal{Vac}$.
}

