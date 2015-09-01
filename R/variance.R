#' Variance of a point cloud.
#' 
#' This function calculates the variance of a point cloud.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param weights An m-element vector of point masses (weights).
#' @param ... Additional arguments passed to \code{\link{inertia}}.
#' @export

variance <-
    function(cloud, weights, ...) {
        
        # If weights is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If weights is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Calculate the barycenter
        bc <- barycenter(cloud = cloud, weights = weights)
        
        # Calculate the inertia with respect to the barycenter
        inertia(point = bc, cloud = cloud, weights = weights, ...)
    }
