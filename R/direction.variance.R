#' Variance in the direction of a subspace.
#' 
#' This function calculates the variance of a point cloud in the direction of a 
#' subspace.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param weights An m-element vector of point masses (weights).
#' @param subspace An k-by-n matrix of coordinates spanning the k-dimensional subspace.
#' @param ... Additional arguments passed to \code{\link{projection}}.
#' @export

direction.variance <-
    function(cloud, weights, subspace, ...) {
        
        # If weights is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If weights is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Calculate projections
        proj <- projection(cloud = cloud, subspace = subspace, ...)
        
        # Calculate variance
        variance(cloud = proj, weights = weights)
    }
