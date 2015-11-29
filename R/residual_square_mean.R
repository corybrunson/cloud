#' The residual square mean of a subspace.
#' 
#' This function calculates the residual square mean of a point (cloud) with
#' respect to a subspace, i.e. the weighted mean of the squares of the distances
#' from the point(s) to the subspace.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param subspace An k-by-n matrix of coordinates spanning the h-dimensional subspace.
#' @param weights An m-element vector of point masses (weights).
#' @param ... Additional arguments passed to \code{\link{projection}}.
#' @export

residual_square_mean <-
    function(cloud, weights, subspace, ...) {
        
        # If cloud is a vector...
        if(is.null(dim(cloud))) {
            vectorInput <- TRUE
            cloud <- matrix(cloud, nrow = 1)
        } else {
            vectorInput <- FALSE
        }
        
        # If weights is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If weights is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Calculate projections
        proj <- projection(cloud = cloud, subspace = subspace, ...)
        
        # Calculate squared distances
        sq_dists <- distances(cloud, proj) ^ 2
        
        # Weighted mean of squared distances
        sum(weights * sq_dists) / sum(weights)
    }
