#' Barycenter of a point cloud.
#' 
#' This function calculates the barycenter (weighted centroid) of a point cloud.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param weights An m-element vector of point masses (weights).
#' @export

barycenter <-
    function(cloud, weights) {
        
        # If w is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If w is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Calculate barycenter
        colSums(cloud * weights) / sum(weights)
    }
