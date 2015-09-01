#' Inertia of a point cloud with respect to a reference point.
#' 
#' This function calculates the inertia of a point cloud with respect to a 
#' reference point.
#' @param point The n coordinates of a reference point.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param weights An m-element vector of point masses (weights).
#' @param pointwise Logical; whether to return pointwise (versus cloud)
#'   inertias. Defaults to \code{FALSE}.
#' @param relative Logical; whether to return relative (versus absolute)
#'   pointwise inertias. Defaults to \code{FALSE}.
#' @export

inertia <-
    function(point, cloud, weights, pointwise = FALSE, relative = FALSE) {
        
        # If weights is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If weights is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Pointwise inertias
        i <- weights * rowSums((cloud - rep(point, each = nrow(cloud))) ^ 2)
        
        # If pointwise...
        if(pointwise) {
            # If relative...
            if(relative) {
                i / sum(i)
            } else {
                i
            }
        } else {
            # Cloud inertia
            sum(i)
        }
    }
