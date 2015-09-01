#' Test first Huyghen's theorem
#' 
#' This function calculates both sides of the equation of Huyghen's theorem.
#' @param point The n coordinates of a reference point.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param weights An m-element vector of point masses (weights).
#' @export

huyghen.test <-
    function(point, cloud, weights) {
        
        # If weights is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If weights is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Left side
        lhs <- inertia(point, cloud, weights = 1 / nrow(cloud))
        
        # Right side
        rhs <- distance(point, barycenter(cloud)) ^ 2 +
            variance(cloud, weights = 1 / nrow(cloud))
        
        # Equal?
        print(all.equal(lhs, rhs))
        
        list(lhs = lhs, rhs = rhs)
    }
