#' Test decomposition of cloud variance.
#' 
#' This function verifies that the variance of a cloud is the sum of its 
#' variance in the direction of any subspace and the variance of the residual 
#' cloud with respect to that subspace.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional
#'   space.
#' @param weights An m-element vector of point masses (weights).
#' @param subspace An k-by-n matrix of coordinates spanning the k-dimensional
#'   linear subspace or (k - 1)-dimensional affine subspace.
#' @param type Whether to interpret h as an affine or a linear subspace. 
#'   Defaults to "affine".
#' @export

cloud.variance.test <-
    function(cloud, weights, subspace, type = "affine") {
        
        # If weights is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If weights is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Decompose x into its fitted and residual clouds.
        decomp <- cloud.decomposition(cloud = cloud, subspace = subspace,
                                      resid = "cloud", type = type)
        
        # Left side
        lhs <- variance(cloud = cloud, weights = weights)
        
        # Right side
        rhs <- variance(cloud = decomp$fitted.cloud, weights = weights) +
            variance(cloud = decomp$residual.cloud, weights = weights)
        
        # Equal?
        print(all.equal(lhs, rhs))
        
        list(lhs = lhs, rhs = rhs)
    }
