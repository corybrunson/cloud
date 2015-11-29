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
#' @param ... Additional arguments passed to \code{\link{cloud_decomposition}}.
#' @export

cloud_variance_test <-
    function(cloud, weights, subspace, ...) {
        
        # If weights is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If weights is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Decompose x into its fitted and residual clouds.
        decomp <- cloud_decomposition(cloud = cloud, subspace = subspace,
                                      resid = "cloud", ...)
        
        # Left side
        lhs <- variance(cloud = cloud, weights = weights)
        
        # Right side
        rhs <- variance(cloud = decomp$fitted_cloud, weights = weights) +
            variance(cloud = decomp$residual_cloud, weights = weights)
        
        # Equal?
        print(all.equal(lhs, rhs))
        
        list(lhs = lhs, rhs = rhs)
    }
