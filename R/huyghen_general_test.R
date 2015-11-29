#' Test general Huyghen's theorem
#' 
#' This function calculates both sides of the equation of Huyghen's theorem.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional
#'   space.
#' @param weights An m-element vector of point masses (weights).
#' @param subspace An k-by-n matrix of coordinates spanning the k-dimensional
#'   affine subspace.
#' @param ... Additional arguments (eventually) passed to \code{\link{projection}}.
#' @export

huyghen_general_test <-
    function(cloud, weights, subspace, ...) {
        
        # If weights is missing...
        if(missing(weights)) weights <- rep(1, nrow(cloud))
        # If weights is a scalar...
        if(length(weights) == 1) weights <- rep(weights, nrow(cloud))
        
        # Left side
        lhs <- residual_square_mean(cloud = cloud, weights = weights,
                                    subspace = subspace, ...)
        
        # Calculate the barycenter of cloud
        bc <- barycenter(cloud)
        
        # Decompose subspace
        decomp <- affine_decomposition(subspace)
        
        # Describe the affine subspace through bc
        parallel_subspace <- affine_recomposition(bc, decomp$linear_subspace)
        
        # Right side
        rhs <- residual_square_mean(cloud = cloud, weights = weights,
                                    subspace = parallel_subspace,
                                    type = "affine") +
            distance(bc, projection(bc, subspace, ...)) ^ 2
        
        # Equal?
        print(all.equal(lhs, rhs))
        
        list(lhs = lhs, rhs = rhs)
    }
