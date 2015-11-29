#' Test barycenter preservation under projection
#' 
#' This function compares the barycenter of the projection of a point cloud to
#' the projection of the barycenter of the cloud.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param subspace An k-by-n matrix of coordinates spanning the k-dimensional linear 
#'   subspace or (k - 1)-dimensional affine subspace.
#' @param ... Additional arguments passed to \code{\link{projection}}.
#' @export

projection_test <-
    function(cloud, subspace, ...) {
        
        # Left side
        lhs <- barycenter(projection(cloud = cloud, subspace = subspace, ...))
        
        # Right side
        rhs <- projection(cloud = barycenter(cloud), subspace = subspace, ...)
        
        # Equal?
        print(all.equal(lhs, rhs))
        
        list(lhs = lhs, rhs = rhs)
    }
