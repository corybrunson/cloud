#' Fitted and residual clouds.
#' 
#' Given a subspace, decompose a point cloud into its fitted cloud and either
#' its residual deviations or its residual cloud.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param subspace An k-by-n matrix of coordinates spanning the k-dimensional linear 
#'   subspace or (k - 1)-dimensional affine subspace.
#' @param resid Whether to return the residuals as a cloud or as deviations.
#'   Defaults to "cloud".
#' @param ... Additional arguments passed to \code{\link{projection}}.
#' @export

cloud_decomposition <-
    function(cloud, subspace, resid = "cloud", ...) {
        
        # Calculate projection
        proj <- projection(cloud = cloud, subspace = subspace, ...)
        
        # Calculate residual deviations
        res_dev <- cloud - proj
        
        # Return projection and residuals
        if(resid == "deviations") {
            list(fitted_cloud = proj, residual_deviations = res_dev)
        } else if (resid == "cloud") {
            bc <- barycenter(cloud)
            list(fitted_cloud = proj,
                 residual_cloud = res_dev + rep(bc, each = nrow(res_dev)))
        } else {
            stop("Residuals can only be returned as deviations or as a cloud.")
        }
    }
