#' Vector space endomorphism
#' 
#' This function encodes the endomorphism $\mathrm{Som}$ on the vector space
#' containing a point cloud (with respect to a given point) obtained by
#' composing $\mathcal{Eff}$ with $\mathcal{Vac}$.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional 
#'   space.
#' @param point An n-coordinate point in the same space as the cloud.
#' @param dir_vec An n-dimensional vector giving the direction of the axis 
#'   through the cloud.
#' @export

som <-
    function(cloud, point, dir_vec) {
        
        # If point is unspecified, use barycenter
        if (missing(point)) point <- barycenter(cloud)
        
        # Transform cloud relative to point
        transform_cloud <- cloud - rep(point, each = nrow(cloud))
        
        # Perform Vac
        coord_vec <- transform_cloud %*% matrix(dir_vec, ncol = 1)
        
        # Perform Eff
        t(coord_vec) %*% transform_cloud
    }
