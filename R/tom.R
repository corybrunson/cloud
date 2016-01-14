#' Measure space endomorphism
#' 
#' This function encodes the endomorphism $\mathcal{Tom}$ no the space of
#' measures over a cloud index set obtained by composing $\mathrm{Vac}$ with
#' $\mathrm{Eff}$.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional 
#'   space.
#' @param point An n-coordinate point in the same space as the cloud.
#' @param coord_vec An m-dimensional vector giving the coordinates in the space 
#'   of measures.
#' @export

tom <-
    function(cloud, point, coord_vec) {
        
        # If point is unspecified, use barycenter
        if (missing(point)) point <- barycenter(cloud)
        
        # Transform cloud relative to point
        transform_cloud <- cloud - rep(point, each = nrow(cloud))
        
        # Perform Eff
        dir_vec <- t(coord_vec) %*% transform_cloud
        
        # Perform Vac
        transform_cloud %*% matrix(dir_vec, ncol = 1)
    }
