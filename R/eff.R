#' Effect linear mapping
#' 
#' This function encodes the effect linear mapping from the space of measures 
#' over a cloud index set $J$ to the vector space containing the cloud $M^J$, 
#' relative to a given point $P$.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional 
#'   space.
#' @param point An n-coordinate point in the same space as the cloud.
#' @param coord_vec An m-dimensional vector giving the coordinates in the space
#'   of measures.
#' @export

eff <-
    function(cloud, point, coord_vec) {
        
        # If point is unspecified, use barycenter
        if (missing(point)) point <- barycenter(cloud)
        
        # Transform cloud relative to point
        transform_cloud <- cloud - rep(point, each = nrow(cloud))
        
        # Calculate coordinates
        t(coord_vec) %*% transform_cloud
    }
