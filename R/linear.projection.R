#' Orthogonal projection to a linear subspace.
#' 
#' This function calculates the orthogonal projection of a point (cloud) in a 
#' Euclidean space to a linear subspace.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param subspace An k-by-n matrix of coordinates spanning, together with the origin,
#'   the h-dimensional linear subspace.

linear.projection <-
    function(cloud, subspace) {
        
        # If cloud is a vector...
        if(is.null(dim(cloud))) {
            vectorInput <- TRUE
            cloud <- matrix(cloud, nrow = 1)
        } else {
            vectorInput <- FALSE
        }
        
        # Calculate projections
        proj <- (cloud %*% t(subspace)) %*%
            solve(subspace %*% t(subspace)) %*%
            subspace
        
        # If x was a vector...
        if(vectorInput) proj <- as.vector(proj)
        
        proj
    }
