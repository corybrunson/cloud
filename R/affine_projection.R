#' Orthogonal projection to an affine subspace.
#' 
#' This function calculates the orthogonal projection of a point (cloud) in a 
#' Euclidean space to a affine subspace.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param subspace An k-by-n matrix of coordinates spanning the (k - 1)-dimensional
#'   affine subspace.

affine_projection <-
    function(cloud, subspace) {
        
        # If cloud is a vector...
        if(is.null(dim(cloud))) {
            vectorInput <- TRUE
            cloud <- matrix(cloud, nrow = 1)
        } else {
            vectorInput <- FALSE
        }
        
        # Decompose subspace into a linear subspace and an orthogonal vector
        decomp <- affine_decomposition(subspace)
        
        # Calculate projections to the linear subspace
        lin_proj <- linear_projection(cloud, decomp$linear_subspace)
        
        # Translate proj along the orthogonal vector
        aff_proj <- lin_proj + rep(decomp$orthogonal_vector,
                                   each = nrow(lin_proj))
        
        # If cloud was a vector...
        if(vectorInput) aff_proj <- as.vector(aff_proj)
        
        aff_proj
    }
