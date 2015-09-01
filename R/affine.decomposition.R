#' Affine subspace characterization by linear subspace and orthogonal vector.
#' 
#' Characterize an affine subspace by the parallel linear subspace and the 
#' orthogonal vector from the origin.
#' @param subspace An k-by-n matrix of coordinates spanning the (k -
#'   1)-dimensional affine subspace.
#' @export

affine.decomposition <-
    function(subspace) {
        
        # Calculate a basis for the parallel linear subspace
        lin.subspace <- subspace[-1, , drop = FALSE] -
            rep(subspace[1, , drop = TRUE], each = nrow(subspace) - 1)
        
        # Calculate the projection of the first point to hh
        proj <- linear.projection(subspace[1, , drop = TRUE], lin.subspace)
        
        # Calculate the orthogonal vector from the origin to h
        vec <- subspace[1, , drop = TRUE] - proj
        
        # Return the parallel linear subspace and the orthogonal vector
        list(linear.subspace = lin.subspace, orthogonal.vector = vec)
    }
