#' Affine subspace generation from a point and a linear subspace
#' 
#' This function returns the affine subspace through an inputed point and parallel (and equidimensional to) an inputed linear subspace.
#' @param point The n coordinates of a reference point.
#' @param subspace An k-by-n matrix (or n-coordinate vector when k equals 1) of coordinates spanning the k-dimensional linear subspace.
#' @export

affine.recomposition <-
    function(point, subspace) {
        
        rbind(0, subspace) + rep(point, each = nrow(subspace) + 1)
    }
