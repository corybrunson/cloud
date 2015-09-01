#' Orthogonal projection to a linear subspace.
#' 
#' This function calculates the orthogonal projection of a point (cloud) in a 
#' Euclidean space to a linear subspace.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional space.
#' @param subspace An k-by-n matrix of coordinates spanning the k-dimensional linear
#'   subspace or (k - 1)-dimensional affine subspace.
#' @param type Whether to interpret h as an affine or a linear subspace.
#'   Defaults to "affine".
#' @export

projection <-
    function(cloud, subspace, type = "affine", ...) {
        
        if(type == "affine") {
            affine.projection(cloud, subspace)
        } else if(type == "linear") {
            linear.projection(cloud, subspace)
        } else {
            stop("Only linear and affine types recognized.")
        }
    }
